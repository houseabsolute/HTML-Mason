# Copyright (c) 1998-2002 by Jonathan Swartz. All rights reserved.
# This program is free software; you can redistribute it and/or modify it
# under the same terms as Perl itself.

package HTML::Mason::Interp;

use strict;

use Config;
use File::Basename;
use File::Path;
use File::Spec;
use HTML::Mason;
use HTML::Mason::Escapes;
use HTML::Mason::Request;
use HTML::Mason::Resolver::File;
use HTML::Mason::Tools qw(make_fh read_file taint_is_on load_pkg);

use HTML::Mason::Exceptions( abbr => [qw(param_error system_error wrong_compiler_error compilation_error error)] );

use Params::Validate qw(:all);
Params::Validate::validation_options( on_fail => sub { param_error join '', @_  } );

use Class::Container;
use base qw(Class::Container);

BEGIN
{
    # Fields that can be set in new method, with defaults
    __PACKAGE__->valid_params
	(
	 autohandler_name =>
         { parse => 'string',  default => 'autohandler', type => SCALAR,
           descr => "The filename to use for Mason's 'autohandler' capability" },

	 code_cache_max_size =>
         { parse => 'string',  default => 10*1024*1024, type => SCALAR,  # 10M
           descr => "The maximum size of the component code cache, in bytes" },

	 compiler =>
         { isa => 'HTML::Mason::Compiler',
           descr => "A Compiler object for compiling components" },

	 current_time =>
         { parse => 'string', default => 'real', optional => 1,
           type => SCALAR, descr => "Current time (deprecated)" },

	 data_dir =>
         { parse => 'string', optional => 1, type => SCALAR,
           descr => "A directory for storing cache files and other state information" },

         escape_flags =>
         { parse => 'hash_list', optional => 1, type => HASHREF,
           descr => "A list of escape flags to set (as if calling the set_escape() method" },

	 static_source =>
         { parse => 'boolean', default => 0, type => BOOLEAN,
           descr => "When true, we only compile source files once" },

	 # OBJECT cause qr// returns an object
	 ignore_warnings_expr =>
         { parse => 'string',  type => SCALAR|OBJECT, default => qr/Subroutine .* redefined/i,
           descr => "A regular expression describing Perl warning messages to ignore" },

	 preloads =>
         { parse => 'list', optional => 1, type => ARRAYREF,
           descr => "A list of components to load immediately when creating the Interpreter" },

	 resolver =>
         { isa => 'HTML::Mason::Resolver',
           descr => "A Resolver object for fetching components from storage" },

	 use_object_files =>
         { parse => 'boolean', default => 1, type => BOOLEAN,
           descr => "Whether to cache component objects on disk" },
	);

    __PACKAGE__->contained_objects
	(
	 resolver => { class => 'HTML::Mason::Resolver::File',
		       descr => "This class is expected to return component information based on a component path" },
	 compiler => { class => 'HTML::Mason::Compiler::ToObject',
		       descr => "This class is used to translate component source into code" },
	 request  => { class => 'HTML::Mason::Request',
		       delayed => 1,
		       descr => "Objects returned by make_request are members of this class" },
	);
}

use HTML::Mason::MethodMaker
    ( read_only => [ qw( autohandler_name
                         code_cache
                         compiler
			 data_dir
			 preloads
                         static_source
                         resolver
                         source_cache
			 use_object_files
                        ) ],

      read_write => [ map { [ $_ => __PACKAGE__->validation_spec->{$_} ] }
		      qw( code_cache_max_size
			  ignore_warnings_expr
                         )
		    ],

      read_write_contained => { request =>
				[ [ autoflush => { type => BOOLEAN } ],
				  [ data_cache_api => { type => SCALAR } ],
				  [ data_cache_defaults => { type => HASHREF } ],
				  [ dhandler_name => { type => SCALAR } ],
				  [ error_format => { type => SCALAR } ],
				  [ error_mode => { type => SCALAR } ],
				  [ max_recurse => { type => SCALAR } ],
				  [ out_method => { type => SCALARREF | CODEREF } ],
				]
			      },
      );

BEGIN {
    #
    # We've found at least one generated component that for some
    # reason never returns from eval-ing its object code.  For
    # systems that provide alarm we can try to protect against
    # this.
    #
    # This appears to be a Perl bug, fixed in 5.7.3+.  We'll skip
    # this hack for versions where the bug is fixed.  To eliminate the
    # checking penalty, we make it a compile-time constant.
    #
    if ( $Config{d_alarm} && $] < 5.007003 ) {
	eval 'sub PERL_BUG_INFINITE_LOOP () { 1 }';
    } else {
	eval 'sub PERL_BUG_INFINITE_LOOP () { 0 }';
    }
}

sub new
{
    my $class = shift;
    my $self = $class->SUPER::new(@_);

    $self->_initialize;
    return $self;
}

sub _initialize
{
    my ($self) = shift;
    $self->{code_cache} = {};
    $self->{code_cache_current_size} = 0;
    $self->{files_written} = [];

    #
    # Check that data_dir is absolute.
    #
    if ($self->{data_dir}) {
        $self->{data_dir} = File::Spec->canonpath( $self->{data_dir} );
        param_error "data_dir '$self->{data_dir}' must be an absolute directory"
            unless File::Spec->file_name_is_absolute( $self->{data_dir} );
    }

    #
    # Create data subdirectories if necessary. mkpath will die on error.
    #
    if ($self->data_dir) {
	foreach my $subdir ( qw(obj cache) ) {
	    my @newdirs = mkpath( File::Spec->catdir( $self->data_dir, $subdir ) , 0, 0775 );
	    $self->push_files_written(@newdirs);
	}
    } else {
	$self->{use_object_files} = 0;
    }

    #
    # Preloads
    #
    if ($self->preloads) {
	foreach my $pattern (@{$self->preloads}) {
	    error "preload pattern '$pattern' must be an absolute path"
		unless File::Spec->file_name_is_absolute($pattern);
	    my @paths = $self->resolver->glob_path($pattern)
		or warn "Didn't find any components for preload pattern '$pattern'";
	    foreach (@paths)
            {
                $self->load($_)
                    or error "Cannot preload component $_, found via pattern $pattern";
            }
	}
    }

    #
    # Add the escape flags (including defaults)
    #
    foreach ( [ h => \&HTML::Mason::Escapes::html_entities_escape ],
              [ u => \&HTML::Mason::Escapes::url_escape ],
            )
    {
        $self->set_escape(@$_);
    }

    if ( my $e = delete $self->{escape_flags} )
    {
        while ( my ($flag, $code) = each %$e )
        {
            $self->set_escape( $flag => $code );
        }
    }
}

#
# Shorthand for various data subdirectories and files.
#
sub object_dir { my $self = shift; return $self->data_dir ? File::Spec->catdir( $self->data_dir, 'obj' ) : ''; }
sub cache_dir  { my $self = shift; return $self->data_dir ? File::Spec->catdir( $self->data_dir, 'cache' ) : ''; }

#
# exec is the initial entry point for executing a component
# in a new request.
#
sub exec {
    my $self = shift;
    my $comp = shift;
    $self->make_request(comp=>$comp, args=>\@_)->exec;
}

sub make_request {
    my $self = shift;

    return $self->create_delayed_object( 'request', interp => $self, @_ );
}

sub comp_exists {
    my ($self, $path) = @_;
    return $self->resolver->get_info($path);
}

#
# Load <$path> into a component, possibly parsing the source and/or
# caching the code. Returns a component object or undef if the
# component was not found.
#
sub load {
    my ($self, $path) = @_;
    my ($maxfilemod, $objfile, $objfilemod);
    my $code_cache = $self->code_cache;
    my $resolver = $self->{resolver};

    #
    # Path must be absolute.
    #
    unless (substr($path, 0, 1) eq '/') {
	error "Component path given to Interp->load must be absolute (was given $path)";
    }

    #
    # Get source info from resolver. Cache the results in static_source mode.
    #
    my $source;
    if ($self->static_source) {
	unless (exists($self->{source_cache}{$path})) {
	    $self->{source_cache}{$path} = $resolver->get_info($path);
	}
	$source = $self->{source_cache}{$path};
    } else {
	$source = $resolver->get_info($path);
    }

    # No component matches this path.
    return unless defined $source;

    # comp_id is the unique name for the component, used for cache key
    # and object file name.
    my $comp_id = $source->comp_id;

    #
    # Get last modified time of source.
    #
    my $srcmod = $source->last_modified;

    #
    # If code cache contains an up to date entry for this path, use
    # the cached comp.  Always use the cached comp in static_source
    # mode.
    #
    if ( exists $code_cache->{$comp_id} &&
         ( $self->static_source || $code_cache->{$comp_id}->{lastmod} >= $srcmod )
       ) {
        return $code_cache->{$comp_id}->{comp};
    }

    if ($self->{use_object_files}) {
	$objfile = $self->comp_id_to_objfile($comp_id);

	my @stat = stat $objfile;
	if ( @stat && ! -f _ ) {
	    error "The object file '$objfile' exists but it is not a file!";
	}

	if ($self->static_source) {
	    # No entry in the code cache so if the object file exists,
	    # we will use it, otherwise we must create it.  These
	    # values make that happen.
	    $objfilemod = @stat ? $srcmod : 0;
	} else {
	    # If the object file exists, get its modification time.
	    # Otherwise (it doesn't exist or it is a directory) we
	    # must create it.
	    $objfilemod = @stat ? $stat[9] : 0;
	}
    }

    my $comp;
    if ($objfile) {
	#
	# We are using object files.  Update object file if necessary
	# and load component from there.
	#
	do
	{
	    if ($objfilemod < $srcmod) {
		$self->compiler->compile_to_file( file => $objfile, source => $source);
	    }
	    $comp = eval { $self->eval_object_code( object_file => $objfile ) };

	    if ($@) {
		if (isa_mason_exception($@, 'Compilation::IncompatibleCompiler')) {
		    $objfilemod = 0;
		} else {
		    $self->_compilation_error( $source->friendly_name, $@ );
		}
	    }
	} until ($comp);
    } else {
	#
	# Not using object files. Load component directly into memory.
	#
	my $object_code = $source->object_code( compiler => $self->compiler );
	$comp = eval { $self->eval_object_code( object_code => $object_code ) };
	$self->_compilation_error( $source->friendly_name, $@ ) if $@;
    }
    $comp->assign_runtime_properties($self, $source);

    #
    # Delete any stale cached version of this component, then
    # cache it if it's small enough.
    #
    $self->delete_from_code_cache($comp_id);

    if ($comp->object_size <= $self->code_cache_max_elem) {
	$code_cache->{$comp_id} = { lastmod => $srcmod, comp => $comp };
	$self->{code_cache_current_size} += $comp->object_size;
    }
    return $comp;
}

sub delete_from_code_cache {
    my ($self, $comp) = @_;
    return unless exists $self->{code_cache}{$comp};

    $self->{code_cache_current_size} -= $self->{code_cache}{$comp}{comp}->object_size;
    delete $self->{code_cache}{$comp};
    return;
}


sub comp_id_to_objfile {
    my ($self, $comp_id) = @_;

    return File::Spec->catfile( $self->object_dir, split /\//, $comp_id );
}

# User method for emptying code cache - useful for preventing memory leak
sub flush_code_cache {
    my $self = shift;

    $self->{code_cache} = {};
    $self->{code_cache_current_size} = 0;
}

#
# If code cache has exceeded maximum, remove least frequently used
# elements from cache until size falls below minimum.
#
sub purge_code_cache {
    my ($self) = @_;

    if ($self->{code_cache_current_size} > $self->code_cache_max_size) {
	my $code_cache = $self->{code_cache};
	my $min_size = $self->code_cache_min_size;
	my $decay_factor = $self->code_cache_decay_factor;

	my @elems;
	while (my ($path,$href) = each(%{$code_cache})) {
	    push(@elems,[$path,$href->{comp}->mfu_count,$href->{comp}]);
	}
	@elems = sort { $a->[1] <=> $b->[1] } @elems;
	while (($self->{code_cache_current_size} > $min_size) and @elems) {
	    $self->delete_from_code_cache(shift(@elems)->[0]);
	}

	#
	# Multiply each remaining cache item's count by a decay factor,
	# to gradually reduce impact of old information.
	#
	foreach my $elem (@elems) {
	    $elem->[2]->mfu_count( $elem->[2]->mfu_count * $decay_factor );
	}
    }
}

#
# Construct a component on the fly.  Virtual if 'path' parameter is
# given, otherwise anonymous.
#
sub make_component {
    my $self = shift;

    my %p = validate(@_, { comp_source => { type => SCALAR, optional => 1 },
			   comp_file   => { type => SCALAR, optional => 1 },
			   name        => { type => SCALAR, optional => 1 } });

    $p{comp_source} = read_file(delete $p{comp_file}) if exists $p{comp_file};
    param_error "Must specify either 'comp_source' or 'comp_file' parameter to 'make_component()'"
	unless defined $p{comp_source};

    $p{name} ||= '<anonymous component>';

    my $source = HTML::Mason::ComponentSource->new( friendly_name => $p{name},
						    comp_path => $p{name},
						    comp_id => undef,
						    last_modified => time,
						    comp_class => 'HTML::Mason::Component',
						    source_callback => sub { $p{comp_source} },
						  );

    my $object_code = $source->object_code( compiler => $self->compiler);

    my $comp = eval { $self->eval_object_code( object_code => $object_code ) };
    $self->_compilation_error( $p{name}, $@ ) if $@;

    $comp->assign_runtime_properties($self, $source);

    return $comp;
}

sub set_global
{
    my ($self, $decl, @values) = @_;
    param_error "Interp->set_global: expects a variable name and one or more values"
	unless @values;
    my ($prefix, $name) = ($decl =~ s/^([\$@%])//) ? ($1, $decl) : ('$', $decl);

    my $varname = sprintf("%s::%s",$self->compiler->in_package,$name);
    no strict 'refs';
    if ($prefix eq '$') {
	$$varname = $values[0];
    } elsif ($prefix eq '@') {
	@$varname = @values;
    } else {
	%$varname = @values;
    }
}

sub comp_root { shift->resolver->comp_root(@_) }

sub files_written
{
    my $self = shift;
    return @{$self->{files_written}};
}

#
# Push onto list of written files.
#
sub push_files_written
{
    my $self = shift;
    my $fref = $self->{'files_written'};
    push(@$fref,@_);
}

#
# Look for component <$name> starting in <$startpath> and moving upwards
# to the root. Return component object or undef.
#
sub find_comp_upwards
{
    my ($self, $startpath, $name) = @_;
    $startpath =~ s{/+$}{};

    # Don't use File::Spec here, this is a URL path.
    do {
      my $comp = $self->load("$startpath/$name");
      return $comp if $comp;
    } while $startpath =~ s{/+[^/]*$}{};

    return;  # Nothing found
}

# Code cache parameter methods

sub code_cache_min_size { shift->code_cache_max_size * 0.75 }
sub code_cache_max_elem { shift->code_cache_max_size * 0.20 }
sub code_cache_decay_factor { 0.75 }


###################################################################
# The eval_object_code & write_object_file methods used to be in
# Parser.pm.  This is a temporary home only.  They need to be moved
# again at some point in the future (during some sort of interp
# re-architecting).
###################################################################

#
# eval_object_code
#   (object_code, object_file, error)
# Evaluate an object file or object text.  Return a component object
# or undef if error.
#
# I think this belongs in the resolver (or comp loader) - Dave
#
sub eval_object_code
{
    my ($self, %p) = @_;

    $self->compiler->assert_creatorship(\%p);

    #
    # Evaluate object file or text with warnings on, unless
    # ignore_warnings_expr is '.'.
    #
    my $ignore_expr = $self->ignore_warnings_expr;
    my ($comp, $err);
    my $warnstr = '';

    {
	local $^W = 1 unless $ignore_expr eq '.';
	local $SIG{__WARN__} =
	    ( $ignore_expr ?
	      sub { $warnstr .= $_[0] if $_[0] !~ /$ignore_expr/ } :
	      sub { $warnstr .= $_[0] } ) unless $ignore_expr eq '.';
	
	local $SIG{ALRM} = sub { die $warnstr } if PERL_BUG_INFINITE_LOOP;
	alarm 5 if PERL_BUG_INFINITE_LOOP;
	
	$comp = $self->_do_or_eval(\%p);
	
	alarm 0 if PERL_BUG_INFINITE_LOOP;
    }

    $err = $warnstr . $@;

    unless ($err) {
	# Yes, I know I always freak out when people start poking
	# around in object internals but since there is no longer a
	# parser_version method in Component.pm there is no other way.
	# Only pre-1.10 components have parser_version set.
	wrong_compiler_error 'This object file was created by a pre-1.10 parser.  Please remove the component files in your object directory.'
	    if ref $comp && exists $comp->{parser_version};

	wrong_compiler_error 'This object file was created by an incompatible Compiler or Lexer.  Please remove the component files in your object directory.'
	    if UNIVERSAL::can( $comp, 'compiler_id' ) && $comp->compiler_id ne $self->compiler->object_id;
    }

    #
    # Return component or error
    #
    if ($err) {
	# attempt to stem very long eval errors
	$err =~ s/has too many errors\..+/has too many errors./s;
	compilation_error $err;
    } else {
	return $comp;
    }
}

sub _do_or_eval
{
    my ($self, $p) = @_;

    if ($p->{object_file}) {
	return do $p->{object_file};
    } else {
	# If in taint mode, untaint the object text
	(${$p->{object_code}}) = ${$p->{object_code}} =~ /^(.*)/s if taint_is_on;

	return eval ${$p->{object_code}};
    }
}

sub _compilation_error {
    my ($self, $filename, $err) = @_;

    HTML::Mason::Exception::Compilation->throw(error=>$err, filename=>$filename);
}


sub object_file {
    my ($self, $comp) = @_;
    return $comp->persistent ?
	$self->comp_id_to_objfile($comp->comp_id) :
	undef;
}

# Generate HTML that describes Interp's current status.
# This is used in things like Apache::Status reports.  Currently shows:
# -- Interp properties
# -- loaded (cached) components
#
# Note that Apache::Status has an extremely narrow URL API, and I
# think the only way to pass info to another request is through
# PATH_INFO.  That's why the expiration stuff is awkward.
sub status_as_html {
    my ($self, %p) = @_;

    # Should I be scared about this?  =)

    my $comp_source = <<'EOF';
<h3>Interpreter properties:</h3>
<blockquote>
 <h4>Startup options:</h4>
 <tt>
<table width="100%">
<%perl>
foreach my $property (sort keys %$interp) {
    my $val = $interp->{$property};

    my $default = ( defined $val && defined $valid{$property}{default} && $val eq $valid{$property}{default} ) || ( ! defined $val && exists $valid{$property}{default} && ! defined $valid{$property}{default} );

    my $display = $val;
    if (ref $val) {
        $display = '<font color="darkred">';
        # only object can ->can, others die
        my $is_object = eval { $val->can('anything'); 1 };
        if ($is_object) {
            $display .= ref $val . ' object';
        } else {
            if (UNIVERSAL::isa($val, 'ARRAY')) {
                $display .= 'ARRAY reference - [ ';
                $display .= join ', ', @$val;
                $display .= '] ';
            } elsif (UNIVERSAL::isa($val, 'HASH')) {
                $display .= 'HASH reference - { ';
                my @pairs;
                while (my ($k, $v) = each %$val) {
                   push @pairs, "$k => $v";
                }
                $display .= join ', ', @pairs;
                $display .= ' }';
            } else {
                $display = ref $val . ' reference';
            }
        }
        $display .= '</font>';
    }

    defined $display && $display =~ s,([\x00-\x1F]),'<font color="purple">control-' . chr( ord('A') + ord($1) - 1 ) . '</font>',eg; # does this work for non-ASCII?
</%perl>
 <tr valign="top" cellspacing="10">
  <td>
    <% $property | h %>
  </td>
  <td>
   <% defined $display ? $display : '<i>undef</i>' %>
   <% $default ? '<font color=green>(default)</font>' : '' %>
  </td>
 </tr>
% }
</table>
  </tt>

 <h4>Components in memory cache:</h4>
 <tt>
% my $cache;
% if ($cache = $interp->code_cache and %$cache) {
%   foreach my $key (sort keys %$cache) {
      <% $key |h%> (modified <% scalar localtime $cache->{$key}->{lastmod} %>)
      <br>
%   }
% } else {
    <I>None</I>
% }
  </tt>
</blockquote>

<%args>
 $interp   # The interpreter we'll elucidate
 %valid    # Default values for interp member data
</%args>
EOF

    my $comp = $self->make_component(comp_source => $comp_source);
    my $out;

    my $args = [interp => $self, valid => $self->validation_spec];
    $self->make_request(comp=>$comp, args=>$args, out_method=>\$out, %p)->exec;

    return $out;
}

sub set_escape
{
    my $self = shift;
    my %p = @_;

    while ( my ($name, $sub) = each %p )
    {
        my $flag_regex = $self->compiler->lexer->escape_flag_regex;

        param_error "Invalid escape name ($name)"
            if $name !~ /^$flag_regex$/ || $name =~ /^n$/;

        my $coderef;
        if ( ref $sub )
        {
            $coderef = $sub;
        }
        else
        {
            if ( $sub =~ /^\w+$/ )
            {
                no strict 'refs';
                unless ( defined &{"HTML::Mason::Escapes::$sub"} )
                {
                    param_error "Invalid escape: $sub (no matching subroutine in HTML::Mason::Escapes";
                }

                $coderef = \&{"HTML::Mason::Escapes::$sub"};
            }
            else
            {
                $coderef = eval $sub;
                param_error "Invalid escape: $sub ($@)" if $@;
            }
        }

        $self->{escapes}{$name} = $coderef;
    }
}

sub remove_escape
{
    my $self = shift;

    delete $self->{escapes}{ shift() };
}

sub apply_escapes
{
    my $self = shift;
    my $text = shift;

    foreach my $flag (@_)
    {
        param_error "Invalid escape flag: $flag"
            unless exists $self->{escapes}{$flag};

        $self->{escapes}{$flag}->(\$text);
    }

    return $text;
}

#
# Set or fetch the current time value (deprecated in 1.1x).
#
sub current_time {
    my $self = shift;
    if (@_) {
	my $newtime = shift;
	param_error "Interp::current_time: invalid value '$newtime' - must be 'real' or a numeric time value" if $newtime ne 'real' && $newtime !~ /^[0-9]+$/;
	return $self->{current_time} = $newtime;
    } else {
	return $self->{current_time};
    }
}

1;

__END__

=head1 NAME

HTML::Mason::Interp - Mason Component Interpreter

=head1 SYNOPSIS

    my $i = new HTML::Mason::Interp (data_dir=>'/usr/local/mason',
                                     comp_root=>'/usr/local/www/htdocs/',
                                     ...other params...);

=head1 DESCRIPTION

Interp is the Mason workhorse, executing components and routing their
output and errors to all the right places. In a mod_perl environment,
Interp objects are handed off immediately to an ApacheHandler object
which internally calls the Interp implementation methods. In that case
the only user method is the new() constructor.

=head1 PARAMETERS TO THE new() CONSTRUCTOR

=over

=item autohandler_name

File name used for L<autohandlers|HTML::Mason::Devel/autohandlers>. Default is "autohandler".

=item code_cache_max_size

Specifies the maximum size, in bytes, of the in-memory code cache
where components are stored. Default is 10 MB. See ADMIN<code cache>
for further details.

=item compiler

The Compiler object to associate with this Interpreter.  By default a
new object of class P<compiler_class> will be created.

=item compiler_class

The class to use when creating a compiler. Defaults to
L<HTML::Mason::Compiler|HTML::Mason::Compiler>.

=item current_time

Interpreter's notion of the current time (deprecated).

=item data_dir

The data directory is a writable directory that Mason uses for various
features and optimizations: for example, component object files and
data cache files. Mason will create the directory on startup, if necessary, and set its
permissions according to the web server User/Group.

Under L<Apache|HTML::Mason::ApacheHandler>, data_dir defaults to a
directory called "mason" under the Apache server root. You will
need to change this on certain systems that assign a high-level
server root such as F</usr>!

In non-Apache environments, data_dir has no default. If it is left
unspecified, Mason will not use L<object files|HTML::Mason::Admin/object files>, and the default
L<data cache class|HTML::Mason::Request/item_cache> will be
C<MemoryCache> instead of C<FileCache>.

=item escape_flags

A hash reference of escape flags to set for this object.  See the
section on the L<set_escape
method|HTML::Mason::Interp/item_set_escape> for more details.

=item ignore_warnings_expr

Regular expression indicating which warnings to ignore when loading
components. Any warning that is not ignored will prevent the
component from being loaded and executed. For example:

    ignore_warnings_expr =>
        'Global symbol.*requires explicit package'

If set to undef, all warnings are heeded. If set to '.', warnings
are turned off completely as a specially optimized case.

By default, this is set to 'Subroutine .* redefined'.  This allows you
to declare global subroutines inside <%once> sections and not receive
an error when the component is reloaded.

=item preloads

A list of component paths, optionally with glob wildcards, to load
when the interpreter initializes. e.g.

    preloads => ['/foo/index.html','/bar/*.pl']

Default is the empty list.  For maximum performance, this should only
be used for components that are frequently viewed and rarely updated.
See ADMIN<preloading components> for further details.

As mentioned in the developer's manual, a component's C<< <%once> >>
section is executed when it is loaded.  For preloaded components, this
means that this section will be executed before a Mason or Apache
request exist, so preloading a component that uses C<$m> or C<$r> in a
C<< <%once> >> section will fail.

=item request_class

The class to use when creating requests. Defaults to
L<HTML::Mason::Request|HTML::Mason::Request>.

=item resolver

The Resolver object to associate with this Compiler. By default a new
object of class P<resolver_class> will be created.

=item resolver_class

The class to use when creating a resolver. Defaults to
L<HTML::Mason::Resolver::File|HTML::Mason::Resolver::File>.

=item static_source

True or false, default is false. When false, Mason checks the
timestamp of the component source file each time the component is used
to see if it has changed. This provides the instant feedback for
source changes that is expected for development.  However it does
entail a file stat for each component executed.

When true, Mason assumes that the component source tree is unchanging:
it will not check component source files to determine if the memory
cache or object file has expired.  This can save many file stats per
request. However, in order to get Mason to recognize a component
source change, you must remove object files and restart the server (so
as to clear the memory cache).

Use this feature for live sites where performance is crucial and
where updates are infrequent and well-controlled.

=item use_object_files

True or false, default is true.  Specifies whether Mason creates
object files to save the results of component parsing. You may want to
turn off object files for disk space reasons, but otherwise this
should be left alone.

=back

=head1 ACCESSOR METHODS

All of the above properties have standard accessor methods of the same
name. In general, no arguments retrieves the value, and one argument
sets and returns the value.  For example:

    my $interp = new HTML::Mason::Interp (...);
    my $c = $interp->compiler;
    $interp->code_cache_max_size(20 * 1024 * 1024);

The following properties can be queried but not modified: data_dir,
preloads.

=head1 ESCAPE FLAG METHODS

=over

=for html <a name="item_apply_escapes"></a>

=item apply_escapes ($text, $flags, [more flags...])

This method applies a one or more escapes to a piece of text.  The
escapes are specified by giving their flag.  Each escape is applied to
the text in turn, after which the now-modified text is returned.

=for html <a name="item_remove_escape"></a>

=item remove_escape ($name)

Given an escape name, this removes that escape from the interpreter's
known escapes.  If the name is not recognized, it is simply ignored.

=for html <a name="item_set_escape"></a>

=item set_escape ($name => see below])

This method is called to add an escape flag to the list of known
escapes for the interpreter.  The flag may only consist of the
characters matching C<\w> and the dash (-).  It must start with an
alpha character or an underscore (_).

The right hand side may be one of several things.  It can be a
subroutine reference.  It can also be a string match C</^\w+$/>, in
which case it is assumed to be the name of a subroutine in the
C<HTML::Mason::Escapes> module.  Finally, if it is a string that does
not match the above regex, then it is assumed to be C<eval>able code,
which will return a subroutine reference.

When setting these with C<PerlSetVar> directives in an Apache
configuration file, you can set them like this:

  PerlSetVar  MasonEscapeFlags  "flag  => \&subroutine"
  PerlSetVar  MasonEscapeFlags  "uc    => sub { ${$_[0]} = uc ${$_[0]}; }"
  PerlAddVar  MasonEscapeFlags  "thing => other_thing"

=back

=head1 OTHER METHODS

=over

=for html <a name="item_comp_exists"></a>

=item comp_exists (path)

Given an I<absolute> component path, this method returns a boolean
value indicating whether or not a component exists for that path.

=for html <a name="item_comp_root"></a>

=item comp_root (comp_root)

This is a convenience method which simply calls the C<comp_root>
method in the resolver object.  Obviously, if you are using a custom
resolver class which does not have a C<comp_root> method, then this
convenience method will not work.

=for html <a name="item_exec"></a>

=item exec (comp, args...)

Creates a new HTML::Mason::Request object for the given I<comp> and
I<args>, and executes it. The return value is the return value of
I<comp>, if any.

This is useful for running Mason outside of a web environment.
See L<HTML::Mason::Admin/using Mason from a standalone script>
for examples.

This method isn't generally useful in a mod_perl environment; see
L<subrequests|HTML::Mason::Devel/Subrequests> instead.

=for html <a name="flush_code_cache"></a>

=item flush_code_cache

Empties the component cache. When using Perl 5.00503 or earlier, you
should call this when finished with an interpreter, in order to remove
circular references that would prevent the interpreter from being
destroyed.

=for html <a name="item_load"></a>

=item load (path)

Returns the component object corresponding to an absolute component
C<path>, or undef if none exists.

=for html <a name="item_make_component"></a>

=item make_component (comp_source => ... )

=item make_component (comp_file => ... )

This method compiles Mason component source code and returns a
Component object.  The source may be passed in as a string in C<comp_source>,
or as a filename in C<comp_file>.  When using C<comp_file>, the
filename is specified as a path on the file system, not as a path
relative to Mason's component root (see 
L<$m-E<gt>fetch_comp|HTML::Mason::Request/item_fetch_comp> for that).

If Mason encounters an error during processing, an exception will be thrown.

Example of usage:

    # Make an anonymous component
    my $anon_comp =
      eval { $interp->make_component
               ( comp_source => '<%perl>my $name = "World";</%perl>Hello <% $name %>!' ) };
    die $@ if $@;

    $m->comp($anon_comp);

=for html <a name="item_set_global"></a>

=item set_global ($varname, [values...])

This method sets a global to be used in components. C<varname> is a
variable name, optionally preceded with a prefix (C<$>, C<@>, or
C<%>); if the prefix is omitted then C<$> is assumed. C<varname> is
followed by a value, in the case of a scalar, or by one or more values
in the case of a list or hash.  For example:

    # Set a global variable $dbh containing the database handle
    $interp->set_global(dbh => DBI->connect(...));

    # Set a global hash %session from a local hash
    $interp->set_global('%session', %s);

The global is set in the package that components run in: usually
C<HTML::Mason::Commands>, although this can be overridden via the
P<in_package> parameter.
The lines above, for example, are equivalent to:

    $HTML::Mason::Commands::dbh = DBI->connect(...);
    %HTML::Mason::Commands::session = %s;

assuming that P<in_package> has not been changed.

Any global that you set should also be registered with the
P<allow_globals> parameter; otherwise you'll get warnings from
C<strict>.

=back

=head1 MEMORY LEAK WARNING

When using Perl 5.00503 or earlier, using the code cache creates a
circular reference between Interp and component objects.  This means
that Interp objects will not be destroyed unless you call
L<flush_code_cache|HTML::Mason::Interp/flush_code_cache>.  If you are
using Perl 5.6.0 or greater, and you have the XS version of
Scalar::Util installed, Mason uses weak references to prevent this
problem.

Win32 users should note that as of this writing, ActiveState's PPD for
Scalar-List-Utils only includes the pure Perl version of these
modules, which don't include the weak references functionality.

=head1 SEE ALSO

L<HTML::Mason|HTML::Mason>,
L<HTML::Mason::Admin|HTML::Mason::Admin>,
L<HTML::Mason::ApacheHandler|HTML::Mason::ApacheHandler>

=cut
