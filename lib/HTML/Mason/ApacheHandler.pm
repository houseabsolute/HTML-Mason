# Copyright (c) 1998-2002 by Jonathan Swartz. All rights reserved.
# This program is free software; you can redistribute it and/or modify it
# under the same terms as Perl itself.

use strict;

#----------------------------------------------------------------------
#
# APACHE-SPECIFIC REQUEST OBJECT
#
package HTML::Mason::Request::ApacheHandler;

use HTML::Mason::Request;
use HTML::Mason::Container;
use base qw(HTML::Mason::Request);

use HTML::Mason::Exceptions( abbr => [qw(param_error error)] );

use HTML::Mason::MethodMaker
    ( read_write => [ qw( ah apache_req ) ] );

__PACKAGE__->valid_params
    ( ah         => { isa => 'HTML::Mason::ApacheHandler',
		      descr => 'An ApacheHandler to handle web requests' },
      apache_req => { isa => 'Apache', default => undef,
		      descr => "An Apache request object" },
      cgi_object => { isa => 'CGI',    default => undef,
		      descr => "A CGI.pm request object" },
    );

sub new
{
    my $class = shift;
    my $self = $class->SUPER::new(@_);  # Magic!

    unless ($self->apache_req or $self->cgi_object)
    {
	param_error __PACKAGE__ . "->new: must specify 'apache_req' or 'cgi_object' parameter";
    }

    return $self;
}

# Override flush_buffer to also call $r->rflush
sub flush_buffer
{
    my ($self) = @_;
    $self->SUPER::flush_buffer;
    $self->apache_req->rflush;
}

sub cgi_object
{
    my ($self) = @_;

    error "Can't call cgi_object() unless 'args_method' is set to CGI.\n"
	unless $self->ah->args_method eq 'CGI';

    if (defined($_[1])) {
	$self->{cgi_object} = $_[1];
    } else {
	# We may not have created a CGI object if, say, request was a
	# GET with no query string. Create one on the fly if necessary.
	$self->{cgi_object} ||= CGI->new('');
    }

    return $self->{cgi_object};
}

#
# Override this method to always die when top level component is not found,
# so we can return NOT_FOUND.
#
sub _handle_error
{
    my ($self, $err) = @_;

    if (isa_mason_exception($err, 'TopLevelNotFound')) {
	die $err;
    } else {
	$self->SUPER::_handle_error($err);
    }
}

#----------------------------------------------------------------------
#
# APACHE-SPECIFIC FILE RESOLVER OBJECT
#
package HTML::Mason::Resolver::File::ApacheHandler;

use strict;

use HTML::Mason::Tools qw(paths_eq);

use HTML::Mason::Resolver::File;
use base qw(HTML::Mason::Resolver::File);

#
# Given an apache request object, return the associated component
# path or undef if none exists. This is called for top-level web
# requests that resolve to a particular file.
#
sub apache_request_to_comp_path {
    my ($self, $r) = @_;

    my $file = $r->filename;
    $file .= $r->path_info unless -f $file;

    foreach my $root (map $_->[1], $self->comp_root_array) {
	if (paths_eq($root, substr($file, 0, length($root)))) {
	    my $path = substr($file, ($root eq '/' ? 0 : length($root)));
	    $path =~ s,\/$,, unless $path eq '/';
	    return $path;
	}
    }
    return undef;
}


#----------------------------------------------------------------------
#
# APACHEHANDLER OBJECT
#
package HTML::Mason::ApacheHandler;

sub OK { return 0 }
sub DECLINED { return -1 }
sub NOT_FOUND { return 404 }
use File::Path;
use File::Spec;
use HTML::Mason::Exceptions( abbr => [qw(param_error system_error error)] );
use HTML::Mason::Interp;
use HTML::Mason::Utils;
use Params::Validate qw(:all);
Params::Validate::validation_options( on_fail => sub { param_error( join '', @_ ) } );

use Apache;
use Apache::Status;
die "mod_perl must be compiled with PERL_METHOD_HANDLERS=1 (or EVERYTHING=1) to use ", __PACKAGE__, "\n"
    unless Apache::perl_hook('MethodHandlers');

# Require a reasonably modern mod_perl - should probably be later
use mod_perl 1.22;

use HTML::Mason::MethodMaker
    ( read_write => [ qw( apache_status_title
                          args_method
			  auto_send_headers
			  decline_dirs
			  interp
			  top_level_predicate ) ]
    );

use vars qw($VERSION);

$VERSION = sprintf '%2d.%02d', q$Revision$ =~ /(\d+)\.(\d+)/;

use HTML::Mason::Container;
use base qw(HTML::Mason::Container);

__PACKAGE__->valid_params
    (
     apache_status_title   => { parse => 'string',  type => SCALAR,       default => 'HTML::Mason status',
			        descr => "The title of the Apache::Status page" },
     args_method           => { parse => 'string',  type => SCALAR,       default => 'mod_perl',
			        callbacks =>
				{ "must be either 'CGI' or 'mod_perl'" =>
				  sub { $_[0] =~ /^(?:CGI|mod_perl)$/ } },
				descr => "Whether to use CGI.pm or Apache::Request for parsing the incoming HTTP request",
                              },
     auto_send_headers     => { parse => 'boolean', type => SCALAR|UNDEF, default => 1,
			        descr => "Whether HTTP headers should be auto-generated" },

     decline_dirs          => { parse => 'boolean', type => SCALAR|UNDEF, default => 1,
			        descr => "Whether Mason should decline to handle requests for directories" },
     multiple_config       => { parse => 'boolean', type => SCALAR|UNDEF, optional => 1,
			        descr => "Whether multiple Mason configurations are in effect, such as when using VirtualHosts" },
     top_level_predicate   => { parse => 'code',    type => CODEREF,      default => sub () {1},
			        descr => "A subroutine that tests whether an HTTP request is valid" },

     # the only required param
     interp                => { isa => 'HTML::Mason::Interp',
			        descr => "A Mason interpreter for processing components" },
    );

__PACKAGE__->contained_objects
    (
     interp => 'HTML::Mason::Interp',
    );

use vars qw($AH);

sub import
{
    my $pack = shift;

    if ( my $args_method = $pack->get_param('ArgsMethod') )
    {
	if ($args_method eq 'CGI')
	{
	    unless (defined $CGI::VERSION)
	    {
		require CGI;
	    }
	}
	elsif ($args_method eq 'mod_perl')
	{
	    unless (defined $Apache::Request::VERSION)
	    {
		require Apache::Request;
	    }
	}

	# if we are in a simple conf file (meaning one without
	# multiple different Mason configs) we make the apachehandler
	# object now and simply reuse it later in the handler sub
	$AH = $pack->make_ah() if $pack->_in_simple_conf_file;
    }
}

#
# This is my best guess as to whether we are being configured via the
# conf file without multiple configs.  Without a comp root it will
# blow up sooner or later anyway.  This may not be the case in the
# future though.
#
sub _in_simple_conf_file
{
    my $self = shift;

    return $ENV{MOD_PERL} && $self->_get_string_param('MasonCompRoot');
}

my %AH;
sub make_ah
{
    my ($package) = shift;

    my ($vals, %p) = $package->_get_mason_params;

    #
    # Now that we have all the original config strings stored, we put
    # them all together in a string that we use to determine whether
    # or not we've seen this particular set of config values before.
    #
    my $key = '';
    foreach my $k (sort keys %$vals)
    {
	$key .= $k;
	$key .= $vals->{$k};
    }

    #
    # If the user has virtual hosts, each with a different document
    # root, then we will have to be called from the handler method.
    # This means we have an active request.  In order to distinguish
    # between virtual hosts with identical config directives that have
    # no comp root defined (meaning they expect to use the default
    # comp root), we append the document root for the current request
    # to the key.
    #
    $key .= Apache->request->document_root if Apache->request;

    return $AH{$key} if exists $AH{$key};

    if (exists $p{comp_root}) {
	if (@{$p{comp_root}} == 1 && $p{comp_root}->[0] !~ /=>/) {
	    $p{comp_root} = $p{comp_root}[0];  # Convert to a simple string
	} else {
	    foreach my $root (@{$p{comp_root}}) {
		$root = [ split /\s*=>\s*/, $root, 2 ];
		param_error "Configuration parameter MasonCompRoot must be either a single string value ".
			    "or multiple key/value pairs like 'foo => /home/mason/foo'" 
		    unless defined $root->[1];
	    }
	}
    }

    my $ah = $package->new(%p);
    $AH{$key} = $ah if $key;

    # If we're running as superuser, change file ownership to http user & group
    if (!($> || $<) && $ah->interp->files_written)
    {
	chown Apache->server->uid, Apache->server->gid, $ah->interp->files_written
	    or system_error( "Can't change ownership of files written by interp object: $!\n" );
    }

    return $ah;
}

# The following routines handle getting information from $r->dir_config

sub calm_form {
    # Transform from StudlyCaps to name_like_this
    my ($self, $string) = @_;
    $string =~ s/^Mason//;
    $string =~ s/(^|.)([A-Z])/$1 ? "$1\L_$2" : "\L$2"/ge;
    return $string;
}

sub studly_form {
    # Transform from name_like_this to StudlyCaps
    my ($self, $string) = @_;
    $string =~ s/(?:^|_)(\w)/\U$1/g;
    return $string;
}

sub _get_mason_params
{
    my $self = shift;

    my $c = Apache->request ? Apache->request : Apache->server;

    my $config = $c->dir_config;
    my $specs = $self->allowed_params;

    #
    # We will accumulate all the string versions of the keys and
    # values here for later use.
    #
    my %vals;

    # Get all params starting with 'Mason'
    my @candidates = map { /^Mason/ ? $self->calm_form($_) : () } keys %$config;
    return (\%vals, map { $_ => $self->get_param($_, $specs->{$_}, \%vals) } @candidates);
}

sub get_param {
    # Gets a single config item from dir_config.

    my ($self, $key, $spec, $vals) = @_;
    $key = $self->calm_form($key);

    # If we weren't given a spec, try to locate one in our own class.
    $spec ||= $self->allowed_params->{$key};
    error "Unknown config item '$key'" unless $spec;

    # Guess the default parse type from the Params::Validate validation spec
    my $type = ($spec->{parse} or
		$spec->{type} & ARRAYREF ? 'list' :
		$spec->{type} & SCALAR   ? 'string' :
		$spec->{type} & CODEREF  ? 'code' :
		undef);
    error "Unknown parse type for config item '$key'" unless $type;

    my $method = "_get_${type}_param";
    return scalar $self->$method('Mason'.$self->studly_form($key), $vals);
}

sub _get_string_param
{
    my $self = shift;
    my ($p, $val) = $self->_get_val(@_);

    return $val;
}

sub _get_boolean_param
{
    my $self = shift;
    my ($p, $val) = $self->_get_val(@_);

    return $val;
}

sub _get_code_param
{
    my $self = shift;
    my ($p, $val) = $self->_get_val(@_);

    return unless $val;

    my $sub_ref = eval $val;

    param_error "Configuration parameter '$p' is not valid perl:\n$@\n"
	if $@;

    return $sub_ref;
}

sub _get_list_param
{
    my $self = shift;
    my ($p, @val) = $self->_get_val($_[0], 1);
    if (@val == 1 && ! defined $val[0])
    {
	@val = ();
    }

    return \@val;
}

sub _get_val
{
    my ($self, $p, $wantarray, $vals) = @_;

    my $c = Apache->request ? Apache->request : Apache->server;

    my @val = Apache::perl_hook('TableApi') ? $c->dir_config->get($p) : $c->dir_config($p);

    param_error "Only a single value is allowed for configuration parameter '$p'\n"
	if @val > 1 && ! $wantarray;

    $vals->{$p} = join '', @val if $vals;

    return ($p, $wantarray ? @val : $val[0]);
}

sub new
{
    my $class = shift;

    my $allowed_params = $class->allowed_params;

    my %defaults;
    if (exists $allowed_params->{comp_root} && Apache->request)
    {
	$defaults{comp_root} = Apache->request->document_root;
    }

    if (exists $allowed_params->{data_dir})
    {
	# constructs path to <server root>/mason
	$defaults{data_dir} = Apache->server_root_relative('mason');
    }

    $defaults{request_class}  = 'HTML::Mason::Request::ApacheHandler';
    $defaults{resolver_class} = 'HTML::Mason::Resolver::File::ApacheHandler';

    # Set default error_format based on error_mode
    my %params = @_;
    if (exists($params{error_mode}) and $params{error_mode} eq 'fatal') {
	$defaults{error_format} = 'line';
    } else {
	$defaults{error_mode} = 'output';
	$defaults{error_format} = 'html';
    }

    my @args = $class->create_contained_objects(%defaults, @_);

    my $self = bless {validate( @args, $class->validation_spec )}, $class;

    unless ( $self->interp->resolver->can('apache_request_to_comp_path') )
    {
	error "The resolver class your Interp object uses does not implement the 'apache_request_to_comp_path' method.  This means that ApacheHandler cannot resolve requests.  Are you using a handler.pl file created before version 1.10?  Please see the handler.pl sample that comes with the latest version of Mason.";
    }

    $self->_initialize;
    return $self;
}

# Register with Apache::Status at module startup.  Will get replaced
# with a more informative status once an interpreter has been created.

my $status_name = 'mason0001';

{
    Apache::Status->menu_item
	    ($status_name => __PACKAGE__->allowed_params->{apache_status_title}{default},
	     sub { ["<b>(no interpreters created in this child yet)</b>"] });
}


sub _initialize {
    my ($self) = @_;

    if ($self->args_method eq 'mod_perl') {
	unless (defined $Apache::Request::VERSION) {
	    warn "Loading Apache::Request at runtime.  You could increase shared memory between Apache processes by preloading it in your httpd.conf or handler.pl file\n";
	    require Apache::Request;
	}
    } else {
	unless (defined $CGI::VERSION) {
	    warn "Loading CGI at runtime.  You could increase shared memory between Apache processes by preloading it in your httpd.conf or handler.pl file\n";

	    require CGI;
	}
    }

    # Add an HTML::Mason menu item to the /perl-status page.
    if (defined $Apache::Status::VERSION) {
	# A closure, carries a reference to $self
	my $statsub = sub {
	    my ($r,$q) = @_; # request and CGI objects
	    return [] if !defined($r);

	    if ($r->path_info and $r->path_info =~ /expire_code_cache=(.*)/) {
		$self->interp->delete_from_code_cache($1);
	    }

	    return ["<center><h2>" . $self->apache_status_title . "</h2></center>" ,
		    $self->status_as_html(apache_req => $r),
		    $self->interp->status_as_html(ah => $self, apache_req => $r)];
	};
	local $^W = 0; # to avoid subroutine redefined warnings
	Apache::Status->menu_item($status_name, $self->apache_status_title, $statsub);
    }

    my $interp = $self->interp;

    #
    # Create data subdirectories if necessary. mkpath will die on error.
    #
    foreach my $subdir (qw(preview)) {
	my @newdirs = mkpath($interp->data_dir."/$subdir",0,0775);
	$interp->push_files_written(@newdirs);
    }

    #
    # Allow global $r in components
    #
    $interp->compiler->add_allowed_globals('$r');
}

# Generate HTML that describes ApacheHandler's current status.
# This is used in things like Apache::Status reports.

sub status_as_html {
    my ($self, %p) = @_;

    # Should I be scared about this?  =)

    my $comp_source = <<'EOF';
<h3>ApacheHandler properties:</h3>
<blockquote>
 <tt>
<table width="75%">
<%perl>
foreach my $property (sort keys %$ah) {
    my $val = $ah->{$property};
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
</blockquote>

<%args>
 $ah       # The ApacheHandler we'll elucidate
 %valid    # Contains default values for member data
</%args>
EOF

    my $interp = $self->interp;
    my $comp = $interp->make_component(comp_source => $comp_source);
    my $out;
    local $interp->{out_method} = \$out;

    my $request = $self->interp->make_request( ah => $self,
					       apache_req => $p{apache_req},
					     );
    $request->exec($comp, ah => $self, valid => $interp->allowed_params);
    return $out;
}

#
# Standard entry point for handling request
#
sub handle_request {

    #
    # Why do we use $apreq instead of $r here? A scoping bug in certain
    # versions of Perl 5.005 was getting confused about $r being used
    # in components, and the easiest workaround was to rename "$r" to
    # something else in this routine.  Go figure...
    # -jswartz 5/23
    #
    my ($self, $apreq) = @_;
    my ($retval);
    my $interp = $self->interp;

    if (lc($apreq->dir_config('Filter')) eq 'on') {
	$apreq = $apreq->filter_register;
    }

    eval { $retval = $self->handle_request_1($apreq) };

    if (my $err = $@) {
	#
	# If first component was not found, return NOT_FOUND. In case
	# of POST we must trick Apache into not reading POST content
	# again. Wish there were a more standardized way to do this...
	#
	if (isa_mason_exception($err, 'TopLevelNotFound')) {
	    if ($apreq->method eq 'POST') {
		$apreq->method('GET');
		$apreq->headers_in->unset('Content-length');
	    }

	    # Log the error the same way that Apache does (taken from default_handler in http_core.c)
	    $apreq->log_error("[Mason] File does not exist: ", $apreq->filename . ($apreq->path_info ? $apreq->path_info : ""));
	    return NOT_FOUND;

	} else {
	    die $err;
	}
    }
    return defined($retval) ? $retval : &OK;
}

#
# Shorthand for various data subdirectories and files.
#
sub preview_dir { return shift->interp->data_dir . "/preview" }

sub handle_request_1
{
    my ($self, $r) = @_;

    my $interp = $self->interp;

    #
    # If filename is a directory, then either decline or simply reset
    # the content type, depending on the value of decline_dirs.
    #
    # ** We should be able to use $r->finfo here, but finfo is broken
    # in some versions of mod_perl (e.g. see Shane Adams message on
    # mod_perl list on 9/10/00)
    #
    my $is_dir = -d $r->filename;
    my $is_file = -f _;

    if ($is_dir) {
	if ($self->decline_dirs) {
	    return DECLINED;
	} else {
	    $r->content_type(undef);
	}
    }

    #
    # Compute the component path via the resolver. Return NOT_FOUND on failure.
    #
    my $comp_path = $interp->resolver->apache_request_to_comp_path($r);
    unless ($comp_path) {
	#
	# Append path_info if filename does not represent an existing file
	# (mainly for dhandlers).
	#
	my $pathname = $r->filename;
	$pathname .= $r->path_info unless $is_file;

	$r->warn("[Mason] Cannot resolve file to component: $pathname (is file outside component root?)");
	return NOT_FOUND;
    }

    #
    # Return NOT_FOUND if file does not pass top level predicate.
    #
    if ($is_file and !$self->top_level_predicate->($r->filename)) {
	$r->warn("[Mason] File fails top level predicate: ".$r->filename);
	return NOT_FOUND;
    }

    # If someone is using a custom request class that doesn't accept
    # 'ah' and 'apache_req' that's their problem.
    #
    my $request = $interp->make_request( ah => $self,
					 apache_req => $r,
				       );

    my %args;
    if ($self->args_method eq 'mod_perl') {
	$r = Apache::Request->new($r);
	%args = $self->_mod_perl_args($r, $request);
    } else {
	%args = $self->_cgi_args($r, $request);
    }

    #
    # Set up interpreter global variables.
    #
    $interp->set_global(r=>$r);

    #
    # Why this strangeness with taking a reference to Apache::print?
    # See below, where a bit of funkiness is done to catch calls to
    # print and $r->print inside components.  Without this, calling
    # $m->flush_buffer can lead to a loop where the content
    # disappears.
    #
    # By using the reference to the original function we ensure that
    # we call the version of the sub that sends its output to the
    # right place.
    #
    my $print = \&Apache::print;

    # Craft the request's out method to handle http headers, content
    # length, and HEAD requests.

    my $must_send_headers = $self->auto_send_headers;
    my $out_method = sub {

	# Send headers if they have not been sent by us or by user.
	if ($must_send_headers) {
	    unless (http_header_sent($r)) {
		$r->send_http_header();
	    }
	    $must_send_headers = 0;
	}

	# Call $r->print. If request was HEAD, suppress output
	# but allow the request to continue for consistency.
	unless ($r->method eq 'HEAD') {
	    $r->$print(grep {defined} @_);
	}
    };
    $request->out_method($out_method);

    my $retval;
    {
	# Remap $r->print to Mason's $m->print while executing request
	local *Apache::print = sub { shift; $request->print(@_) };
	$retval = $request->exec($comp_path, %args);
    }

    # On a success code, send headers if they have not been sent.
    # On an error code, leave it to Apache to send the headers.
    if ($must_send_headers and !http_header_sent($r) and (!$retval or $retval==200)) {
	$r->send_http_header();
    }
    undef $request; # ward off leak

    return $retval;
}

#
# Get %args hash via CGI package
#
sub _cgi_args
{
    my ($self, $r, $request) = @_;

    # For optimization, don't bother creating a CGI object if request
    # is a GET with no query string
    return if $r->method eq 'GET' && !scalar($r->args);

    my $q = CGI->new;
    $request->cgi_object($q);

    return HTML::Mason::Utils::cgi_request_args($q, $r->method);
}

#
# Get %args hash via Apache::Request package.
#
sub _mod_perl_args
{
    my ($self, $apr, $request) = @_;

    my %args;
    foreach my $key ( $apr->param ) {
	my @values = $apr->param($key);
	$args{$key} = @values == 1 ? $values[0] : \@values;
    }

    return %args;
}

#
# Determines whether the http header has been sent.
#
sub http_header_sent { shift->header_out("Content-type") }

#
# PerlHandler HTML::Mason::ApacheHandler
#
sub handler ($$)
{
    my ($package, $r) = @_;

    my $ah = $AH || $package->make_ah();

    return $ah->handle_request($r);
}

1;

__END__

=head1 NAME

HTML::Mason::ApacheHandler - Mason/mod_perl interface

=head1 SYNOPSIS

    use HTML::Mason::ApacheHandler;

    my $ah = new HTML::Mason::ApacheHandler (..name/value params..);
    ...
    sub handler {
        my $r = shift;
        $ah->handle_request($r);
    }

=head1 DESCRIPTION

The ApacheHandler object links Mason to mod_perl, running components in
response to HTTP requests. It is controlled primarily through
parameters to the new() constructor.

handle_request() is not a user method, but rather is called from the
HTML::Mason::handler() routine in handler.pl.


=head1 PARAMETERS TO THE new() CONTRUCTOR

=over

=item apache_status_title

Title that you want this ApacheHandler to appear as under
Apache::Status.  Default is "HTML::Mason status".  This is useful if
you create more than one ApacheHandler object and want them all
visible via Apache::Status.

=item args_method

Method to use for unpacking GET and POST arguments. The valid options
are 'CGI' and 'mod_perl'; these indicate that a CGI.pm or
Apache::Request object (respectively) will be created for the purposes
of argument handling.

Apache::Request is the default and requires that you have installed
this package.

If the args_method is 'CGI', the Mason request object ($m) will have a
method called C<cgi_object> available.  This method returns the CGI
object used in the ApacheHandler code.

If args_method is 'mod_perl', the $r global is upgraded to an
Apache::Request object. This object inherits all Apache methods and
adds a few of its own, dealing with parameters and file uploads.  See
L<Apache::Request> for more information.

While Mason will load Apache::Request or CGI as needed at runtime, it
is recommended that you preload the relevant module either in your
httpd.conf or handler.pl file, as this will save some memory.

=item auto_send_headers

True or undef; default true.  Indicates whether Mason should
automatically send HTTP headers before sending content back to the
client. If you set to false, you should call $r->send_http_header
manually.

See the L<Devel/sending_http_headers> of the Component Developer's
Guide for details about the automatic header feature.

=item decline_dirs

Indicates whether Mason should decline directory requests, leaving
Apache to serve up a directory index or a FORBIDDEN error as
appropriate. Default is 1. See L<Admin/Allowing directory requests>
for more information about handling directories with Mason.

=item interp

The only required parameter.  Specifies a Mason interpreter to use for
handling requests.  The interpreter should be an instance of the
C<HTML::Mason::Interp> class, or a subclass thereof.

=item top_level_predicate

Reference to a subroutine that decides whether a component can answer
top level requests. This allows for private-use components that live
within the DocumentRoot but are inaccesible from URLs. By default,
always returns 1.

The subroutine receives one parameter, the absolute path to the
component.  It then returns either a true (serve component) or false
(reject component). In this example, the predicate rejects requests
for components whose name starts with an "_" character:

    top_level_predicate => sub { $_[0] !~ m:/_[^/]+$: }

=back

=head1 ACCESSOR METHODS

All of the above properties have standard accessor methods of the
same name: no arguments retrieves the value, and one argument sets it.
For example:

    my $ah = new HTML::Mason::ApacheHandler;
    my $auto_send_headers = $ah->auto_send_headers;
    $ah->auto_send_headers(0);

=head1 SEE ALSO

L<HTML::Mason>,
L<HTML::Mason::Admin>,
L<HTML::Mason::Interp>

=cut
