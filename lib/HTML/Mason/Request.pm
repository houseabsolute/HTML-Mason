# Copyright (c) 1998-2002 by Jonathan Swartz. All rights reserved.
# This program is free software; you can redistribute it and/or modify it
# under the same terms as Perl itself.

package HTML::Mason::Request;

use strict;

use File::Spec;
use HTML::Mason::Tools qw(read_file compress_path load_pkg absolute_comp_path);
use HTML::Mason::Utils;
use HTML::Mason::Buffer;

use HTML::Mason::Container;
use base qw(HTML::Mason::Container);

use HTML::Mason::Exceptions( abbr => [qw(param_error syntax_error abort_error
					 top_level_not_found_error error)] );

use Params::Validate qw(:all);
Params::Validate::validation_options( on_fail => sub { param_error( join '', @_ ) } );

use HTML::Mason::MethodMaker
    ( read_only => [ qw( aborted
			 aborted_value
			 count
			 declined
			 interp
			 top_comp ) ],

      read_write => [ qw( autoflush
                          data_cache_defaults
			  error_format
			  error_mode
			  out_method ) ],
    );

__PACKAGE__->valid_params
    (
     autoflush  => { parse => 'boolean', default => 0, type => SCALAR,
		     descr => "Whether output should be buffered or sent immediately" },
     interp     => { isa => 'HTML::Mason::Interp',
		     descr => "An interpreter for Mason control functions" },
     error_format => { parse => 'string', type => SCALAR, default => 'text',
		       callbacks => { "must be one of 'brief', 'text', 'line', or 'html'" =>
					  sub { $_[0] =~ /^(?:brief|text|line|html)$/; } },
                       descr => "How error messages are formatted" },
     error_mode => { parse => 'string', type => SCALAR, default => 'fatal',
		     callbacks => { "must be one of 'output' or 'fatal'" =>
					sub { $_[0] =~ /^(?:output|fatal)$/ } },
		     descr => "How error conditions are returned to the caller" },
     out_method => { type => SCALARREF | CODEREF,
		     descr => "A subroutine or scalar reference through which all output will pass" },
     data_cache_defaults => { type => HASHREF|UNDEF, optional => 1,
			      descr => "A hash of default parameters for Cache::Cache" },
    );

__PACKAGE__->contained_objects
    (
     buffer     => { class => 'HTML::Mason::Buffer',
		     delayed => 1 },
    );

sub new
{
    my $class = shift;

    my @args = $class->create_contained_objects(@_);

    my $self = bless {validate( @args, $class->validation_spec ),
		      aborted => undef,
		      aborted_value => undef,
		      count => 0,
		      declined => undef,
		      dhandler_arg => undef,
		      buffer_stack => undef,
		      stack => undef,
		      wrapper_chain => undef,
		      wrapper_index => undef,
		     }, $class;

    $self->{count} = ++$self->{interp}{request_count};
    $self->_initialize;
    return $self;
}

# in the future this method may do something completely different but
# for now this works just fine.
sub instance {
    return $HTML::Mason::Commands::m;
}

sub _initialize {
    my ($self) = @_;
    my $interp = $self->interp;

    # Initialize hooks arrays for fast access
    while (my ($type,$href) = each(%{$interp->{hooks}})) {
	$self->{"hooks_$type"} = [values(%$href)] if (%$href);
    }

    # create base buffer
    $self->{buffer_stack} = [];
    $self->{stack} = [];
}

sub _reinitialize {
    my ($self) = @_;
    $self->_initialize;
    foreach my $field (qw(aborted aborted_value declined dhandler_arg)) {
	$self->{$field} = undef;
    }

    $self->{buffer_stack} = [];
}

sub exec {
    my ($self, $comp, @args) = @_;
    my $interp = $self->interp;

    # All errors returned from this routine will be in exception form.
    local $SIG{'__DIE__'} = sub {
	my $err = $_[0];
	UNIVERSAL::can($err, 'rethrow') ? $err->rethrow : error $err;
    };

    my @result;
    eval {
	# Check if reload file has changed.
	$interp->check_reload_file if ($interp->use_reload_file);

	# Purge code cache if necessary. Generally happens at the end of
	# the component; this is just in case many errors are occurring.
	$interp->purge_code_cache;

	# $comp can be an absolute path or component object.  If a path,
	# load into object. If not found, check for dhandler.
	my ($path, $orig_path);
	if (!ref($comp) && substr($comp,0,1) eq '/') {
	    $comp =~ s,/+,/,g;
	    $orig_path = $path = $comp;
	    $comp = $self->fetch_comp($path);

	    unless ($comp) {
		if ( $comp = $interp->find_comp_upwards($path, $interp->dhandler_name) ) {

		    my $parent_path = $comp->dir_path;
		    ($self->{dhandler_arg} = $path) =~ s{^$parent_path/?}{};
		}
	    }
	    unless ($comp) {
		top_level_not_found_error "could not find component for initial path '$path'\n";
	    }
	} elsif ( ! UNIVERSAL::isa( $comp, 'HTML::Mason::Component' ) ) {
	    param_error "exec: first argument ($comp) must be an absolute component path or a component object";
	}

	# This block repeats only if $m->decline is called in a component
	my $declined;
	do
	{
	    $declined = 0;

	    my $buffer = $self->create_delayed_object( 'buffer', sink => $self->out_method );
	    $self->push_buffer_stack($buffer);

	    # Build wrapper chain and index.
	    my $first_comp;
	    {
		my @wrapper_chain = ($comp);

		for (my $parent = $comp->parent; $parent; $parent = $parent->parent) {
		    unshift(@wrapper_chain,$parent);
		    error "inheritance chain length > 32 (infinite inheritance loop?)"
			if (@wrapper_chain > 32);
		}

		$first_comp = $wrapper_chain[0];
		$self->{wrapper_chain} = [@wrapper_chain];
		$self->{wrapper_index} = {map((($wrapper_chain[$_]->path || '') => $_),(0..$#wrapper_chain))};
	    }

	    # Fill top_level slots for introspection.
	    $self->{top_comp} = $comp;
	    $self->{top_args} = \@args;

	    # Call the first component, with STDOUT mapped to $m->out.
	    tie *STDOUT, 'Tie::Handle::Mason', $self;
	    if (wantarray) {
		@result = eval {$self->comp({base_comp=>$comp}, $first_comp, @args)};
	    } else {
		$result[0] = eval {$self->comp({base_comp=>$comp}, $first_comp, @args)};
	    }
	    untie *STDOUT;

	    if (my $err = $@) {
		# If declined, try to find the next dhandler.
		if ( ($declined = $self->declined) and $path) {
		    $path =~ s,/[^/]+$,, if defined($self->{dhandler_arg});
		    if (my $next_comp = $interp->find_comp_upwards($path, $interp->dhandler_name)) {
			$comp = $next_comp;
			my $parent = $comp->dir_path;
			$self->_reinitialize;
			($self->{dhandler_arg} = $orig_path) =~ s{^$parent/}{};
		    }
		} else {
		    UNIVERSAL::can($err, 'rethrow') ? $err->rethrow : error $err;
		}
	    }

	} while ($declined && $path);
    };

    # Handle errors.
    my $err = $@;
    if ($err and !$self->aborted) {
	$self->pop_buffer_stack;
	$self->_handle_error($err);
	return;
    }

    # Flush output buffer.
    $self->flush_buffer;
    $self->pop_buffer_stack;

    # Purge code cache if necessary. We do this at the end so as not
    # to affect the response of the request as much.
    $interp->purge_code_cache;

    # Return aborted value or result.
    return ($self->aborted ? $self->aborted_value : (wantarray ? @result : $result[0]));
}

#
# Display or die with error as dictated by error_mode and error_format.
#
sub _handle_error
{
    my ($self, $err) = @_;

    # Set error format for when error is stringified.
    if (UNIVERSAL::can($err, 'format')) {
	$err->format($self->error_format);
    }

    # In fatal mode, die with error. In display mode, output stringified error.
    if ($self->error_mode eq 'fatal') {
	die $err;
    } else {
	UNIVERSAL::isa( $self->out_method, 'CODE' ) ? $self->out_method->("$err") : ( ${ $self->out_method } = "$err" );
    }
}

#
# Abort out of current execution.
#
sub abort
{
    my ($self) = @_;
    $self->{aborted} = 1;
    $self->{aborted_value} = $_[1] || $self->{aborted_value} || undef;
    HTML::Mason::Exception::Abort( error => 'Request->abort was called', aborted_value => $self->{aborted_value} );
}

#
# Return a new cache object specific to this component.
#
sub cache
{
    my ($self, %options) = @_;

    if ($self->data_cache_defaults) {
	%options = (%{$self->data_cache_defaults}, %options);
    }
    $options{namespace}   ||= compress_path($self->current_comp->comp_id);
    $options{cache_root}  ||= File::Spec->catdir($self->interp->data_dir,"cache");
    $options{username}      = "mason";

    my $cache_class = 'Cache::FileCache';
    if ($options{cache_class}) {
	$cache_class = $options{cache_class};
	$cache_class = "Cache::$cache_class" unless $cache_class =~ /::/;
	delete($options{cache_class});
    }
    load_pkg('Cache::Cache', '$m->cache requires the Cache::Cache module, available from CPAN.');
    load_pkg($cache_class, 'Fix your Cache::Cache installation or choose another cache class.');

    my $cache = $cache_class->new (\%options)
	or error "could not create cache object";

    return $cache;
}

sub cache_self {
    my ($self, %options) = @_;

    return if $self->{in_cache_self};

    my $expire = delete $options{expire_in};
    my $key = '__cache_self__';
    $key .= delete $options{key} if exists $options{key};

    my $cache = $self->cache(%options);

    my ($output, $retval);
    if (my $cached = $cache->get($key)) {
	($output, $retval) = @$cached;
    } else {
	local $self->{in_cache_self} = 1;

	$self->push_buffer_stack($self->top_buffer->new_child(sink => \$output, ignore_flush => 1));

	my $comp = $self->top_stack->{comp};
	my @args = @{ $self->top_stack->{args} };

	#
	# Because this method should always be called in a scalar
	# context we need to go back and find the context that the
	# component was first called in (back up there in the comp
	# method).
	#
	my $wantarray = (caller(1))[5];

	my @result;

	eval {
	    if ($wantarray) {
		@result = $comp->run(@args);
	    } elsif (defined $wantarray) {
		$result[0] = $comp->run(@args);
	    } else {
		$comp->run(@args);
	    }
	};

	#
	# Whether there was an error or not we need to pop the buffer
	# stack.
	#
	my $buffer = $self->pop_buffer_stack;

	if (my $err = $@) {
	    UNIVERSAL::can($err, 'rethrow') ? $err->rethrow : error $err;
	}

	$retval = \@result;

	$cache->set($key => [$output, $retval], $expire ? $expire : ());
    }

    $self->out($output);

    #
    # Return the component return value in case the caller is interested,
    # followed by 1 indicating the cache retrieval success.
    #
    return (@$retval, 1);

}

sub call_dynamic {
    my ($m, $key, @args) = @_;
    my $comp = ($m->current_comp->is_subcomp) ? $m->current_comp->owner : $m->current_comp;
    if (!defined($comp->dynamic_subs_request) or $comp->dynamic_subs_request ne $m) {
	$comp->dynamic_subs_init;
	$comp->dynamic_subs_request($m);
    }

    return $comp->run_dynamic_sub($key, @args);
}

sub call_next {
    my ($self,@extra_args) = @_;
    my $comp = $self->fetch_next
	or error "call_next: no next component to invoke";
    return $self->comp($comp, @{$self->current_args}, @extra_args);
}

sub caller
{
    my ($self) = @_;
    return $self->callers(1);
}

#
# Return a specified component from the stack, or the whole stack as a list.
#
sub callers
{
    my $self = shift;
    if (defined $_[0]) {
	return $self->stack_entry($_[0])->{comp};
    } else {
	return map($_->{comp}, reverse $self->stack);
    }
}

#
# Return a specified argument list from the stack.
#
sub caller_args
{
    my ($self,$index) = @_;
    param_error "caller_args expects stack level as argument" unless defined $index;

    my $args = $self->stack_entry($index)->{args};
    return wantarray ? @$args : { @$args };
}

sub comp_exists
{
    my ($self, $path) = @_;
    return $self->interp->comp_exists(absolute_comp_path($path, $self->current_comp->dir_path)) ? 1 : 0;
}

sub decline
{
    my ($self) = @_;
    $self->{declined} = 1;
    error "decline() called (and not caught)";
}

#
# Return the current number of stack levels. 1 means top level, 0
# means that no component has been called yet.
#
sub depth
{
    my ($self) = @_;
    return scalar $self->stack;
}

sub dhandler_arg { shift->{dhandler_arg} }

#
# Given a component path (absolute or relative), returns a component.
# Handles SELF and PARENT, comp:method, relative->absolute
# conversion, and local subcomponents.
#
sub fetch_comp
{
    my ($self,$path) = @_;
    param_error "fetch_comp: requires path as first argument" unless defined($path);

    #
    # Handle paths SELF and PARENT
    #
    if ($path eq 'SELF') {
	return $self->base_comp;
    }
    if ($path eq 'PARENT') {
	my $c = $self->current_comp->parent
	    or error "PARENT designator used from component with no parent";
	return $c;
    }

    #
    # Handle paths of the form comp_path:method_name
    #
    if (index($path,':') != -1) {
	my $method_comp;
	my ($owner_path,$method_name) = split(':',$path,2);
	my $owner_comp = $self->fetch_comp($owner_path)
	    or error "could not find component for path '$owner_path'\n";
	$owner_comp->_locate_inherited('methods',$method_name,\$method_comp)
	    or error "no method '$method_name' for component " . $owner_comp->title;
	return $method_comp;
    }

    #
    # If path does not contain a slash, check for a subcomponent in the
    # current component first.
    #
    if ($path !~ /\//) {
	my $cur_comp = $self->current_comp;
	# Check my subcomponents.
	if (my $subcomp = $cur_comp->subcomps->{$path}) {
	    return $subcomp;
	}
	# If I am a subcomponent, also check my owner's subcomponents.
	# This won't work when we go to multiply embedded subcomponents...
	if ($cur_comp->is_subcomp and my $subcomp = $cur_comp->owner->subcomps->{$path}) {
	    return $subcomp;
	}
    }

    #
    # Otherwise pass the absolute path to interp->load.
    #
    $path = absolute_comp_path($path, $self->current_comp->dir_path)
	unless substr($path, 0, 1) eq '/';

    my $comp = $self->interp->load($path);

    return $comp;
}

#
# Fetch the index of the next component in wrapper chain. If current
# component is not in chain, search the component stack for the most
# recent one that was.
#
sub _fetch_next_helper {
    my ($self) = @_;
    my $index = $self->{wrapper_index}->{$self->current_comp->path};
    unless (defined($index)) {
	my @callers = $self->callers;
	shift(@callers);
	while (my $comp = shift(@callers) and !defined($index)) {
	    $index = $self->{wrapper_index}->{$comp->path};
	}
    }
    return $index;
}

#
# Fetch next component in wrapper chain.
#
sub fetch_next {
    my ($self) = @_;
    my $index = $self->_fetch_next_helper;
    error "fetch_next: cannot find next component in chain"
	unless defined($index);
    return $self->{wrapper_chain}->[$index+1];
}

#
# Fetch remaining components in wrapper chain.
#
sub fetch_next_all {
    my ($self) = @_;
    my $index = $self->_fetch_next_helper;
    error "fetch_next_all: cannot find next component in chain"
	unless defined($index);
    my @wc = @{$self->{wrapper_chain}};
    return @wc[($index+1)..$#wc];
}

sub file
{
    my ($self,$file) = @_;
    my $interp = $self->interp;
    unless ( File::Spec->file_name_is_absolute($file) ) {
	if ($self->current_comp->is_file_based) {
	    my $source_dir = $self->current_comp->source_dir;
	    $file = File::Spec->catfile( $source_dir, $file );
	} else {
	    $file = File::Spec->catfile( File::Spec->rootdir, $file );
	}
    }
    $self->call_hooks('start_file',$file);
    my $content = read_file($file,1);
    $self->call_hooks('end_file',$file);
    return $content;
}

sub print
{
    my $self = shift;
    $self->top_buffer->receive(@_);
    $self->flush_buffer if $self->autoflush;
}

*out = \&print;

#
# Execute the given component
#
sub comp {
    my $self = shift;

    # Get modifiers: optional hash reference passed in as first argument.
    # merge multiple hash references to simplify user and internal usage.
    my %mods = ();
    %mods = (%{shift()},%mods) while ref($_[0]) eq 'HASH';

    my ($comp,@args) = @_;
    my $interp = $self->interp;
    my $depth = $self->depth;
    param_error "comp: requires path or component as first argument"
	unless defined($comp);

    #
    # $comp can be an absolute path or component object.  If a path,
    # load into object.
    #
    my $path;
    if (!ref($comp)) {
	$path = $comp;
	$comp = $self->fetch_comp($path)
	    or error "could not find component for path '$path'\n";
    }

    #
    # Check for maximum recursion.
    #
    error "$depth levels deep in component stack (infinite recursive call?)\n"
        if ($depth >= $interp->max_recurse);

    #
    # $m is a dynamically scoped global containing this
    # request. This needs to be defined in the HTML::Mason::Commands
    # package, as well as the component package if that is different.
    #
    local $HTML::Mason::Commands::m = $self;
    $interp->set_global('m'=>$self) if ($interp->compiler->in_package ne 'HTML::Mason::Commands');

    #
    # Determine base_comp (base component for method and attribute inheritance)
    # User may override with { base_comp => $compref }
    # Don't change on SELF:x and PARENT:x calls
    # Assume they know what they are doing if a component ref is passed in
    #
    my $base_comp = exists($mods{base_comp}) ? $mods{base_comp} : $self->base_comp;
    unless ( $mods{base_comp} ||	# base_comp override
	     !$path || 		# path is undef if $comp is a reference
	     $path =~ m/^(?:SELF|PARENT)(?:\:..*)?$/ ) {
	$base_comp = ( $path =~ m/(.*):/ ?
		       $self->fetch_comp($1) :
		       $comp );
	$base_comp = $base_comp->owner if $base_comp->is_subcomp;
    }

    # Push new frame onto stack.
    $self->push_stack( {comp => $comp,
			args => [@args],
			base_comp => $base_comp,
			content => $mods{content},
		       } );

    if ($mods{store}) {
	# This extra buffer is to catch flushes (in the given scalar ref).
	# The component's main buffer can then be cleared without
	# affecting previously flushed output.
        $self->push_buffer_stack($self->top_buffer->new_child( sink => $mods{store}, ignore_flush => 1 ));
    }
    $self->push_buffer_stack($self->top_buffer->new_child);

    # Call start_comp hooks.
    $self->call_hooks('start_comp');

    my @result;

    # The eval block creates a new context so we need to get this
    # here.
    my $wantarray = wantarray;

    #
    # Finally, call component subroutine.
    #
    eval {
	if ($wantarray) {
	    @result = $comp->run(@args);
	} elsif (defined $wantarray) {
	    $result[0] = $comp->run(@args);
	} else {
	    $comp->run(@args);
	}
    };

    #
    # If an error occurred, pop stack and pass error down to next level.
    #
    if (my $err = $@) {
	# any unflushed output is at $self->top_buffer->output
	$self->pop_stack;
	$self->pop_buffer_stack;
	UNIVERSAL::can($err, 'rethrow') ? $err->rethrow : error $err;
    }

    #
    # Call end_comp hooks.
    #
    $self->call_hooks('end_comp');

    $self->top_buffer->flush;
    $self->pop_stack;
    $self->pop_buffer_stack;
    $self->pop_buffer_stack if ($mods{store});

    return wantarray ? @result : $result[0];  # Will return undef in void context (correct)
}

#
# Like comp, but return component output.
#
sub scomp {
    my $self = shift;
    my $buf;
    $self->comp({store=>\$buf},@_);
    return $buf;
}

sub content {
    my $self = shift;
    my $content = $self->top_stack->{content};
    return undef unless defined($content);

    # make the stack frame look like we are still the previous component
    my $old_frame = $self->pop_stack;

    $self->push_buffer_stack( $self->top_buffer->new_child( ignore_flush => 1 ) );
    eval { $content->(); };
    my $err = $@;

    my $buffer = $self->pop_buffer_stack;

    $self->push_stack($old_frame);

    if ($err) {
	UNIVERSAL::can($err, 'rethrow') ? $err->rethrow : error $err;
    }

    return $buffer->output;
}

#
# Call hooks of the specified type, passing along params if any.
#
sub call_hooks {
    my ($self, $type, @params) = @_;
    if ($self->{"hooks_$type"}) {
	foreach my $code (@{$self->{"hooks_$type"}}) {
	    $code->($self, @params);
	}
    }
}

#
# Cancel a specified hook for the remainder of this request.
#
sub suppress_hook {
    my ($self, %args) = @_;
    foreach (qw(name type)) {
	param_error "suppress_hook: must specify $_\n"
	    unless exists($args{$_});
    }
    my $code = $self->interp->hooks->{$args{type}}->{$args{name}};
    $self->{"hooks_$args{type}"} = [grep($_ ne $code,@{$self->{"hooks_$args{type}"}})];
}

#
# Reinstate a specified hook.
#
sub unsuppress_hook {
    my ($self, %args) = @_;
    foreach (qw(name type)) {
	param_error "unsuppress_hook: must specify $_\n"
	    unless exists($args{$_});
    }
    my $code = $self->interp->hooks->{$args{type}}->{$args{name}};
    $self->{"hooks_$args{type}"} = [grep($_ ne $code,@{$self->{"hooks_$args{type}"}})];
    push(@{$self->{"hooks_$args{type}"}},$code);
}

sub clear_buffer
{
    my $self = shift;
    for ($self->buffer_stack) {
	last if $_->ignore_flush;
	$_->clear;
    }
}

sub flush_buffer
{
    my $self = shift;
    for ($self->buffer_stack) {
	last if $_->ignore_flush;
	$_->flush;
    }
}

sub top_args
{
    my ($self) = @_;
    if (wantarray) {
	return @{$self->{top_args}};
    } else {
	my %h = @{$self->{top_args}};
	return \%h;
    }
}

#
# Subroutine called by every component while in debug mode, convenient
# for breakpointing.
#
sub debug_hook
{
    1;
}


#
# stack handling
#

# Return the current stack as an array.
sub stack {
    my ($self) = @_;
    return @{ $self->{stack} };
}

# Return the stack entry 'i' slots from the /back/ of the array
sub stack_entry {
    my ($self, $i) = @_;
    return $self->{stack}->[-1 - $i];
}

# Set or retrieve the hashref at the top of the stack.
sub top_stack {
    my ($self,$href) = @_;
    error "top_stack: nothing on component stack"
	unless $self->depth > 0;
    $self->{stack}->[-1] = $href if defined($href);
    return $self->{stack}->[-1];
}

sub push_stack {
    my ($self,$href) = @_;
    push @{ $self->{stack} }, $href;
}

sub pop_stack {
    my ($self) = @_;
    return pop @{ $self->{stack} };
}

sub push_buffer_stack {
    my $self = shift;

    validate_pos( @_, { can => [ qw( receive clear flush output ) ] } );

    push @{ $self->{buffer_stack} }, shift;
}

sub pop_buffer_stack {
    my ($self) = @_;
    return pop @{ $self->{buffer_stack} };
}

sub buffer_stack {
    my ($self) = @_;
    return wantarray ? reverse @{ $self->{buffer_stack} } : @{ $self->{buffer_stack} };
}


#
# Accessor methods for top of stack elements.
#
sub current_comp { return $_[0]->top_stack->{comp} }
sub current_args { return $_[0]->top_stack->{args} }
sub current_sink { return $_[0]->{buffer_stack}->[-1]->sink }
sub top_buffer { return $_[0]->{buffer_stack}->[-1] }
sub base_comp { return $_[0]->top_stack->{base_comp} }

package Tie::Handle::Mason;

sub TIEHANDLE
{
    my $class = shift;

    my $req = shift;
    my $object = shift;

    return bless { request => $req }, $class;
}

sub PRINT
{
    my $self = shift;

    $self->{request}->out(@_);
}

sub PRINTF
{
    my $self = shift;

    # apparently sprintf(@_) won't work, it needs to be a scalar
    # followed by a list
    $self->PRINT(sprintf(shift, @_));
}

1;

__END__

=head1 NAME

HTML::Mason::Request - Mason Request Class

=head1 SYNOPSIS

    $m->abort (...)
    $m->comp (...)
    etc.

=head1 DESCRIPTION

The Request API is your gateway to all Mason features not provided by
syntactic tags. Mason creates a new Request object for every web
request. Inside a component you access the current request object via
the global C<$m>.  Outside of a component, you can use the class
method C<instance>.

=head1 COMPONENT PATHS

The methods L<Request/comp>, L<Request/comp_exists>,
L<Request/fetch_comp>.  Component paths are like URL paths, and always
use a forward slash (/) as the separator, regardless of what your
operating system uses.

=over

=item *

If the path is absolute (starting with a '/'), then the component is
found relative to the component root.

=item *

If the path is relative (no leading '/'), then the component is found
relative to the current component directory.

=item *

If the path matches both a subcomponent and file-based component, the
subcomponent takes precedence.

=back

=head1 METHODS

=over

=for html <a name="item_instance">

=item instance

This class method returns the C<HTML::Mason:::Request> currently in
use.  If called when no Mason request is active it will return C<undef>.

=for html <a name="item_abort">

=item abort ([return value])

Ends the current request, finishing the page without returning
through components. The optional argument specifies the return
value from C<Interp::exec>; in a web environment, this ultimately
becomes the HTTP status code.

abort() is implemented via die() and can thus be caught by eval(). 

Under the current implementation, any pending C<E<lt>%filterE<gt>> sections will
not be applied to the output after an abort.  This is a known bug but
there is no easy workaround.

The methods C<aborted> and C<aborted_value> return a boolean
indicating whether the current request was aborted and the argument
with which it was aborted, respectively. These would be used,
for example, after an eval() returned with a non-empty C<$@>.

=for html <a name="item_aborted">

=item aborted

Returns true or undef indicating whether the current request was aborted
with C<abort>.

=for html <a name="item_aborted_value">

=item aborted_value

Returns the argument passed to C<abort> when the request was
aborted. Returns undef if the request was not aborted or was aborted
without an argument.

=for html <a name="item_base_comp">

=item base_comp

Returns the current base component for method and attributes.
Generally set to the original page component; however, if you invoke
call_method on a component, C<base_comp> is dynamically set to that
component until call_method exits. See L<Devel/Object-Oriented
Techniques> for examples of usage.

=for html <a name="item_cache">

=item cache (cache_class=>'...', [cache_options])

C<$m-E<gt>cache> returns a new cache object with a namespace specific
to this component.

I<cache_class> specifies the class of cache object to create. It
defaults to Cache::FileCache and must be a subclass of Cache::Cache.
If I<cache_class> does not contain a "::", the prefix "Cache::" is
automatically prepended.

I<cache_options> may include any valid options to the new() method of
the cache class. e.g. for Cache::FileCache, valid options include
expires_in, max_size, and cache_depth.

See the L<Devel/data caching> section of the I<Component Developer's
Guide> for examples and caching strategies. See the Cache::Cache
documentation for a complete list of options and methods.

=for html <a name="item_cache_self">

=item cache_self (expire_in => '...', key => '...', [cache_options])

This method is called by a component when it wants to cache its entire
output.

It takes all of the options which can be passed to the cache method.

In addition, it takes two additional options.  The first,
I<expire_in>, will be passed to the caching object.  See the
Cache::Cache documentation for details on what formats it accepts.

The second, I<key> is an identifier used to uniquely identify the
cache results.  This means that you can call cache_self with different
keys and the component will be executed once for each key.  If no key
option is provided than a default key is used.

To cache the component's output:

    <%init>
    return if $m->cache_self(expire_in => '3 hours'[, key => 'fookey']);
    ... <rest of init> ...
    </%init>

To cache the component's return value:

    <%init>
    my (@retval) = $m->cache_self(expire_in => '3 hours'[, key => 'fookey']);

    return @retval if pop @retval;
    ... <rest of init> ...
    </%init>

The reason that we call C<pop> on C<@retval> is that the return value
from C<$m-E<gt>cache_self> is a list made up of the return value of
the component followed by a 1.  This is to ensure that $m->cache_self
always returns a true value when returning cached results.

=for html <a name="item_caller_args">

=item caller_args

Returns the arguments passed by the component at the specified stack
level. Use a positive argument to count from the current component and
a negative argument to count from the component at the bottom of the
stack. e.g.

    $m->caller_args(0)   # arguments passed to current component
    $m->caller_args(1)   # arguments passed to component that called us
    $m->caller_args(-1)  # arguments passed to first component executed

When called in scalar context, a hash reference is returned.  When
called in list context, a list of arguments (which may be assigned to
a hash) is returned.

=for html <a name="item_callers">

=item callers

With no arguments, returns the current component stack as a list of
component objects, starting with the current component and ending with
the top-level component. With one numeric argument, returns the
component object at that index in the list. Use a positive argument to
count from the current component and a negative argument to count from
the component at the bottom of the stack. e.g.

    my @comps = $m->callers   # all components
    $m->callers(0)            # current component
    $m->callers(1)            # component that called us
    $m->callers(-1)           # first component executed

=for html <a name="item_call_next">

=item call_next ([args...])

Calls the next component in the content wrapping chain; usually called
from an autohandler. With no arguments, the original arguments are
passed to the component.  Any arguments specified here serve to
augment and override (in case of conflict) the original
arguments. Works like C<$m-E<gt>comp> in terms of return value and
scalar/list context.  See the L<Devel/autohandlers> section of the
I<Component Developer's Guide> for examples.

=for html <a name="item_clear_buffer">

=item clear_buffer

Clears the Mason output buffer. Any output sent before this line is
discarded. Useful for handling error conditions that can only be
detected in the middle of a request.

clear_buffer is, of course, thwarted by C<flush_buffer>.

=for html <a name="item_comp">

=item comp (comp, args...)

Calls the component designated by I<comp> with the specified
option/value pairs. I<comp> may be a component path or a component
object. 

Components work exactly like Perl subroutines in terms of return
values and context. A component can return any type of value, which is
then returned from the C<$m-E<gt>comp> call.

The <& &> tag provides a convenient shortcut for C<$m-E<gt>comp>.

=for html <a name="item_comp_exists">

=item comp_exists (comp_path)

Returns 1 if I<comp_path> is the path of an existing component, 0
otherwise.  That path given may be relative, in which case the current
component's directory path will be prepended.

=for html <a name="content">

=item content

Evaluates the content (passed between <&| comp &> and </&> tags) of the 
current component, and returns the resulting text.

Returns undef if there is no content.

=for html <a name="item_count">

=item count

Returns the number of this request, which is unique for a given
request and interpreter.

=for html <a name="item_current_comp">

=item current_comp

Returns the current component object.

=for html <a name="item_decline">

=item decline

Used from a top-level component or dhandler, this method aborts the
current request and restarts with the next applicable dhandler
up the tree. If no dhandler is available, an error occurs.
This method bears no relation to the Apache DECLINED status
except in name.

=for html <a name="item_depth">

=item depth

Returns the current size of the component stack.  The lowest possible
value is 1, which indicates we are in the top-level component.

=for html <a name="item_dhandler_arg">

=item dhandler_arg

If the request has been handled by a dhandler, this method returns the
remainder of the URI or C<Interp::exec> path when the dhandler directory is
removed. Otherwise returns undef.

C<dhandler_arg> may be called from any component in the request, not just
the dhandler.

=for html <a name="item_fetch_comp">

=item fetch_comp (comp_path)

Given a I<comp_path>, returns the corresponding component object or
undef if no such component exists.

=for html <a name="item_fetch_next">

=item fetch_next

Returns the next component in the content wrapping chain, or undef if
there is no next component. Usually called from an autohandler.  See
the L<Devel/autohandlers> section of the I<Component Developer's
Guide> for usage and examples.

=for html <a name="item_fetch_next_all">

=item fetch_next_all

Returns a list of the remaining components in the content wrapping
chain. Usually called from an autohandler.  See the
L<Devel/autohandlers> section of the I<Component Developer's Guide>
for usage and examples.

=for html <a name="item_file">

=item file (filename)

Returns the contents of I<filename> as a string. If I<filename> is a
relative path, Mason prepends the current component directory.

=for html <a name="item_flush_buffer">

=item flush_buffer

Flushes the Mason output buffer. Under mod_perl, also sends HTTP
headers if they haven't been sent and calls $r->rflush to flush the
Apache buffer. Flushing the initial bytes of output can make your
servers appear more responsive.

=for html <a name="item_interp">

=item interp

Returns the Interp object associated with this request.

=for html <a name="item_out">

=item out (string)

Print the given I<string>. Rarely needed, since normally all HTML is just
placed in the component body and output implicitly. C<$m-E<gt>out> is useful
if you need to output something in the middle of a Perl block.

C<$m-E<gt>out> should be used instead of C<print> or C<$r-E<gt>print>,
since C<$m-E<gt>out> may be redirected or buffered depending on the
current state of the interpreter.

=for html <a name="item_scomp">

=item scomp (comp, args...)

Like C<$m-E<gt>comp>, but returns the component output as a string
instead of printing it. (Think sprintf versus printf.) The
component's return value is discarded.

=for html <a name="item_top_args">

=item top_args

Returns the arguments originally passed to the top level component
(see L<Request/top_comp> for definition).  When called in scalar
context, a hash reference is returned. When called in list context, a
list of arguments (which may be assigned to a hash) is returned.

=for html <a name="item_top_comp">

=item top_comp

Returns the component originally called in the request. Without
autohandlers, this is the same as the first component executed.  With
autohandlers, this is the component at the end of the
C<$m-E<gt>call_next> chain.

=back

=head1 APACHE-ONLY METHODS

These additional methods are available when running Mason with mod_perl
and the ApacheHandler.

=over

=for html <a name="item_ah">

=item ah

Returns the ApacheHandler object associated with this request.

=for html <a name="item_apache_req">

=item apache_req

Returns the Apache request object.  This is also available in the
global $r.

=for html <a name="item_cgi_object">

=item cgi_object

Returns the CGI object used to parse any CGI parameters submitted to
the component, assuming that you have not changed the default value of
the ApacheHandler C<args_method> parameter.  If you are using the
'mod_perl' args method, then calling this method is a fatal error.
See the L<HTML::Mason::ApacheHandler> documentation for more details.

=back

=head1 AUTHORS

Jonathan Swartz <swartz@pobox.com>, Dave Rolsky <autarch@urth.org>, Ken Williams <ken@mathforum.org>

=head1 SEE ALSO

L<HTML::Mason>,
L<HTML::Mason::Devel>,
L<HTML::Mason::Component>
