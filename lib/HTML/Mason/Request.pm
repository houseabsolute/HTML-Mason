# Copyright (c) 1998-2000 by Jonathan Swartz. All rights reserved.
# This program is free software; you can redistribute it and/or modify it
# under the same terms as Perl itself.

package HTML::Mason::Request;

use strict;

use File::Spec;
use HTML::Mason::Tools qw(read_file compress_path);
use HTML::Mason::Utils;
use HTML::Mason::Buffer;

use Params::Validate qw(:all);
Params::Validate::set_options( on_fail => sub { HTML::Mason::Exception::Params->throw( error => join '', @_ ) } );

use HTML::Mason::MethodMaker
    ( read_only => [ qw( aborted
			 aborted_value
			 count
			 declined
			 error_code
			 interp
			 top_comp ) ],

      read_write => [ qw( out_method
			  out_mode ) ],
    );

my %fields =
    (aborted => undef,
     aborted_value => undef,
     count => 0,
     declined => undef,
     error_code => undef,
     interp => undef,
     out_method => undef,
     out_mode => undef
     );

sub new
{
    my $class = shift;
    my $self = {
	%fields,
	dhandler_arg => undef,
	buffer_stack => undef,
	stack => undef,
	wrapper_chain => undef,
	wrapper_index => undef
    };

    validate( @_,
	      { interp => { isa => 'HTML::Mason::Interp' },
		out_method => { type => SCALARREF | CODEREF, optional => 1 },
		out_mode => { type => SCALAR, optional => 1 },
	      }
	    );
    my (%options) = @_;

    foreach ( qw( out_method out_mode ) ) {
	$self->{$_} = $options{$_} if exists $options{$_};
    }

    bless $self, $class;
    $self->{interp} = $options{interp};
    $self->{count} = ++$self->{interp}{request_count};
    $self->_initialize;
    return $self;
}

sub _initialize {
    my ($self) = @_;
    my $interp = $self->interp;

    # Initialize hooks arrays for fast access
    while (my ($type,$href) = each(%{$interp->{hooks}})) {
	$self->{"hooks_$type"} = [values(%$href)] if (%$href);
    }

    # Inherit some properties from interp if not otherwise specified
    $self->{out_method} = $interp->out_method if !defined($self->{out_method});
    $self->{out_mode} = $interp->out_mode if !defined($self->{out_mode});

    # Initialize other properties
    $self->{buffer_stack} = [];
    $self->{stack} = [];
}

sub _reinitialize {
    my ($self) = @_;
    $self->_initialize;
    foreach my $field (qw(aborted aborted_value autohandler_next declined dhandler_arg)) {
	$self->{$field} = undef;
    }
}

sub exec {
    my ($self, $comp, @args) = @_;
    my $interp = $self->interp;

    # Error may occur in several places in function.
    my $err;
    
    # Check if reload file has changed.
    $interp->check_reload_file if ($interp->use_reload_file);

    # Purge code cache if necessary. Generally happens at the end of
    # the component; this is just in case many errors are occurring.
    $interp->purge_code_cache;
    
    # $comp can be an absolute path or component object.  If a path,
    # load into object. If not found, check for dhandler.
    my ($path, $orig_path);
    if (!ref($comp) && substr($comp,0,1) eq '/') {
	$orig_path = $path = $comp;
	{
	    local $SIG{'__DIE__'} = $interp->die_handler if $interp->die_handler;
	    eval { $comp = $interp->load($path) };
	    $err = $@;
	    goto error if ($err);
	}
	unless ($comp) {
	    if (defined($interp->dhandler_name) and $comp = $interp->find_comp_upwards($path,$interp->dhandler_name)) {
		my $parent_path = $comp->dir_path;
		($self->{dhandler_arg} = $path) =~ s{^$parent_path/?}{};
	    }
	}
	unless ($comp) {
	    $self->{error_code} = 'top_not_found';
	    HTML::Mason::Exception->throw( error => "could not find component for initial path '$path'\n" );
	}
    } elsif ( ! UNIVERSAL::isa( $comp, 'HTML::Mason::Component' ) ) {
	HTML::Mason::Exception::Params->throw( error => "exec: first argument ($comp) must be an absolute component path or a component object" );
    }

    # create base buffer
    $self->push_buffer_stack( HTML::Mason::Buffer->new( sink => $self->out_method, mode => $self->out_mode ) );

    # This label is for declined requests.
    retry:
    # Build wrapper chain and index.
    my $first_comp;
    {my @wrapper_chain = ($comp);
     for (my $parent = $comp->parent; $parent; $parent = $parent->parent) {
	 unshift(@wrapper_chain,$parent);
	 HTML::Mason::Exception->throw( error => "inheritance chain length > 32 (infinite inheritance loop?)" )
	     if (@wrapper_chain > 32);
     }
     $first_comp = $wrapper_chain[0];
     $self->{wrapper_chain} = [@wrapper_chain];
     $self->{wrapper_index} = {map(($wrapper_chain[$_]->path => $_),(0..$#wrapper_chain))}; }

    # Fill top_level slots for introspection.
    $self->{top_comp} = $comp;
    $self->{top_args} = \@args;

    # Call the first component.
    my ($result, @result);
    {
	local $SIG{'__DIE__'} = $interp->die_handler if $interp->die_handler;
	if (wantarray) {
	    @result = eval {$self->comp({base_comp=>$comp}, $first_comp, @args)};
	} else {
	    $result = eval {$self->comp({base_comp=>$comp}, $first_comp, @args)};
	}
    }
    $err = $@;

    # If declined, try to find the next dhandler.
    if ($self->declined and $path) {
	$path =~ s/\/[^\/]+$// if defined($self->{dhandler_arg});
	if (defined($interp->dhandler_name) and my $next_comp = $interp->find_comp_upwards($path,$interp->dhandler_name)) {
	    $comp = $next_comp;
	    my $parent = $comp->dir_path;
	    $self->_reinitialize;
	    ($self->{dhandler_arg} = $orig_path) =~ s{^$parent/}{};
	    goto retry;
	}
    }

    # If an error occurred...
    if ($err and !$self->aborted) {
	goto error;
    }

    # Flush output buffer for batch mode.
    $self->flush_buffer if $self->out_mode eq 'batch';
    $self->pop_buffer_stack;

    # Purge code cache if necessary. We do this at the end so as not
    # to affect the response of the request as much.
    $interp->purge_code_cache;
    
    # Handle abort.
    return $self->{aborted_value} if ($self->{aborted});

    return wantarray ? @result : $result;

    error:
    $self->pop_buffer_stack;
    # don't mess with error message if default $SIG{__DIE__} was overridden
    if ($interp->die_handler_overridden) {
	die $err;
    } else {
	$err = $self->{error_clean} if $self->{error_clean};
	if ($self->{error_backtrace}) {
	    my $title = $self->{error_backtrace}->[0]->title;
	    $err = "error while executing $title:\n$err";
	}
	HTML::Mason::Exception->throw( error => $err );
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
    HTML::Mason::Exception::Abort->throw( message => $self->{aborted_value} );
}

#
# Return a new cache object specific to this component.
#
sub cache
{
    my ($self, %options) = @_;

    if (%{$self->interp->data_cache_defaults}) {
	%options = (%{$self->interp->data_cache_defaults},%options);
    }
    $options{namespace}   ||= compress_path($self->current_comp->fq_path);
    $options{cache_root}  ||= File::Spec->catdir($self->interp->data_dir,"cache");
    $options{username}      = "mason";

    my $cache_class = 'Cache::FileCache';
    if ($options{cache_class}) {
	$cache_class = $options{cache_class};
	$cache_class = "Cache::$cache_class" unless $cache_class =~ /::/;
	delete($options{cache_class});
    }

    my $cache = $cache_class->new (\%options)
	or HTML::Mason::Exception->throw( error => "could not create cache object" );

    return $cache;
}

#
# Old synonym for comp.
#
sub call { shift->comp(@_) }

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
	or HTML::Mason::Exception->throw( error => "call_next: no next component to invoke" );
    my @args = (@{$self->current_args},@extra_args);
    return $self->comp($comp, @args);
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
    my ($self,$index) = @_;
    my @caller_stack = reverse $self->stack;
    if (defined($index)) {
	return $caller_stack[$index]->{comp};
    } else {
	return map($_->{comp},@caller_stack);
    }
}

#
# Return a specified argument list from the stack.
#
sub caller_args
{
    my ($self,$index) = @_;
    my @caller_stack = reverse $self->stack;
    if (defined($index)) {
	if (wantarray) {
	    return @{$caller_stack[$index]->{args}};
	} else {
	    my %h = @{$caller_stack[$index]->{args}};
	    return \%h;
	}
    } else {
	HTML::Mason::Exception::Params->throw( error => "caller_args expects stack level as argument" );
    }
}

sub comp_exists
{
    my ($self,$path) = @_;
    return $self->interp->lookup($self->interp->process_comp_path($path,$self->current_comp->dir_path)) ? 1 : 0;
}

sub decline
{
    my ($self) = @_;
    $self->{declined} = 1;
    HTLM::Mason::Exception->throw( error => "decline() called (and not caught)" );
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
    HTML::Mason::Exception::Params->throw( error => "fetch_comp: requires path as first argument" ) unless defined($path);

    #
    # Handle paths SELF and PARENT
    #
    if ($path eq 'SELF') {
	return $self->base_comp;
    }
    if ($path eq 'PARENT') {
	return $self->current_comp->parent
	    or HTML::Mason::Exception->throw( error =>"PARENT designator used from component with no parent" );
    }

    #
    # Handle paths of the form comp_path:method_name
    #
    if (index($path,':') != -1) {
	my $method_comp;
	my ($owner_path,$method_name) = split(':',$path,2);
	my $owner_comp = $self->fetch_comp($owner_path)
	    or HTML::Mason::Exception->throw( error => "could not find component for path '$owner_path'\n" );
	$owner_comp->_locate_inherited('methods',$method_name,\$method_comp)
	    or HTML::Mason::Exception->throw( error => "no method '$method_name' for component " . $owner_comp->title );
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
    $path = $self->interp->process_comp_path($path,$self->current_comp->dir_path);
    return $self->interp->load($path);
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
    HTML::Mason::Exception->throw( error => "fetch_next: cannot find next component in chain" )
	unless defined($index);
    return $self->{wrapper_chain}->[$index+1];
}

#
# Fetch remaining components in wrapper chain.
#
sub fetch_next_all {
    my ($self) = @_;
    my $index = $self->_fetch_next_helper;
    HTML::Mason::Exception->throw( error => "fetch_next_all: cannot find next component in chain" )
	unless defined($index);
    my @wc = @{$self->{wrapper_chain}};
    return @wc[($index+1)..$#wc];
}

sub file
{
    my ($self,$file) = @_;
    my $interp = $self->interp;
    unless ( File::Spec->file_name_is_absolute($file) ) {
	if ($interp->static_file_root) {
	    $file = File::Spec->catfile( $interp->static_file_root, $file );
	} elsif ($self->current_comp->is_file_based) {
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

sub file_root
{
    return shift->interp->static_file_root;
}

sub print
{
    my $self = shift;
    $self->top_buffer->receive(@_);
}

*out = \&print;

sub time
{
    my ($self) = @_;
    my $time = $self->interp->current_time;
    $time = time() if $time eq 'real';
    return $time;
}

#
# Execute the next component in this request.
#
sub comp {
    my $self = shift;

    # Clear error backtrace and message, in case we had an error which
    # was caught by an eval.
    undef $self->{error_backtrace};
    undef $self->{error_clean};
    
    # Get modifiers: optional hash reference passed in as first argument.
    # merge multiple hash references to simplify user and internal usage.
    my %mods = ();
    %mods = (%{shift()},%mods) while ref($_[0]) eq 'HASH';

    my ($comp,@args) = @_;
    my $interp = $self->interp;
    my $depth = $self->depth;
    HTML::Mason::Exception::Params->throw( error => "comp: requires path or component as first argument" )
	unless defined($comp);

    #
    # $comp can be an absolute path or component object.  If a path,
    # load into object.
    #
    if (!ref($comp)) {
	my $path = $comp;
	$comp = $self->fetch_comp($path)
	    or HTML::Mason::Exception->throw( error => "could not find component for path '$path'\n" );
    }

    #
    # Check for maximum recursion.
    #
    HTML::Mason::Exception->throw( error => "$depth levels deep in component stack (infinite recursive call?)\n" )
        if ($depth >= $interp->max_recurse);

    #
    # $m is a dynamically scoped global containing this
    # request. This needs to be defined in the HTML::Mason::Commands
    # package, as well as the component package if that is different.
    #
    local $HTML::Mason::Commands::m = $self;
    $interp->set_global('m'=>$self) if ($interp->compiler->in_package ne 'HTML::Mason::Commands');

    #
    # Determine base_comp (base component for method and attribute
    # references). Stays the same unless passed in as a modifier.
    #
    my $base_comp = exists($mods{base_comp}) ? $mods{base_comp} : $self->base_comp;

    # Push new frame onto stack.
    $self->push_stack( {comp => $comp,
			args => [@args],
			base_comp => $base_comp,
		       } );

    if ($mods{store}) {
        $self->push_buffer_stack($self->top_buffer->new_child( mode => 'batch', ignore_flush => 1 ));
    } else {
        $self->push_buffer_stack($self->top_buffer->new_child);
    }

    # Call start_comp hooks.
    $self->call_hooks('start_comp');

    #
    # Finally, call component subroutine.
    #
    my ($result, @result);
    if (wantarray) {
	@result = eval { $comp->run(@args) };
    } elsif (defined wantarray) {
	$result = eval { $comp->run(@args) };
    } else {
	eval { $comp->run(@args) };
    }

    #
    # If an error occurred, pop stack and pass error down to next level.
    # Put current component stack in error backtrace unless this has already
    # been done higher up.
    #
    if (my $err = $@) {
	##
	## any unflushed output is at $self->top_buffer->output
	##
	$self->{error_backtrace} ||= [reverse(map($_->{'comp'}, $self->stack))];
	$self->{error_clean}     ||= $err;
	unless ($self->interp->die_handler_overridden) {
	    $err .= "\n" if $err !~ /\n$/;
	}
	$self->pop_stack;
	UNIVERSAL::can($err, 'rethrow') ? $err->rethrow : HTML::Mason::Exception->throw( error => $err );
    }

    #
    # Call end_comp hooks.
    #
    $self->call_hooks('end_comp');

    if ($mods{store}) {
	${$mods{store}} = $self->top_buffer->output;
    } else {
        $self->top_buffer->flush;
    }
    $self->pop_stack;

    return wantarray ? @result : $result;  # Will return undef in void context (correct)
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

sub process_comp_path
{
    my ($self) = shift;
    return $self->interp->process_comp_path(@_,$self->current_comp->dir_path);
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
	HTML::Mason::Exception::Params->throw( error => "suppress_hook: must specify $_\n" )
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
	HTML::Mason::Exception::Params->throw( error => "unsuppress_hook: must specify $_\n" )
	    unless exists($args{$_});
    }
    my $code = $self->interp->hooks->{$args{type}}->{$args{name}};
    $self->{"hooks_$args{type}"} = [grep($_ ne $code,@{$self->{"hooks_$args{type}"}})];
    push(@{$self->{"hooks_$args{type}"}},$code);
}

sub clear_buffer
{
    my $self = shift;
    for (reverse $self->buffer_stack) {
	$_->clear;
    }
}

sub flush_buffer
{
    my $self = shift;
    for (reverse $self->buffer_stack) {
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

# Return the current stack as a list ref.
sub stack {
    my ($self) = @_;
    return @{ $self->{stack} };
}

# Set or retrieve the hashref at the top of the stack.
sub top_stack {
    my ($self,$href) = @_;
    HTML::Mason::Exception->throw( error => "top_stack: nothing on component stack" )
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
    pop @{ $self->{stack} };
    $self->pop_buffer_stack;
}

sub push_buffer_stack {
    my ($self, $buffer) = @_;
    push @{ $self->{buffer_stack} }, $buffer;
}

sub pop_buffer_stack {
    my ($self) = @_;
    my $buffer = pop @{ $self->{buffer_stack} };
    $buffer->destroy;
}

sub buffer_stack {
    my ($self) = @_;
    return @{ $self->{buffer_stack} };
}


#
# Accessor methods for top of stack elements.
#
sub current_comp { return $_[0]->top_stack->{comp} }
sub current_args { return $_[0]->top_stack->{args} }
sub current_sink { return $_[0]->{buffer_stack}->[-1]->sink }
sub top_buffer { return $_[0]->{buffer_stack}->[-1] }
sub base_comp { return $_[0]->top_stack->{base_comp} }


1;
