# Copyright (c) 1998-2000 by Jonathan Swartz. All rights reserved.
# This program is free software; you can redistribute it and/or modify it
# under the same terms as Perl itself.

package HTML::Mason::Request;

use strict;

use Carp;

use File::Spec;
use HTML::Mason::Tools qw(read_file);
use HTML::Mason::Utils;

use vars qw($REQ $REQ_DEPTH %REQ_DEPTHS);

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
	error_flag => undef,
	out_buffer => '',
	stack_array => undef,
	wrapper_chain => undef,
	wrapper_index => undef
    };
    my (%options) = @_;
    while (my ($key,$value) = each(%options)) {
	if (exists($fields{$key})) {
	    $self->{$key} = $value;
	} else {
	    die "HTML::Mason::Request::new: invalid option '$key'\n";
	}
    }
    bless $self, $class;
    my $interp = $self->{interp} or die "HTML::Mason::Request::new: must specify interp";
    ++$self->{count};
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

    # Allow scalar or code reference as argument to out_method.
    if (ref(my $ref = $self->{out_method}) eq 'SCALAR') {
	$self->{out_method} = sub { $$ref .= $_[0] if defined($_[0]) };
    }

    # Initialize other properties
    $self->{stack_array} = [];
}

sub _reinitialize {
    my ($self) = @_;
    $self->_initialize;
    foreach my $field (qw(aborted aborted_value autohandler_next declined dhandler_arg error_flag)) {
	$self->{$field} = undef;
    }
}

sub exec {
    my ($self, $comp, @args) = @_;
    my $interp = $self->interp;
    
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
	if (!($comp = $interp->load($path))) {
	    if (defined($interp->dhandler_name) and $comp = $interp->find_comp_upwards($path,$interp->dhandler_name)) {
		my $parent_path = $comp->dir_path;
		($self->{dhandler_arg} = $path) =~ s{^$parent_path/?}{};
	    }
	}
	unless ($comp) {
	    $self->{error_code} = 'top_not_found';
	    die "could not find component for initial path '$path'\n";
	}
    } elsif ( UNIVERSAL::isa( $comp, 'HTML::Mason::Component' ) ) {
	die "exec: first argument ($comp) must be an absolute component path or a component object";
    }

    # This label is for declined requests.
    retry:
    
    # Build wrapper chain and index.
    my $first_comp;
    {my @wrapper_chain = ($comp);
     for (my $parent = $comp->parent; $parent; $parent = $parent->parent) {
	 unshift(@wrapper_chain,$parent);
	 die "inheritance chain length > 32 (infinite inheritance loop?)" if (@wrapper_chain > 32);
     }
     $first_comp = $wrapper_chain[0];
     $self->{wrapper_chain} = [@wrapper_chain];
     $self->{wrapper_index} = {map(($wrapper_chain[$_]->path => $_),(0..$#wrapper_chain))}; }

    # Fill top_level slots for introspection.
    $self->{top_comp} = $first_comp;
    $self->{top_args} = \@args;

    # Call the first component.
    my ($result, @result);
    if (wantarray) {
	local $SIG{'__DIE__'} = $interp->die_handler if $interp->die_handler;
	@result = eval {$self->comp({base_comp=>$comp}, $first_comp, @args)};
    } else {
	local $SIG{'__DIE__'} = $interp->die_handler if $interp->die_handler;
	$result = eval {$self->comp({base_comp=>$comp}, $first_comp, @args)};
    }
    my $err = $@;

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
	if ($interp->die_handler_overridden) {
	    # the default $SIG{__DIE__} was overridden so let's not
	    # mess with the error message
	    die $err;
	} else {
	    my $i = index($err,'HTML::Mason::Interp::exec');
	    $err = substr($err,0,$i) if $i!=-1;
	    $err =~ s/^\s*(HTML::Mason::Commands::__ANON__|HTML::Mason::Request::call).*\n//gm;
	    # Salvage what was left in the request stack for backtrace information
	    if (@{$self->{stack_array}}) {
		my @titles = map($_->{comp}->title,@{$self->{stack_array}});
		my $errmsg = "error while executing $titles[-1]:\n";
		$errmsg .= $err."\n";
		$errmsg .= "backtrace: " . join(" <= ",reverse(@titles)) . "\n" if @titles > 1;
		die ($errmsg);
	    } else {
		die ($err);
	    }
	}
    }

    # Flush output buffer for batch mode.
    $self->flush_buffer if $self->out_mode eq 'batch';

    # Purge code cache if necessary. We do this at the end so as not
    # to affect the response of the request as much.
    $interp->purge_code_cache;
    
    # Handle abort.
    return $self->{aborted_value} if ($self->{aborted});

    return wantarray ? @result : $result;
}

#
# Abort out of current execution.
#
sub abort
{
    my ($self) = @_;
    $self->{aborted} = 1;
    $self->{aborted_value} = $_[1] || $self->{aborted_value} || undef;
    croak "abort() called";
}

sub cache
{
    my ($self,%options) = @_;
    my $interp = $self->interp;
    return undef unless $interp->use_data_cache;
    $options{action} = $options{action} || 'retrieve';
    
    my $comp = $self->current_comp;
    $options{cache_file} = $comp->cache_file
	or die "no cache file for component ".$comp->title;
    if ($options{keep_in_memory}) {
	$options{memory_cache} = $interp->{data_cache_store};
	delete($options{keep_in_memory});
    }

    if ($options{action} eq 'retrieve' or $options{action} eq 'store') {
	my $results = HTML::Mason::Utils::access_data_cache(%options);
	if ($options{action} eq 'retrieve') {
	    $interp->write_system_log('CACHE_READ',$comp->title,$options{key} || 'main',
				      defined $results ? 1 : 0);
	} elsif ($options{action} eq 'store') {
	    $interp->write_system_log('CACHE_WRITE',$comp->title,$options{key} || 'main');
	}
	return $results;
    } else {
	return HTML::Mason::Utils::access_data_cache(%options);
    }

}

sub cache_self
{
    my ($self,%options) = @_;

    my $interp = $self->interp;
    return undef unless $interp->use_data_cache;
    return undef if $self->top_stack->{in_cache_self_flag};
    my (%retrieve_options,%store_options);
    foreach (qw(key expire_if keep_in_memory busy_lock)) {
	$retrieve_options{$_} = $options{$_} if (exists($options{$_}));
    }
    foreach (qw(key expire_at expire_next expire_in)) {
	$store_options{$_} = $options{$_} if (exists($options{$_}));
    }
    my $result = $self->cache(action=>'retrieve',%retrieve_options);
    my ($output,$retval);
    
    #
    # See if our result is cached. Older versions of Mason only stored
    # output, so if we get something that isn't a two-item listref,
    # recompute anyway.
    #
    unless (defined($result) and ref($result) eq 'ARRAY' and @$result == 2) {
	#
	# Reinvoke the component. Collect output ($output) and return
	# value ($retval).
	#
	my $lref = $self->top_stack;
	my %save_locals = %$lref;
	$lref->{sink} = $self->_new_sink(\$output);
	$lref->{in_cache_self_flag} = 1;

	$retval = $lref->{comp}->run( @{ $lref->{args} } );
	$self->top_stack({%save_locals});

	#
	# Store output and return value as a two-item listref.
	#
	$self->cache(action=>'store',value=>[$output,$retval],%store_options);
    } else {
	($output,$retval) = @$result;
    }
    $self->out($output);

    #
    # Return the component return value in case the caller is interested,
    # followed by 1 indicating the cache retrieval success.
    #
    return ($retval,1);
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
    my $comp = $self->fetch_next or die "call_next: no next component to invoke";
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
    my @caller_stack = reverse(@{$self->stack});
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
    my @caller_stack = reverse(@{$self->stack});
    if (defined($index)) {
	if (wantarray) {
	    return @{$caller_stack[$index]->{args}};
	} else {
	    my %h = @{$caller_stack[$index]->{args}};
	    return \%h;
	}
    } else {
	die "caller_args expects stack level as argument";
    }
}

sub call_self
{
    my ($self,$cref,$rref) = @_;
    return 0 if $self->top_stack->{in_call_self_flag};

    #
    # Reinvoke the component with in_call_self_flag=1. Collect
    # output and return value in references provided.
    #
    my $content;
    my $lref = $self->top_stack;
    my %save_locals = %$lref;
    $lref->{sink} = $self->_new_sink(\$content);
    $lref->{in_call_self_flag} = 1;


    if (ref($rref) eq 'SCALAR') {
	$$rref = $lref->{comp}->run( @{ $lref->{args} } );
    } elsif (ref($rref) eq 'ARRAY') {
	@$rref = $lref->{comp}->run( @{ $lref->{args} } );
    } else {
	$lref->{comp}->run( @{ $lref->{args} } );
    }
    $self->top_stack({%save_locals});
    $$cref = $content if ref($cref) eq 'SCALAR';
    undef $content;

    return 1;
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
    croak "decline() called (and not caught)";
}

#
# Return the current number of stack levels. 1 means top level, 0
# means that no component has been called yet.
#
sub depth
{
    my ($self) = @_;
    return ($REQ eq $self) ? $REQ_DEPTH : ($REQ_DEPTHS{$self} || 0);
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
    die "fetch_comp: requires path as first argument" unless defined($path);

    #
    # Handle paths SELF and PARENT
    #
    if ($path eq 'SELF') {
	return $self->base_comp;
    }
    if ($path eq 'PARENT') {
	return $self->current_comp->parent || die "PARENT designator used from component with no parent";
    }

    #
    # Handle paths of the form comp_path:method_name
    #
    if (index($path,':') != -1) {
	my $method_comp;
	my ($owner_path,$method_name) = split(':',$path,2);
	my $owner_comp = $self->fetch_comp($owner_path)
	    or die "could not find component for path '$owner_path'\n";
	$owner_comp->_locate_inherited('methods',$method_name,\$method_comp)
	    or die "no method '$method_name' for component ".$owner_comp->title;
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
    die "fetch_next: cannot find next component in chain" unless defined($index);
    return $self->{wrapper_chain}->[$index+1];
}

#
# Fetch remaining components in wrapper chain.
#
sub fetch_next_all {
    my ($self) = @_;
    my $index = $self->_fetch_next_helper;
    die "fetch_next_all: cannot find next component in chain" unless defined($index);
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

sub out
{
    my $self = shift;
    $self->current_sink->(@_);
}

sub time
{
    my ($self) = @_;
    my $time = $self->interp->current_time;
    $time = time() if $time eq 'real';
    return $time;
}

# Return the current stack as a list ref.
sub stack
{
    my ($self) = @_;
    my $stack = $self->{stack_array};
    my $depth = $self->depth;
    splice(@$stack,$depth) unless (@$stack == $depth);
    return $stack;
}

# Set or retrieve the hashref at the top of the stack.
sub top_stack {
    my ($self,$href) = @_;
    $self->{stack_array}->[$self->depth-1] = $href if defined($href);
    return $self->{stack_array}->[$self->depth-1];
}

#
# Return the parser associated with this request (by way of interp).
#
sub parser
{
    return shift->interp->parser;
}

#
# Execute the next component in this request. comp() sets up proper
# dynamically scoped variables and invokes comp1() to do the work.
#
sub comp {
    my $self = shift(@_);

    if (defined($REQ) and $REQ eq $self) {
	local $REQ_DEPTH = $REQ_DEPTH;
	$self->comp1(@_);
    } else {
	local %REQ_DEPTHS = %REQ_DEPTHS;
	$REQ_DEPTHS{$REQ} = $REQ_DEPTH if defined($REQ);
	local $REQ = $self;
	local $REQ_DEPTH = $REQ_DEPTHS{$self} || 0;
	$self->comp1(@_);
    }
}
sub comp1 {
    my $self = shift;

    # Get modifiers: optional hash reference passed in as first argument.
    my %mods = (ref($_[0]) eq 'HASH') ? %{shift()} : ();

    my ($comp,@args) = @_;
    my $interp = $self->{interp};
    my $depth = $REQ_DEPTH;
    die "comp: requires path or component as first argument" unless defined($comp);

    #
    # $comp can be an absolute path or component object.  If a path,
    # load into object.
    #
    if (!ref($comp)) {
	my $path = $comp;
	$comp = $self->fetch_comp($path) or die "could not find component for path '$path'\n";
    }

    #
    # $m is a dynamically scoped global containing this
    # request. This needs to be defined in the HTML::Mason::Commands
    # package, as well as the component package if that is different.
    #
    local $HTML::Mason::Commands::m = $self;
    $interp->set_global('m'=>$self) if ($interp->parser->in_package ne 'HTML::Mason::Commands');

    #
    # Determine sink (where output is going).
    #
    # Look for STORE and scalar reference passed as last two arguments. This is deprecated
    # and will go away eventually.
    #
    my $sink;
    if (@args >= 2 and $args[-2] eq 'STORE' and ref($args[-1]) eq 'SCALAR' and @args % 2 == 0) {
	my $store = $args[-1];
	$$store = '';
	$sink = $self->_new_sink($store);
	splice(@args,-2);
    } elsif ($depth>0) {
	$sink = $self->top_stack->{sink};
    } elsif ($self->out_mode eq 'batch') {
	$sink = $self->_new_sink(\($self->{out_buffer}));
    } else {
	$sink = $self->out_method;
    }

    #
    # Determine base_comp (base component for method and attribute
    # references). Stays the same unless passed in as a modifier.
    #
    my $base_comp = exists($mods{base_comp}) ? $mods{base_comp} : $self->top_stack->{base_comp};
    
    #
    # Check for maximum recursion.
    #
    die "$depth levels deep in component stack (infinite recursive call?)\n" if ($depth >= $interp->max_recurse);

    # Push new frame onto stack and increment (localized) depth.
    my $stack = $self->stack;
    my $frame = {'comp'=>$comp,args=>[@args],sink=>$sink,base_comp=>$base_comp};
    push(@$stack,$frame);
    $REQ_DEPTH++;

    # Call start_comp hooks.
    $self->call_hooks('start_comp');

    #
    # Finally, call component subroutine.
    #
    my ($result, @result);
    if (wantarray) {
	@result = $comp->run(@args);
    } elsif (defined wantarray) {
	$result = $comp->run(@args);
    } else {
	$comp->run(@args);
    }

    #
    # Call end_comp hooks.
    #
    $self->call_hooks('end_comp');
    
    #
    # Pop stack and return.
    #
    pop(@$stack);
    return wantarray ? @result : $result;  # Will return undef in void context (correct)
}

#
# Like comp, but return component output.
#
sub scomp {
    my $self = shift;

    # Set a new top-level sink.
    my $store = '';
    local $self->top_stack->{sink} = $self->_new_sink(\$store);
    $self->comp(@_);
    return $store;
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
	die "suppress_hook: must specify $_\n" if !exists($args{$_});
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
	die "unsuppress_hook: must specify $_\n" if !exists($args{$_});
    }
    my $code = $self->interp->hooks->{$args{type}}->{$args{name}};
    $self->{"hooks_$args{type}"} = [grep($_ ne $code,@{$self->{"hooks_$args{type}"}})];
    push(@{$self->{"hooks_$args{type}"}},$code);
}

#
# Returns a new closure that will write its arguments to the given scalar ref.
#
sub _new_sink {
    shift;  # Don't need $self
    my $out_ref = shift;
    return sub { for (@_) { $$out_ref .= $_ if defined } };
}

sub clear_buffer
{
    my ($self) = @_;
    $self->{out_buffer} = '';
}

sub flush_buffer
{
    my ($self, $content) = @_;
    $self->out_method->($self->{out_buffer});
    $self->{out_buffer} = '';    
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
# Accessor methods for top of stack elements.
#
sub current_comp { return $_[0]->top_stack->{'comp'} }
sub current_args { return $_[0]->top_stack->{args} }
sub current_sink { return $_[0]->top_stack->{sink} }
sub base_comp { return $_[0]->top_stack->{base_comp} }

1;
