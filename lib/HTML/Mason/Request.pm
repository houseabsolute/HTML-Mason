# Copyright (c) 1998-99 by Jonathan Swartz. All rights reserved.
# This program is free software; you can redistribute it and/or modify it
# under the same terms as Perl itself.

package HTML::Mason::Request;
require 5.004;
require Exporter;
@ISA = qw(Exporter);
@EXPORT = qw();
@EXPORT_OK = qw();

use HTML::Mason::Tools qw(is_absolute_path read_file);

use Carp;
use strict;
use vars qw($REQ $REQ_DEPTH %REQ_DEPTHS);
my @_used = ($HTML::Mason::CODEREF_NAME,$::opt_P,$HTML::Mason::Commands::m);

my %fields =
    (aborted => undef,
     aborted_value => undef,
     count => 0,
     declined => undef,
     interp => undef,
     out_method => undef,
     out_mode => undef,
     );
# Create read-only accessor routines
foreach my $f (keys %fields) {
    no strict 'refs';
    *{$f} = sub {my $s=shift; die "cannot modify request field $f" if @_; return $s->{$f}};
}

sub new
{
    my $class = shift;
    my $self = {
	%fields,
	autohandler_next => undef,
	dhandler_arg => undef,
	error_flag => undef,
	inherit_chain => undef,
	out_buffer => '',
	stack_array => undef,
	wrapper_chain => undef
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
    $self->{count} = ++($interp->{request_count});
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
    $interp->check_reload_file if ($interp->{use_reload_file});

    # Purge code cache if necessary. Generally happens at the end of
    # the component; this is just in case many errors are occurring.
    $interp->purge_code_cache;
    
    # $comp can be an absolute path or component object.  If a path,
    # load into object. If not found, check for dhandler.
    my ($path, $orig_path);
    if (!ref($comp) && substr($comp,0,1) eq '/') {
	$orig_path = $path = $comp;
	if (!($comp = $interp->load($path))) {
	    if (defined($interp->{dhandler_name}) and $comp = $interp->find_comp_upwards($path,$interp->{dhandler_name})) {
		my $parent = $comp->dir_path;
		($self->{dhandler_arg} = $path) =~ s{^$parent/}{};
	    }
	}
	die "could not find component for path '$path'\n" if !$comp;
    } elsif (ref($comp) !~ /Component/) {
	die "exec: first argument ($comp) must be an absolute component path or a component object";
    }

    # This label is for declined requests.
    retry:
    
    # Build inheritance chain.
    my @inherit_chain = ($comp);
    while (1) {
	my $inherit;
	my $dir_path = $comp->dir_path;
	if (exists($comp->flags->{inherit})) {
	    my $inherit_path = $comp->flags->{inherit};
	    if ($inherit_path =~ /\S/) {
		$inherit = $interp->load($self->process_comp_path($inherit_path,$dir_path));
	    }
	} elsif (defined($interp->{autohandler_name})) {
	    # Look for autohandler, making sure that autohandler doesn't inherit
	    # from itself.
	    my $comp_is_autohandler = ($comp->name eq $interp->{autohandler_name});
	    if ($interp->{allow_recursive_autohandlers}) {
		if ($comp_is_autohandler) {
		    last if ($dir_path eq '/');
		    $dir_path =~ s/\/[^\/]*$//;
		    $dir_path = '/' if !$dir_path;
		}
		$inherit = $interp->find_comp_upwards($dir_path,$interp->{autohandler_name});
	    } else {
		$inherit = $interp->load("$dir_path/".$interp->{autohandler_name}) unless $comp_is_autohandler;
	    }
	}
	if (defined($inherit)) {
	    push(@inherit_chain,$inherit);
	    $comp = $inherit;
	    die "assert error: inherit chain length > 32 (infinite loop?)" if (@inherit_chain > 32);
	} else {
	    last;
	}
    }
    $self->{inherit_chain} = \@inherit_chain;
    $self->{wrapper_chain} = [reverse(@inherit_chain)];
    shift(@{$self->{wrapper_chain}});

    # Call the first component.
    my ($result, @result);
    if (wantarray) {
	local $SIG{'__DIE__'} = sub { confess($_[0]) };
	@result = eval {$self->comp($comp, @args)};
    } else {
	local $SIG{'__DIE__'} = sub { confess($_[0]) };
	$result = eval {$self->comp($comp, @args)};
    }
    my $err = $@;

    # If declined, try to find the next dhandler.
    if ($self->declined and $path) {
	$path =~ s/\/[^\/]+$// if defined($self->{dhandler_arg});
	if (defined($interp->{dhandler_name}) and my $next_comp = $interp->find_comp_upwards($path,$interp->{dhandler_name})) {
	    $comp = $next_comp;
	    my $parent = $comp->dir_path;
	    $self->_reinitialize;
	    ($self->{dhandler_arg} = $orig_path) =~ s{^$parent/}{};
	    goto retry;
	}
    }

    # If an error occurred...
    if ($err and !$self->aborted) {
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
    $options{key} = $options{key} || 'main';
    
    my $comp = $self->current_comp;
    $options{cache_file} = $comp->cache_file
	or die "no cache file for component ".$comp->title;
    if ($options{keep_in_memory}) {
	$options{memory_cache} = $interp->{data_cache_store};
	delete($options{keep_in_memory});
    }
    
    my $results = HTML::Mason::Utils::access_data_cache(%options);
    if ($options{action} eq 'retrieve') {
	$interp->write_system_log('CACHE_READ',$comp->title,$options{key},
				  defined $results ? 1 : 0);
    } elsif ($options{action} eq 'store') {
	$interp->write_system_log('CACHE_WRITE',$comp->title,$options{key});
    }
    return $results;
}

sub cache_self
{
    my ($self,%options) = @_;

    my $interp = $self->interp;
    return undef unless $interp->use_data_cache;
    return undef if $self->top_stack->{in_cache_self_flag};
    my (%retrieveOptions,%storeOptions);
    foreach (qw(key expire_if keep_in_memory busy_lock)) {
	$retrieveOptions{$_} = $options{$_} if (exists($options{$_}));
    }
    foreach (qw(key expire_at expire_next expire_in)) {
	$storeOptions{$_} = $options{$_} if (exists($options{$_}));
    }
    my $result = $self->cache(action=>'retrieve',%retrieveOptions);
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
	my %saveLocals = %$lref;
	$lref->{sink} = sub { $output .= $_[0] };
	$lref->{in_cache_self_flag} = 1;
	my $sub = $lref->{comp}->{code};
	my @args = @{$lref->{args}};
	$retval = &$sub(@args);
	$self->top_stack({%saveLocals});

	#
	# Store output and return value as a two-item listref.
	#
	$self->cache(action=>'store',value=>[$output,$retval],%storeOptions);
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

sub call_next {
    my ($self,@extra_args) = @_;
    my $comp = shift(@{$self->{wrapper_chain}}) or die "call_next: no next component to invoke";
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
	return $caller_stack[$index]->{args};
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
    my %saveLocals = %$lref;
    $lref->{sink} = sub { $content .= $_[0] };
    $lref->{in_call_self_flag} = 1;
    my $sub = $lref->{comp}->{code};
    my @args = @{$lref->{args}};
    if (ref($rref) eq 'SCALAR') {
	$$rref = &$sub(@args);
    } elsif (ref($rref) eq 'ARRAY') {
	@$rref = &$sub(@args);
    } else {
	&$sub(@args);
    }
    $self->top_stack({%saveLocals});
    $$cref = $content if ref($cref) eq 'SCALAR';

    return 1;
}

sub comp_exists
{
    my ($self,$path) = @_;
    return $self->interp->lookup($self->process_comp_path($path)) ? 1 : 0;
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
# Does relative->absolute conversion as well as checking for local
# subcomponents.
#
sub fetch_comp
{
    my ($self,$path) = @_;
    die "fetch_comp: requires path as first argument" unless defined($path);

    #
    # If path contains a colon, interpret as <comp>:<subcomp>.
    # SELF means search all components in inherit chain.
    # SUPER means search all components above current component in inherit chain.
    #
    if (index($path,':')!=-1) {
	my @search_list;
	my ($main_path,$sub_path) = split(':',$path,2);
	if ($main_path eq 'SELF') {
	    @search_list = @{$self->{inherit_chain}};
	} elsif ($main_path eq 'SUPER') {
	    # Call the "superclass" method. We start searching in the inheritance chain
	    # past the current component or the current subcomponent's parent.
	    # This won't work when we go to multiply embedded subcomponents...
	    @search_list = @{$self->{inherit_chain}};
	    my $cur_comp = $self->current_comp;
	    while (my $next = shift(@search_list)) {
		last if $next eq $cur_comp or ($cur_comp->is_subcomp and $next eq $cur_comp->parent_comp);
	    }
	    return undef unless @search_list;
	} else {
	    my $main_comp = $self->fetch_comp($main_path);
	    return undef unless $main_comp;
	    @search_list = ($main_comp);
	}
	foreach my $main_comp (@search_list) {
	    if (my $subcomp = $main_comp->subcomps->{$sub_path}) {
		return $subcomp;
	    }
	}
	return undef;
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
	# If I am a subcomponent, also check my parent's subcomponents.
	# This won't work when we go to multiply embedded subcomponents...
	if ($cur_comp->is_subcomp and my $subcomp = $cur_comp->parent_comp->subcomps->{$path}) {
	    return $subcomp;
	}
    }

    #
    # Otherwise pass the absolute path to interp->load.
    #
    $path = $self->process_comp_path($path);
    return $self->{interp}->load($path);
}

sub fetch_next { return shift->{wrapper_chain}->[0] }

sub file
{
    my ($self,$file) = @_;
    my $interp = $self->interp;
    unless (is_absolute_path($file)) {
	if ($interp->static_file_root) {
	    $file = $interp->static_file_root . "/" . $file;
	} elsif (my $dir_path = $self->current_comp->dir_path) {
	    $file = $interp->comp_root . $dir_path . "/" . $file;
	} else {
	    $file = "/$file";
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
    my ($self,$text) = @_;
    $self->current_sink->($text) if defined($text);
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
    my ($self, $comp, @args) = @_;
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
    $interp->set_global('m'=>$self) if ($interp->parser->{in_package} ne 'HTML::Mason::Commands');

    #
    # Determine sink (where output is going).
    # Look for STORE and scalar reference passed as last two arguments. This is deprecated
    # and will go away eventually.
    #
    my $sink;
    if (@args >= 2 and $args[-2] eq 'STORE' and ref($args[-1]) eq 'SCALAR' and @args % 2 == 0) {
	my $store = $args[-1];
	$$store = '';
	$sink = sub { $$store .= $_[0] if defined ($_[0]) };
	splice(@args,-2);
    } elsif ($depth>0) {
	$sink = $self->top_stack->{sink};
    } elsif ($self->out_mode eq 'batch') {
	$sink = sub { $self->{out_buffer} .= $_[0] if defined ($_[0]) };
    } else {
	$sink = $self->out_method;
    }

    #
    # Check for maximum recursion.
    #
    die "$depth levels deep in component stack (infinite recursive call?)\n" if ($depth >= $interp->{max_recurse});

    # Push new frame onto stack and increment (localized) depth.
    my $stack = $self->stack;
    push(@$stack,{'comp'=>$comp,args=>[@args],sink=>$sink});
    $REQ_DEPTH++;

    # Call start_comp hooks.
    $self->call_hooks('start_comp');

    #
    # CODEREF_NAME maps component coderefs to component names (for profiling)
    #
    my $sub = $comp->{code};
    $HTML::Mason::CODEREF_NAME{$sub} = $comp->source_file if $::opt_P && defined($comp->source_file);

    #
    # Finally, call component subroutine.
    #
    $comp->{run_count}++; $comp->{mfu_count}++;
    my ($result, @result);
    if (wantarray) {
	@result = $sub->(@args);
    } else {
	$result = $sub->(@args);
    }

    #
    # Call end_comp hooks.
    #
    $self->call_hooks('end_comp');
    
    #
    # Pop stack and return.
    #
    pop(@$stack);
    return wantarray ? @result : $result;
}

#
# Like comp, but return component output.
#
sub scomp {
    my $self = shift;
    my $store = '';

    # Set a new top-level sink.
    my $save_sink = $self->current_sink;
    $self->top_stack->{sink} = sub { $store .= $_[0] if defined($_[0]) };

    # Call comp wrapped in an eval so we can pop off sink no matter what happens
    my $retval = eval { $self->comp(@_) };
    my $err = $@;
    $self->top_stack->{sink} = $save_sink;
    die $err if $err;

    return $store;
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
    my $code = $self->interp->{hooks}->{$args{type}}->{$args{name}};
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
    my $code = $self->interp->{hooks}->{$args{type}}->{$args{name}};
    $self->{"hooks_$args{type}"} = [grep($_ ne $code,@{$self->{"hooks_$args{type}"}})];
    push(@{$self->{"hooks_$args{type}"}},$code);
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

#
# Return the absolute version of a component path. Handles . and ..
# Empty string resolves to current component path. Optional second
# argument is directory path to resolve relative paths against.
#
sub process_comp_path
{
    my ($self,$comp_path,$dir_path) = @_;
    if ($comp_path !~ /\S/) {
	return $self->current_comp->path;
    }
    if ($comp_path !~ m@^/@) {
	$dir_path = $self->current_comp->dir_path unless defined($dir_path);
	die "relative component path ($comp_path) used from component with no current directory" unless $dir_path;
	$comp_path = $dir_path . "/" . $comp_path;
    }
    while ($comp_path =~ s@/[^/]+/\.\.@@) {}
    while ($comp_path =~ s@/\./@/@) {}
    return $comp_path;    
}

1;
