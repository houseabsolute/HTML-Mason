# Copyright (c) 1998-99 by Jonathan Swartz. All rights reserved.
# This program is free software; you can redistribute it and/or modify it
# under the same terms as Perl itself.

package HTML::Mason::Request;
require 5.004;
require Exporter;
@ISA = qw(Exporter);
@EXPORT = qw();
@EXPORT_OK = qw();

use HTML::Mason::Tools qw(is_absolute_path);

use strict;
use vars qw($REQ $REQ_DEPTH %REQ_DEPTHS);
my @_used = ($HTML::Mason::CODEREF_NAME,$::opt_P,$HTML::Mason::Commands::REQ);

my %fields =
    (interp => undef,
     );
# Create accessor routines
foreach my $f (keys %fields) {
    no strict 'refs';
    *{$f} = sub {my $s=shift; return @_ ? ($s->{$f}=shift) : $s->{$f}};
}

sub new
{
    my $class = shift;
    my $self = {
	%fields,
	autohandler_next => undef,
	abort_flag => undef,
	abort_retval => undef,
	count => 0,
	dhandler_arg => undef,
	error_flag => undef,
	out_buffer => '',
	stack_array => undef,
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
    while (my ($type,$href) = each(%{$interp->{hooks}})) {
	$self->{"hooks_$type"} = [values(%$href)] if (%$href);
    }
    $self->{stack_array} = [];
    $self->{count} = ++($interp->{request_count});

    return $self;
}

#
# Abort out of current execution.
#
sub abort
{
    my ($self) = @_;
    $self->{abort_flag} = 1;
    $self->{abort_retval} = $_[1] || $self->{abort_retval} || undef;
    die "aborted";
}

#
#
#
sub aborted {
    my ($self) = @_;
    return $self->{abort_flag};
}

sub aborted_value {
    my ($self) = @_;
    return $self->{abort_retval};
}

sub auto_comp {
    my ($self) = @_;
    my $aref = $self->{autohandler_next};
    return $aref ? $aref->[0] : undef;
}

sub auto_next {
    my ($self,@extra_args) = @_;
    my $aref = $self->{autohandler_next} or die "auto_next: no autohandler invoked";
    my ($comp, $args_ref) = @$aref;
    my @args = (@$args_ref,@extra_args);
    return $self->call($comp, @args);
}

sub cache
{
    my ($self,%options) = @_;
    my $interp = $self->interp;
    return undef unless $interp->use_data_cache;
    $options{action} = $options{action} || 'retrieve';
    $options{key} = $options{key} || 'main';
    
    my $comp = $self->callers(0);
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
    my $result = mc_cache(action=>'retrieve',%retrieveOptions);
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
	mc_cache(action=>'store',value=>[$output,$retval],%storeOptions);
    } else {
	($output,$retval) = @$result;

	#
	# Not clear whether to call these hooks...Best guess is
	# whether the component output anything. These may
	# be going away soon anyway...
	if ($output) {
	    $self->call_hooks('start_primary');
	    $self->call_hooks('end_primary');
	}
    }
    mc_out($output);

    #
    # Return the component return value in case the caller is interested,
    # followed by 1 indicating the cache retrieval success.
    #
    return ($retval,1);
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
    my @callers = reverse(@{$self->stack});
    if (defined($index)) {
	return $callers[$index]->{comp};
    } else {
	return map($_->{comp},@callers);
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
    my @args = %{$lref->{args}};
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

#
# comp is a synonym for call
#
*comp = \&call;

sub comp_exists
{
    return $REQ->lookup(shift) ? 1 : 0;
}

sub count { shift->{count} }

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
    if ($path !~ /\//) {
	# Check my subcomponents.
	if (my $comp = $self->comp->subcomps->{$path}) {	
	    return $comp;
	}
	# If I am a subcomponent, also check my parent's subcomponents.
	# This won't work when we go to multiply embedded subcomponents...
	if ($self->comp->is_subcomp and my $comp = $self->comp->parent_comp->subcomps->{$path}) {
	    return $comp;
	}
    }
    $path = $self->process_comp_path($path);
    return $self->{interp}->load($path);
}

sub file
{
    my ($self,$file) = @_;
    my $interp = $self->interp;
    unless (is_absolute_path($file)) {
	if ($interp->static_file_root) {
	    $file = $interp->static_file_root . "/" . $file;
	} elsif (my $dir_path = $self->callers(0)->dir_path) {
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
    $self->sink->($text) if defined($text);
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
# Execute the next component in this request. call() sets up proper
# dynamically scoped variables and invokes call1() to do the work.
#
sub call {
    my $self = shift(@_);

    if (defined($self) and $self eq $self) {
	local $REQ_DEPTH = $REQ_DEPTH;
	$self->call1(@_);
    } else {@lst = qw(a b c d e);
	local %REQ_DEPTHS = %REQ_DEPTHS;
	$REQ_DEPTHS{$self} = $REQ_DEPTH if defined($self);
	local $self = $self;
	local $REQ_DEPTH = $REQ_DEPTHS{$self} || 0;
	$self->call1(@_);
    }
}
sub call1 {
    my ($self, $comp, @args) = @_;
    my $interp = $self->{interp};
    my $depth = $REQ_DEPTH;

    #
    # $comp can be an absolute path or component object.  If a path,
    # load into object.
    #
    if (!ref($comp)) {
	my $path = $comp;
	$comp = $self->fetch_comp($path) or die "could not find component for path '$path'\n";
    }

    #
    # $REQ is a dynamically scoped global containing this
    # request. This needs to be defined in the HTML::Mason::Commands
    # package, as well as the component package if that is different.
    #
    local $HTML::Mason::Commands::REQ = $self;
    $interp->set_global(REQ=>$self) if ($interp->parser->{in_package} ne 'HTML::Mason::Commands');

    #
    # Determine sink (where output is going). Look for STORE and scalar
    # reference passed as last two arguments.
    #
    my $sink;
    if ($args[-2] eq 'STORE' and ref($args[-1]) eq 'SCALAR' and @args > 2 and @args % 2 == 0) {
	my $store = $args[-1];
	$$store = '';
	$sink = sub { $$store .= $_[0] if defined ($_[0]) };
	splice(@args,-2);
    } elsif ($depth>0) {
	$sink = $self->top_stack->{sink};
    } elsif ($interp->out_mode eq 'batch') {
	$sink = sub { $self->{out_buffer} .= $_[0] if defined ($_[0]) };
    } else {
	$sink = $interp->{out_method};
    }

    #
    # Check for maximum recursion.
    #
    die "$depth levels deep in component stack (infinite recursive call?)\n" if ($depth >= $interp->{max_recurse});

    # Push new frame onto stack and increment (localized) depth.
    my $stack = $self->stack;
    push(@$stack,{comp=>$comp,args=>[@args],sink=>$sink});
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
    $comp->{run_count}++;
    my ($result, @result);
    if (wantarray) {
	@result = $sub->($self,@args);
    } else {
	$result = $sub->($self,@args);
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

sub set_buffer
{
    my ($self, $content) = @_;
    $self->{out_buffer} = $content;
}

sub get_buffer
{
    my ($self) = @_;
    return $self->{out_buffer};
}

sub clear_buffer
{
    my ($self) = @_;
    $self->{out_buffer} = '';
}

sub flush_buffer
{
    my ($self, $content) = @_;
    $self->interp->out_method($self->{out_buffer});
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
sub comp { return $_[0]->top_stack->{comp} }
sub args { return $_[0]->top_stack->{args} }
sub sink { return $_[0]->top_stack->{sink} }

#
# Return the absolute version of a component path. Handles . and ..
# Empty string resolves to current component path.
#
sub process_comp_path
{
    my ($self,$compPath) = @_;
    if ($compPath !~ /\S/) {
	return $self->comp->path;
    }
    if ($compPath !~ m@^/@) {
	die "relative component path ($compPath) used from component with no current directory" if !defined($self->comp->dir_path);
	$compPath = $self->comp->dir_path . "/" . $compPath;
    }
    while ($compPath =~ s@/[^/]+/\.\.@@) {}
    while ($compPath =~ s@/\./@/@) {}
    return $compPath;    
}

1;
