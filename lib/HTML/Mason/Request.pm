# Copyright (c) 1998-99 by Jonathan Swartz. All rights reserved.
# This program is free software; you can redistribute it and/or modify it
# under the same terms as Perl itself.

package HTML::Mason::Request;
require 5.004;
require Exporter;
@ISA = qw(Exporter);
@EXPORT = qw();
@EXPORT_OK = qw();

use strict;
use vars qw($REQ $REQ_DEPTH %REQ_DEPTHS);
my @_used = ($HTML::Mason::CODEREF_NAME,$::opt_P,$HTML::Mason::Commands::REQ);

my %fields =
    (autohandler_next => undef,
     count => 0,
     dhandler_arg => undef,
     interp => undef,
     stack_array => undef,
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
	abort_flag => undef,
	abort_retval => undef,
	error_flag => undef,
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

#
# Return the current number of stack levels. 1 means top level, 0
# means that no component has been called yet.
#
sub depth
{
    my ($self) = @_;
    return ($self eq $REQ) ? $REQ_DEPTH : ($REQ_DEPTHS{$self} || 0);
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
    return $_[0]->{interp}->{parser};
}

#
# Execute the next component in this request. call() sets up proper
# dynamically scoped variables and invokes call1() to do the work.
#
sub call {
    my $req = shift(@_);

    if (defined($REQ) and $req eq $REQ) {
	local $REQ_DEPTH = $REQ_DEPTH;
	$req->call1(@_);
    } else {
	local %REQ_DEPTHS = %REQ_DEPTHS;
	$REQ_DEPTHS{$REQ} = $REQ_DEPTH if defined($REQ);
	local $REQ = $req;
	local $REQ_DEPTH = $REQ_DEPTHS{$REQ} || 0;
	$req->call1(@_);
    }
}
sub call1 {
    my ($req, $comp, %args) = @_;
    my $interp = $req->{interp};
    my $depth = $REQ_DEPTH;

    #
    # $comp can be an absolute path or component object.  If a path,
    # load into object.
    #
    if (!ref($comp)) {
	my $path = $comp;
	$comp = $req->fetch_comp($path) or die "could not find component for path '$path'\n";
    }

    #
    # $REQ is a global containing this request. This needs to
    # be defined in the HTML::Mason::Commands package, as well
    # as the component package if that is different.
    #
    local $HTML::Mason::Commands::REQ = $req;
    $interp->set_global(REQ=>$req) if ($interp->parser->{in_package} ne 'HTML::Mason::Commands');

    #
    # Determine sink (where output is going).
    #
    my $sink;
    if (exists($args{STORE})) {
	my $store = $args{STORE};
	die "Request::call: STORE value ($store) is not a scalar reference" if ref($store) ne 'SCALAR';
	$$store = '';
	$sink = sub { $$store .= $_[0] if defined ($_[0]) };
	delete($args{STORE});
    } elsif ($depth==0) {
	$sink = $interp->{out_method};
    } else {
	$sink = $req->top_stack->{sink};
    }

    #
    # Check for maximum recursion.
    #
    die "$depth levels deep in component stack (infinite recursive call?)\n" if ($depth >= $interp->{max_recurse});

    # Push new frame onto stack and increment (localized) depth.
    my $stack = $req->stack;
    push(@$stack,{comp=>$comp,args=>{%args},sink=>$sink});
    $REQ_DEPTH++;

    # Call start_comp hooks.
    $req->call_hooks('start_comp');

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
	@result = $sub->(%args);
    } else {
	$result = $sub->(%args);
    }

    #
    # Call end_comp hooks.
    #
    $req->call_hooks('end_comp');
    
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
# Abort out of current execution.
#
sub abort
{
    my ($self) = @_;
    $self->{abort_flag} = 1;
    $self->{abort_retval} = $_[1];
    die "aborted";
}

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
	die "relative component path ($compPath) used from anonymous component" if !defined($self->comp->dir_path);
	$compPath = $self->comp->dir_path . "/" . $compPath;
    }
    while ($compPath =~ s@/[^/]+/\.\.@@) {}
    while ($compPath =~ s@/\./@/@) {}
    return $compPath;    
}

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

1;
