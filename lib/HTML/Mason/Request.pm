# Copyright (c) 1998 by Jonathan Swartz. All rights reserved.
# This program is free software; you can redistribute it and/or modify it
# under the same terms as Perl itself.

package HTML::Mason::Request;
require 5.004;
require Exporter;
@ISA = qw(Exporter);
@EXPORT = qw();
@EXPORT_OK = qw();

use strict;
use vars qw($AUTOLOAD);
my @_used = ($HTML::Mason::CODEREF_NAME,$::opt_P);

my %fields =
    (autohandler_next => undef,
     count => 0,
     interp => undef,
     stack => [],
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
	req_code_cache => {},
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
    $self->{count} = ++($interp->{request_count});

    return $self;
}

#
# Return the current number of stack levels. 1 means top level.
#
sub depth
{
    return scalar(@{$_[0]->{stack}});
}

#
# Return the parser associated with this request (by way of interp).
#
sub parser
{
    return $_[0]->{interp}->{parser};
}

#
# Execute the next component in this request.
#
sub exec_next {
    my ($req, $comp, %args) = @_;
    my $interp = $req->{interp};

    #
    # $comp can be an absolute path or component object.  If a path,
    # load into object.
    #
    if (!ref($comp)) {
	my $path = $req->process_comp_path($comp);
	$comp = $interp->load($path);
	die "could not find component for path '$path'\n" if (!$comp);
    }

    #
    # $REQ is a global containing this request. This needs to
    # be defined in the HTML::Mason::Commands package, as well
    # as the component package if that is different.
    #
    local $HTML::Mason::Commands::REQ = $req;
    $interp->set_global(REQ=>$req) if ($interp->parser->{in_package} ne 'HTML::Mason::Commands');

    #
    # Check for maximum recursion.
    #
    my $depth = scalar(@{$req->{stack}});
    die "$depth levels deep in component stack (infinite recursive call?)\n" if ($depth > $interp->{max_recurse});

    #
    # Determine sink (where output is going).
    #
    my $sink;
    if (exists($args{STORE})) {
	my $store = $args{STORE};
	die "exec_next: STORE value ($store) is not a scalar reference" if ref($store) ne 'SCALAR';
	$$store = '';
	$sink = sub { $$store .= $_[0] if defined ($_[0]) };
	delete($args{STORE});
    } elsif (!$depth) {
	$sink = $interp->{out_method};
    } else {
	$sink = $req->{stack}->[0]->{sink};
    }

    #
    # Push new frame onto stack.
    #
    unshift(@{$req->{stack}},{comp=>$comp,args=>{%args},sink=>$sink});

    #
    # Call start_comp hooks.
    #
    $req->call_hooks('start_comp');

    #
    # CODEREF_NAME maps component coderefs to component names (for profiling)
    #
    my $sub = $comp->code;
    $HTML::Mason::CODEREF_NAME{$sub} = $comp->source_file if $::opt_P && defined($comp->source_file);

    #
    # Finally, call component subroutine.
    #
    my ($result, @result);
    if (wantarray) { @result = $sub->(%args) } else { $result = $sub->(%args) }

    #
    # Call end_comp hooks.
    #
    $req->call_hooks('end_comp');

    #
    # Pop stack and return.
    #
    shift(@{$req->{stack}});
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
    my $code = $self->{hooks}->{$args{type}}->{$args{name}};
    $self->interp->{"hooks_$args{type}"} = [grep($_ ne $code,@{$self->{"hooks_$args{type}"}})];
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
# Return hash reference at top of stack.
#
sub topstack
{
    return $_[0]->{stack}->[0];
}

#
# Accessor methods for top of stack elements.
#
sub comp { return $_[0]->{stack}->[0]->{comp} }
sub args { return $_[0]->{stack}->[0]->{args} }
sub sink { return $_[0]->{stack}->[0]->{sink} }

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
	die "relative component path ($compPath) used from anonymous component with no parent path" if !defined($self->comp->parent_path);
	$compPath = $self->comp->parent_path . "/" . $compPath;
    }
    while ($compPath =~ s@/[^/]+/\.\.@@) {}
    while ($compPath =~ s@/\./@/@) {}
    return $compPath;    
}

1;
