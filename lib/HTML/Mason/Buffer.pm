# Copyright (c) 1998-2000 by Jonathan Swartz. All rights reserved.
# This program is free software; you can redistribute it and/or modify it
# under the same terms as Perl itself.

package HTML::Mason::Buffer;

use strict;

# 1) output to a code ref
# 	coderef, [parent]
# 	sink => CODEREF
# 2) output to a user defined scalar ref
# 	scalarref, [parent]
# 	sink => SCALARREF
# 3) output to an internal scalar ref
# 	parent, mode
# 	parent => x, buffered => 1
# 4) output with same code ref as parent
# 	parent
# 	parent => x
# 5) output to internal scalar ref because parent is scalar ref
# 	parent
# 	parent => x
# 
# comp (first)
# 	output to $m->output_method (1 or 2, no parent)
# comp (not first)
# 	output same as parent (4 or 5)
# scomp
# 	output to internal scalar (3) +ignore_flush
# comp with <%filter>
# 	output to internal scalar (3)
# comp with content
# 	output same as parent (4 or 5)
# $m->content
# 	locally replace top level stack, or add an extra level to the stack (3)
# 	need to manipulate top of stack, so it looks like the right component
# 	$m->depth will be wrong either way

use HTML::Mason::MethodMaker
    ( read_only => [ qw( sink
			 parent
			 mode
                         filter
			 ignore_flush
		       ) ],
    );

sub new
{
    my $class = shift;
    my $self = { sink => undef,
		 parent => undef,
		 mode => undef,
		 ignore_flush => 0,
		 filter => undef,
		 @_
	       };
#	my (%options) = @_;
#	while (my ($key,$value) = each(%options)) {
#		if (exists($fields{$key})) {
#			$self->{$key} = $value;
#		} else {
#			die "HTML::Mason::Request::new: invalid option '$key'\n";
#		}
#	}
    bless $self, $class;
    $self->initialize;
    return $self;
}

sub initialize
{
    my $self = shift;

    if ( defined $self->{sink} )
    {
	# user-defined sink
	if ( UNIVERSAL::isa( $self->{sink}, 'CODE' ) )
	{
	    $self->{mode} ||= 'stream';
	}
	elsif ( UNIVERSAL::isa( $self->{sink}, 'SCALAR' ) )
	{
	    $self->{mode} ||= 'batch';
	    # convert scalarref to a coderef for efficiency
	    $self->{buffer} = $self->{sink};
	    $self->{sink} = sub { for (@_) { ${ $self->{buffer} } .= $_ if defined } };
	}
	else
	{
	    HTML::Mason::Exception::Params->throw( error => "Sink must be a coderef or a scalarref." );
	}

    }
    else
    {
	# If we have no sink we must have a parent whose sink we can use
	HTML::Mason::Exception::Params->throw( error => "Buffering to default sink requires a parent buffer." )
	    unless $self->{parent};

	$self->{mode} ||= $self->{parent}->mode;

	if ($self->{mode} eq 'stream')
	{
	    $self->{sink} = $self->{parent}->sink;
	}
	else
	{
	    $self->{buffer} = '';
	    $self->{sink} = sub { for (@_) { $self->{buffer} .= $_ if defined } };
	}
    }
}

sub new_child
{
    my $self = shift;
    return HTML::Mason::Buffer->new( parent => $self, @_ );
}

sub receive
{
    my $self = shift;
    $self->sink->(@_);
}

sub flush
{
    my $self = shift;
    return if $self->ignore_flush;
    $self->parent->receive( $self->output ) if $self->parent;
}

sub clear
{
    my $self = shift;
    if ( ref $self->{buffer} )
    {
	${ $self->{buffer} } = '';
    }
    else
    {
	$self->{buffer} = '';
    }
}

sub output
{
    my $self = shift;
    my $output = ref $self->{buffer} ? ${ $self->{buffer} } : $self->{buffer};
    return $self->filter->( $output ) if $self->filter;
    return $output;
}

sub dispose
{
    my $self = shift;
    foreach ( qw( parent sink ) )
    {
	undef $self->{$_};
    }
}

=pod
=begin nobody
## old buffer stack attempt

sub new
{
	$self->{stdout_tie} = tied *STDOUT;
	$self->{stack} = [ \&Apache::print ];
	tie *STDOUT,buffer
	# do this in Request::exec --- local *Apache::print = \&buffer::print;
}

sub DESTROY
{
	$self->pop_buffer(1) while @{$self->{stack}};
	untie *STDOUT;
	tie *STDOUT, $self->{stdout_tie} if $self->{stdout_tie};
}

PRINT
PRINTF
(for tied stdout)


### Reimplement <%filter> with buffer stack and/or closures

out_mode (batch/stream)
batch: top level buffer is scalar
stream: top level buffer is $r->print
	scomp and filter must run component output to a scalar


buffer stack will be separate from component stack, because we will 
need a buffer for each comp(), <%filter>, <| |>, or user creation.

probably create $m->print, $m->out will do the same thing, depricated


Task #202
stacked output buffers
roll back output when error occurs (batch mode)
(stream mode will only stream until <%filter> <| |> or scomp happens)

Task #117
reimplement filters without $m->call_self
(buffer stack)
( could do it with current code, but it only allows one sink per 
  component stack level )

Task #201
subvert print() and $r->print

Task #231
$m->flush_buffer does nothing inside <%filter> or $m->scomp
(just exactly what _should_ it do??? 
 if it actually flushes, then the caller will not see anything
 or is the desired behavior: 
   comp:
     stuff the caller/filter will not see
    flush_buffer
     stuff the caller/filter will see
)

Task #210
abort inside filter or scomp does not display content
(#210 implies flush_buffer on exception, #202 implies clear_buffer on exception)
perhaps exception should clear buffer, but keep a copy of cleared data
in case user wants to flush instead.


Currently, each component has its own sink (on the component stack)
which writes to a (usually) shared buffer.
When the component exits, the sink is popped of the stack along with
the component.

+------+  +------+  +------+
| comp |  | comp |  | comp |
+------+  +------+  +------+
   |         |         |
   V         V         V
+--------------------------+
|  buffer                  |
+--------------------------+


For <%filter> or user buffers, there is no longer a 1-1 match between
buffers and components.  So how do we be sure to keep them in sync?

One way below, but if users can push/pop buffers, they may not do the right thing.

push_buffer
eval {
 ...
 flush_buffer
}
$unflushed_data = buffer
pop_buffer

OR

$level = push_buffer
eval {
  ...
  flush_buffer($level)
}
pop_buffer($level)


flush/clear _all_ output?
	flush_buffer(0);


comp:
# should push a \$scalar if the component has a <%filter>
push_buffer()  # default: same method as before
eval {
  run
  flush_buffer
}
$unflushed = buffer if buffer
pop_buffer
die if $@


scomp:
push_buffer(\$scalar)
eval {
  run
}
$data = buffer
pop_buffer
if $@ { $unflushed = $data ; die }
return $data


<%filter>:
... component body
$_ = buffer
... filter body


<%filter>:
push_buffer(\$scalar)
eval {
  component body
}
$_ = buffer
pop_buffer
die if $@
filter body 


$m->flush_buffer:
flush_buffer(0)  # all the way down


$m->clear_buffer:
clear_buffer(0)  # all the way down


( or maybe 0 is current buffer, undef is all the way down )
=end nobody
=cut

1;

