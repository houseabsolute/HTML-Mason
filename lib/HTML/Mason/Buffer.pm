# Copyright (c) 1998-2001 by Jonathan Swartz. All rights reserved.
# This program is free software; you can redistribute it and/or modify it
# under the same terms as Perl itself.

package HTML::Mason::Buffer;

use strict;

use Params::Validate qw(:all);
Params::Validate::set_options( on_fail => sub { HTML::Mason::Exception::Params->throw( error => join '', @_ ) } );

use HTML::Mason::MethodMaker
    ( read_only => [ qw( sink
			 parent
			 mode
                         filter
			 ignore_flush
		       ) ],
    );

my %valid_params =
    (
     sink => { type => SCALARREF | CODEREF, optional => 1 },
     parent => { isa => 'HTML::Mason::Buffer', optional => 1 },
     mode => { callbacks =>
	       { 'batch or stream' => sub { $_[0] =~ /^(?:batch|stream)/ } },
	       optional => 1 },
     ignore_flush => { type => SCALAR, default => 0 },
     filter => { type => CODEREF, optional => 1 },
    );

sub valid_params { \%valid_params }

sub new
{
    my $class = shift;
    my $self = bless { validate( @_, $class->valid_params ) }, $class;

    bless $self, $class;
    $self->_initialize;
    return $self;
}

sub _initialize
{
    my $self = shift;

    # first figure out our mode if not given
    unless ( $self->{mode} )
    {
	if ( $self->{sink} )
	{
	    $self->{mode} = UNIVERSAL::isa( $self->{sink}, 'CODE' ) ? 'stream' : 'batch';
	}
	elsif ( $self->{parent} )
	{
	    $self->{mode} = $self->{parent}->mode;
	}
	else
	{
	    HTML::Mason::Exception::Params->throw( error => "HTML::Mason::Buffer->new requires either a mode, parent, or sink parameter" );
	}
    }

    if ( defined $self->{sink} )
    {
	if ( UNIVERSAL::isa( $self->{sink}, 'SCALAR' ) )
	{
	    # convert scalarref to a coderef for efficiency
	    $self->{buffer} = $self->{sink};
	    my $b = $self->{buffer};
	    $self->{sink} = sub { for (@_) { $$b .= $_ if defined } };
	}
    }
    else
    {
	HTML::Mason::Exception::Params->throw( error => "Buffering to a default sink only works in batch mode or with a parent buffer." )
	    unless $self->{parent} || $self->{mode} eq 'batch';


	if ($self->{mode} eq 'stream')
	{
	    $self->{sink} = $self->{parent}->sink;
	}
	else
	{
	    $self->{buffer} = '';
	    my $b = \$self->{buffer};
	    $self->{sink} = sub { for (@_) { $$b .= $_ if defined } };
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

=pod

=begin nobody


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

