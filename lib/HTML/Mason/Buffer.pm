# Copyright (c) 1998-2001 by Jonathan Swartz. All rights reserved.
# This program is free software; you can redistribute it and/or modify it
# under the same terms as Perl itself.

package HTML::Mason::Buffer;

use strict;

use HTML::Mason::Container;
use base qw(HTML::Mason::Container);

use HTML::Mason::Exceptions( abbr => ['param_error'] );

use Params::Validate qw(:all);
Params::Validate::validation_options( on_fail => sub { param_error join '', @_ } );

use HTML::Mason::MethodMaker
    ( read_only => [ qw( sink
			 parent
			 mode
                         filter
			 ignore_flush
		       ) ],
    );

__PACKAGE__->valid_params
    (
     sink         => { type => SCALARREF | CODEREF, optional => 1 },
     parent       => { isa => 'HTML::Mason::Buffer', optional => 1 },
     mode         => { type => SCALAR,
		       callbacks =>
		       { 'batch or stream' => sub { $_[0] =~ /^(?:batch|stream)$/ } },
		       optional => 1 },
     ignore_flush => { type => SCALAR, default => 0 },
     filter       => { type => CODEREF, optional => 1 },
    );

__PACKAGE__->contained_objects
    (
     # None
    );

sub new
{
    my $class = shift;
    my @args = $class->create_contained_objects(@_);

    my $self = bless { validate( @args, $class->validation_spec ) }, $class;

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
	    param_error "HTML::Mason::Buffer->new requires either a mode, parent, or sink parameter";
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
	param_error "Buffering to a default sink only works in batch mode or with a parent buffer."
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
    return __PACKAGE__->new( parent => $self, @_ );
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

    if ($self->parent)
    {
	$self->parent->receive( $self->output );
	$self->clear;
    }
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

1;

