# Copyright (c) 1998-2002 by Jonathan Swartz. All rights reserved.
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
                         filter
			 ignore_flush
		       ) ],
    );

__PACKAGE__->valid_params
    (
     sink         => { type => SCALARREF | CODEREF, optional => 1 },
     parent       => { isa => 'HTML::Mason::Buffer', optional => 1 },
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

    if ( defined $self->{sink} )
    {
	if ( UNIVERSAL::isa( $self->{sink}, 'SCALAR' ) )
	{
	    # convert scalarref to a coderef for efficiency
	    my $b = $self->{buffer} = $self->{sink};
	    $self->{sink} = sub { for (@_) { $$b .= $_ if defined } };
	}
    }
    else
    {
	# create an empty string to use as buffer
	my $buf = '';
	my $b = $self->{buffer} = \$buf;
	$self->{sink} = sub { for (@_) { $$b .= $_ if defined } };
    }

    $self->{ignore_flush} = 1 unless $self->{parent};
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
    $self->parent->receive( $self->output ) if $self->parent;
    $self->clear;
}

sub clear
{
    my $self = shift;
    ${$self->{buffer}} = '';
}

sub output
{
    my $self = shift;
    my $output = ${$self->{buffer}};
    return $self->filter->( $output ) if $self->filter;
    return $output;
}

1;

