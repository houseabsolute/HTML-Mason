# Copyright (c) 1998-2001 by Jonathan Swartz. All rights reserved.
# This program is free software; you can redistribute it and/or modify it
# under the same terms as Perl itself.

package HTML::Mason::Buffer;

use strict;

use Params::Validate qw(:all);
Params::Validate::validation_options( on_fail => sub { HTML::Mason::Exception::Params->throw( error => join '', @_ ) } );

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

sub allowed_params { \%valid_params }
sub validation_spec { return shift->allowed_params }

sub new
{
    my $class = shift;
    my $self = bless { validate( @_, $class->validation_spec ) }, $class;

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

1;

