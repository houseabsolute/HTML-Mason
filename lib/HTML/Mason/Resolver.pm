# Copyright (c) 1998-2001 by Jonathan Swartz. All rights reserved.
# This program is free software; you can redistribute it and/or modify it
# under the same terms as Perl itself.

package HTML::Mason::Resolver;

use strict;

use Params::Validate qw(:all);
Params::Validate::set_options( on_fail => sub { HTML::Mason::Exception::Params->throw( error => join '', @_ ) } );

use HTML::Mason::MethodMaker
    ( read_write => [ 'interp' ] );

sub new
{
    my $class = shift;

    validate_pos( @_, { isa => 'HTML::Mason::Interp' } );

    my $self = { interp => shift };

    return bless $self, $class;
}

sub resolve {
    shift->_virtual;
}

sub glob_path {
    shift->_virtual;
}

sub comp_class {
    shift->_virtual;
}

sub get_component {
    shift->_virtual;
}

sub _virtual
{
    my $self = shift;

    my $sub = (caller(1))[3];
    $sub =~ s/.*::(.*?)$/$1/;
    HTML::Mason::Exception::VirtualMethod->throw( error =>
						  "$sub is a virtual method and must be subclassed in " . ref $self );
}

1;
