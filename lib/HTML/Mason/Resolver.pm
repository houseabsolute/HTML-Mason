# Copyright (c) 1998-2001 by Jonathan Swartz. All rights reserved.
# This program is free software; you can redistribute it and/or modify it
# under the same terms as Perl itself.

package HTML::Mason::Resolver;

use strict;

use HTML::Mason::Exceptions( abbr => ['param_error', 'virtual_error'] );

use Params::Validate qw(:all);
Params::Validate::validation_options( on_fail => sub { param_error join '', @_ } );

use HTML::Mason::Container;
use base qw(HTML::Mason::Container);

__PACKAGE__->valid_params();
__PACKAGE__->contained_objects();


sub new
{
    my $class = shift;
    return bless {validate(@_, $class->validation_spec)}, $class;
}

# Returns all info about a component including its source.
sub resolve {
    shift->_virtual;
}

sub glob_path {
    shift->_virtual;
}

sub comp_class {
    shift->_virtual;
}

# Gets just the source of the component as a string.
sub get_source {
    shift->_virtual;
}

# Returns all info about a component, but not its source.
sub get_info {
    shift->_virtual;
}

sub _virtual
{
    my $self = shift;

    my $sub = (caller(1))[3];
    $sub =~ s/.*::(.*?)$/$1/;
    virtual_error "$sub is a virtual method and must be overridden in " . ref($self);
}

1;
