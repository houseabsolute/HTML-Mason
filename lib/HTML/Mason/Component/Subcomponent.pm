# Copyright (c) 1998-2002 by Jonathan Swartz. All rights reserved.
# This program is free software; you can redistribute it and/or modify
# it under the same terms as Perl itself.

package HTML::Mason::Component::Subcomponent;

use strict;

use HTML::Mason::Component;

use vars qw(@ISA);

@ISA = qw(HTML::Mason::Component);

use HTML::Mason::MethodMaker ( read_only => [ qw( name owner ) ] );

#
# Assign parent and name at runtime
#
sub assign_subcomponent_properties {
    my $self = shift;
    ($self->{owner},$self->{name}) = @_;
}

sub cache_file { return $_[0]->owner->cache_file }
sub load_time { return $_[0]->owner->load_time }
sub comp_id { return "[subcomponent '" . $_[0]->name . "' of " . $_[0]->owner->comp_id . "]" }
sub compiler_id { return $_[0]->owner->compilation_params }
sub dir_path { return $_[0]->owner->dir_path }
sub is_subcomp { 1 }
sub object_file { return $_[0]->owner->object_file }
sub parent { return $_[0]->owner->parent }
sub parent_comp { return $_[0]->{owner} }  # deprecated, replaced by owner
sub path { return $_[0]->owner->path . ":" . $_[0]->name }
sub persistent { return $_[0]->owner->persistent }
sub title { return $_[0]->owner->title . ":" . $_[0]->name }

# This method is a bit of a hack but it works ;)
#
# Basically, we check if there is a method of the same name in our
# owner, and to be extra careful, we make sure that that method is
# actually $self (the same object) in case some wacko defines a method
# and a subcomp with the same name, in the same component, which is
# dumb but legal.
sub is_method
{
    my $self = shift;

    my $m = $self->owner->methods( $self->name );

    return $m && $m eq $self;
}

1;
