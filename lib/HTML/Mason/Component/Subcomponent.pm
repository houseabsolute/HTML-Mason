# Copyright (c) 1998-99 by Jonathan Swartz. All rights reserved.
# This program is free software; you can redistribute it and/or modify it
# under the same terms as Perl itself.

package HTML::Mason::Component::Subcomponent;
require 5.004;
@ISA = qw(HTML::Mason::Component);

use HTML::Mason::Component;
use strict;

use HTML::Mason::MethodMaker ( read_only => [ qw( name owner ) ] );

#
# Assign parent and name at runtime
#
sub assign_subcomponent_properties {
    my $self = shift;
    ($self->{owner},$self->{name}) = @_;
}

sub cache_file { return $_[0]->owner->cache_file }
sub create_time { return $_[0]->owner->create-time }
sub dir_path { return $_[0]->owner->dir_path }
sub is_subcomp { 1 }
sub object_file { return $_[0]->owner->object_file }
sub parent { return $_[0]->owner->parent }
sub parent_comp { return $_[0]->{owner} }  # deprecated, replaced by owner
sub parser_version { return $_[0]->owner->parser_version }
sub path { return $_[0]->owner->path . ":" . $_[0]->name }
sub persistent { return $_[0]->owner->persistent }
sub title { return $_[0]->owner->title . ":" . $_[0]->name }

1;
