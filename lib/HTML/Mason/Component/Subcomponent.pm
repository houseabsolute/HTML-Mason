# Copyright (c) 1998-99 by Jonathan Swartz. All rights reserved.
# This program is free software; you can redistribute it and/or modify it
# under the same terms as Perl itself.

package HTML::Mason::Component::Subcomponent;
require 5.004;
require Exporter;
@ISA = qw(HTML::Mason::Component);
@EXPORT = qw();
@EXPORT_OK = qw();

use HTML::Mason::Component;
use strict;

#
# Assign parent and name at runtime
#
sub assign_subcomponent_properties {
    my $self = shift;
    ($self->{owner},$self->{name}) = @_;
}

sub is_subcomp { 1 }
sub owner { return $_[0]->{owner} }
sub parent_comp { return $_[0]->{owner} }  # deprecated, replaced by owner
sub parent { return $_[0]->owner->parent }
sub name { return $_[0]->{name} }
sub path { return $_[0]->owner->path . ":" . $_[0]->name }
sub title { return $_[0]->owner->title . ":" . $_[0]->name }
sub dir_path { return $_[0]->owner->dir_path }

sub persistent { return $_[0]->owner->persistent }
sub object_file { return $_[0]->owner->object_file }
sub cache_file { return $_[0]->owner->cache_file }

1;
