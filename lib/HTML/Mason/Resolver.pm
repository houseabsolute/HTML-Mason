# Copyright (c) 1998-99 by Jonathan Swartz. All rights reserved.
# This program is free software; you can redistribute it and/or modify it
# under the same terms as Perl itself.

package HTML::Mason::Resolver;
require 5.004;
require Exporter;
@ISA = qw(Exporter);
@EXPORT = qw();
@EXPORT_OK = qw();

use strict;

sub new
{
    my $class = shift;
    my $self = {};
    bless $self,$class;
    return $self;
}

sub lookup_path {
    my ($self,$path,$interp) = @_;    
    return $path;
}

sub file_to_path {
    my ($self,$path,$interp) = @_;    
    return $path;
}
sub get_last_modified {
    return 0;
}

sub get_source_text {
    die "Base class HTML::Mason::Resolver can't get source text";
}

sub get_source_description {
    die "Base class HTML::Mason::Resolver can't get source description";
}

sub get_source_params {
    return (script=>($_[0]->get_source_text(@_)));
}

1;
