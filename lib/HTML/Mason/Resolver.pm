# Copyright (c) 1998-2000 by Jonathan Swartz. All rights reserved.
# This program is free software; you can redistribute it and/or modify it
# under the same terms as Perl itself.

package HTML::Mason::Resolver;
require 5.004;

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

sub glob_path {
    return ();
}

sub file_to_path {
    my ($self,$file,$interp) = @_;    
    return $file;
}
sub get_last_modified {
    return 0;
}

sub get_source_text {
    shift->_virtual;
}

sub get_source_description {
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

sub get_source_params {
    return (script=>($_[0]->get_source_text(@_)));
}

1;
