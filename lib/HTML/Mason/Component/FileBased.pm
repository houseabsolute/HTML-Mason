# Copyright (c) 1998-2001 by Jonathan Swartz. All rights reserved.
# This program is free software; you can redistribute it and/or modify
# it under the same terms as Perl itself.

package HTML::Mason::Component::FileBased;

use strict;

use File::Basename;
use File::Spec;

use HTML::Mason::Component;
use base qw(HTML::Mason::Component);

use HTML::Mason::Exceptions( abbr => ['error'] );

use HTML::Mason::MethodMaker ( read_only => [ qw( path source_file ) ] );

sub is_file_based { 1 }
sub persistent { 1 }
sub source_dir {
    my $dir = dirname($_[0]->source_file);
    return File::Spec->canonpath($dir);
}
sub title {
    my ($self) = @_;
    return $self->path . ($self->{source_root_key} ? " [".lc($self->{source_root_key})."]" : "");
    #return $self->path . ($self->{source_root_key} ? " [$self->{source_root_key}]" : "");
}
sub name { return basename($_[0]->path) }
sub dir_path {
    my $dir_path = dirname($_[0]->path);
    $dir_path =~ s/\/$// unless $dir_path eq '/';
    return $dir_path;
}

# Ends up setting $self->{path, source_root_key, source_file} and a few in the parent class
sub assign_runtime_properties {
    my ($self, $interp, %info) = @_;

    $self->{path} = $info{url_path};
    $self->{source_file} = $info{disk_path};
    $self->{source_root_key} = $info{comp_root};

    $self->SUPER::assign_runtime_properties($interp, %info);
}

1;
