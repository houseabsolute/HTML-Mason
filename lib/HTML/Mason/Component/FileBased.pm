# Copyright (c) 1998-99 by Jonathan Swartz. All rights reserved.
# This program is free software; you can redistribute it and/or modify it
# under the same terms as Perl itself.

package HTML::Mason::Component::FileBased;
require 5.004;
require Exporter;
@ISA = qw(HTML::Mason::Component);
@EXPORT = qw();
@EXPORT_OK = qw();

use File::Basename;
use strict;

sub is_file_based { 1 }
sub persistent { 1 }
sub title { return $_[0]->fq_path }
sub path { return $_[0]->fq_path }
sub name {
    my ($name,$dir_path) = fileparse($_[0]->fq_path);
    return $name;
}
sub dir_path {
    my ($name,$dir_path) = fileparse($_[0]->fq_path);
    $dir_path =~ s/\/$//g;
    return $dir_path;
}
sub source_file { return $_[0]->interp->comp_root . ($_[0]->fq_path) }

1;
