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
use HTML::Mason::Tools qw(compress_path);
use strict;

sub is_file_based { 1 }
sub persistent { 1 }
sub path { return $_[0]->{'path'} }
sub source_file { return $_[0]->{'source_file'} }
sub source_dir {
    my $dir = dirname($_[0]->source_file);
    $dir =~ s/\/$// unless $dir eq '/';
    return $dir;
}
sub title {
    my ($self) = @_;
    return $self->path . ($self->{source_root_key} ? " [".lc($self->{source_root_key})."]" : "");
}
sub name { return basename($_[0]->path) }
sub dir_path {
    my $dir_path = dirname($_[0]->path);
    $dir_path =~ s/\/$// unless $dir_path eq '/';
    return $dir_path;
}
sub assign_runtime_properties {
    my ($self,$interp,$fq_path) = @_;
    $self->SUPER::assign_runtime_properties($interp,$fq_path);
    my $comp_root = $interp->comp_root;    
    if (!ref($comp_root)) {
	$self->{source_root} = $comp_root;
	$self->{'path'} = $fq_path;
    } else {
	($self->{source_root_key},$self->{'path'}) = ($fq_path =~ m{ ^/([^/]+)(/.*)$ }x)
	    or die "Assert error: could not split FQ path ($fq_path) as expected";
	foreach my $lref (@$comp_root) {
	    my ($key,$root) = @$lref;
	    if ($self->{source_root_key} eq uc($key)) {
		$self->{source_root} = $root;
	    }
	}
	die "Assert error: FQ path ($fq_path) contained unknown source root key" unless $self->{source_root};
    }
    $self->{'source_file'} = $self->{source_root} . $fq_path;
}

1;
