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
    my ($self,$interp,$fq_path) = @_;

    # XXX I don't think the Component should be poking around in
    # comp_root.  That's the resolver's territory. -Ken
    my $comp_root = $interp->resolver->comp_root;    
    my $source_root;
    if (!ref($comp_root)) {
	$source_root = $comp_root;
	$self->{'path'} = $fq_path;
    } else {
	($self->{source_root_key},$self->{'path'}) = ($fq_path =~ m{ ^/([^/]+)(/.*)$ }x)
	    or error "could not split FQ path ($fq_path) as expected";
	foreach my $lref (@$comp_root) {
	    my ($key,$root) = @$lref;
	    if ($self->{source_root_key} eq uc($key)) {
		$source_root = $root;
		last;
	    }
	}
	error "FQ path ($fq_path) contained unknown source root key"
	    unless $source_root;
    }
    $self->{'source_file'} = File::Spec->canonpath( File::Spec->catfile( $source_root, $self->{'path'} ) );
    $self->SUPER::assign_runtime_properties($interp,$fq_path);
}

1;
