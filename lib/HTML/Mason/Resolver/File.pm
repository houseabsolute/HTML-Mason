# Copyright (c) 1998-2001 by Jonathan Swartz. All rights reserved.
# This program is free software; you can redistribute it and/or modify
# it under the same terms as Perl itself.

package HTML::Mason::Resolver::File;

use strict;

use File::Spec;
use HTML::Mason::Resolver;
use HTML::Mason::Tools qw(paths_eq read_file);
use Params::Validate qw(:all);

use HTML::Mason::Resolver;
use base qw(HTML::Mason::Resolver);

__PACKAGE__->valid_params
    (
     comp_root    => { parse => 'list', type => SCALAR|ARRAYREF },
    );

__PACKAGE__->contained_objects();

use HTML::Mason::MethodMaker
    ( read_only => ['comp_root'] );


sub new {
    my $package = shift;
    my $self = $package->SUPER::new(@_);

    #
    # Check that directories are absolute.
    #
    if (ref $self->{comp_root}) {
	foreach my $pair (@{$self->comp_root}) {
	    HTML::Mason::Exception::Params->throw( error => "Multiple-path component root must consist of a list of two-element lists; see documentation" )
		if ref($pair) ne 'ARRAY';
	    $pair->[1] = File::Spec->canonpath( $pair->[1] );
	    HTML::Mason::Exception::Params->throw( error => "comp_root ('$pair->[0]') must contain only absolute directories" )
		unless File::Spec->file_name_is_absolute( $pair->[1] );
	}

    } else {
	# comp_root is a string
	$self->{comp_root} = File::Spec->canonpath( $self->{comp_root} );
 	HTML::Mason::Exception::Params->throw( error => "comp_root ('".$self->{comp_root}."') must be an absolute directory" )
	    unless File::Spec->file_name_is_absolute( $self->{comp_root} );
    }

    return $self;
}

#
# With a single component root, the fully-qualified path is just
# the component path. With multiple component roots, we search
# through each root in turn, and the fully-qualified path is
# uc(root key) + component path.
#
sub resolve {
    my ($self,$path) = @_;
    my $comp_root = $self->{comp_root};
    if (!ref($comp_root)) {
	my $srcfile = File::Spec->catdir( $comp_root, $path );
	return (-f $srcfile) ? ( path => $path, description => $srcfile, last_modified => (stat _)[9] ) : ();
    } else {
	foreach my $lref (@$comp_root) {
	    my ($key,$root) = @$lref;
	    $key = uc($key);   # Always make key uppercase in fqpath
	    my $srcfile = File::Spec->catfile( $root, $path );
	    my @srcstat = stat $srcfile;
	    my $fq_path = File::Spec->canonpath( File::Spec->catfile( File::Spec->rootdir, $key, $path ) );
	    return ( path => $fq_path, description => $srcfile, last_modified => $srcstat[9] ) if (-f _);
	}
	return;
    }
}

sub comp_class {
    return 'HTML::Mason::Component::FileBased';
}

#
# Given a glob pattern, return all existing paths.
#
sub glob_path {
    my ($self,$pattern) = @_;
    my $comp_root = $self->{comp_root};
    my @roots;
    if (!ref($comp_root)) {
	@roots = ($comp_root);
    } else {
	@roots = map($_->[1],@{$comp_root});
    }
    my %path_hash;
    foreach my $root (@roots) {
	my @files = glob($root.$pattern);
	foreach my $file (@files) {
	    if (my ($path) = ($file =~ m/$root(\/.*)$/)) {  # File::Spec?
		$path_hash{$path}++;
	    }
	}
    }
    return keys(%path_hash);
}

sub get_component {
    my ($self, %lookup_info) = @_;

    return read_file( $lookup_info{description} );
}

#
# Given a filename, return the associated component path or undef if
# none exists. This is called for top-level web requests that resolve
# to a particular file.
#
sub file_to_path {
    my ($self,$file) = @_;
    my $comp_root = $self->{comp_root};
    my @roots;

    if (!ref($comp_root)) {
	@roots = ($comp_root);
    } else {
	@roots = map($_->[1],@{$comp_root});
    }
    foreach my $root (@roots) {
	if (paths_eq($root,substr($file,0,length($root)))) {
	    my $path = substr($file, ($root eq '/' ? 0 : length($root)));
	    $path =~ s/\/$// unless $path eq '/';
	    return $path;
	}
    }
    return undef;
}

1;
