# Copyright (c) 1998-2002 by Jonathan Swartz. All rights reserved.
# This program is free software; you can redistribute it and/or modify
# it under the same terms as Perl itself.

package HTML::Mason::Resolver::File;

use strict;

use File::Spec;
use HTML::Mason::Tools qw(paths_eq read_file);
use Params::Validate qw(:all);

use HTML::Mason::Resolver;
use base qw(HTML::Mason::Resolver);

use HTML::Mason::Exceptions (abbr => ['param_error']);

__PACKAGE__->valid_params
    (
     comp_root    => { parse => 'list', type => SCALAR|ARRAYREF },
    );

__PACKAGE__->contained_objects();


sub new {
    my $package = shift;
    my $self = $package->SUPER::new(@_);

    if ($self->{comp_root}) {
	# Put it through the accessor to ensure proper data structure
	$self->comp_root( $self->{comp_root} ) unless ref $self->{comp_root};

	# Check that directories are absolute.
	foreach my $pair ($self->comp_root_array) {
	    param_error "Multiple-path component root must consist of a list of two-element lists; see documentation"
		if ref($pair) ne 'ARRAY';
	    $pair->[1] = File::Spec->canonpath( $pair->[1] );
	    param_error "comp_root '$pair->[1]' is not an absolute directory"
		unless File::Spec->file_name_is_absolute( $pair->[1] );
	}
    }

    return $self;
}

sub comp_root_array
{
    return () unless $_[0]->{comp_root};
    return @{ $_[0]->{comp_root} };
}

sub comp_root
{
    my $self = shift;
    if (@_)
    {
	validate_pos @_, {type => ARRAYREF|SCALAR};
	$self->{comp_root} = ref($_[0]) ? $_[0] : [[ MAIN => $_[0] ]];
    }
    return unless $self->{comp_root};
    return $self->{comp_root}[0][1] if @{$self->{comp_root}} == 1 and $self->{comp_root}[0][0] eq 'MAIN';
    return $self->{comp_root};
}

#  get_info() returns a hash:
#  (
#   url_path => same as incoming path parameter
#   disk_path => where this component is stored on the filesystem
#   fq_path   => combination of comp_root and url_path: "$comp_root:$url_path"
#   comp_root => name of component root we found the component in
#   last_modified => time of last modification, in seconds
#  );

sub get_info {
    my ($self, $path) = @_;

    foreach my $lref ($self->comp_root_array) {
	my ($key, $root) = @$lref;
	my $srcfile = File::Spec->canonpath( File::Spec->catfile( $root, $path ) );
	next unless -f $srcfile;
	my $modified = (stat _)[9];
	my $base = $key eq 'MAIN' ? '' : "/$key";
	$key = undef if $key eq 'MAIN';
	return ( url_path => $path, disk_path => $srcfile, fq_path => "$base$path",
		 comp_root => $key, last_modified => $modified );
    }
    return;
}

# If the caller has already done get_info(), they can pass that hash to get_source()
# and obtain the source.  resolve() will do everything in one step.
sub get_source {
    my ($self, %info) = @_;
    return read_file $info{disk_path};
}

# Returns everything get_info() returns, plus the component source in a 'comp_text' entry.
sub resolve {
    my ($self, $path) = @_;

    my %info = $self->get_info($path) or return;
    return ( %info, comp_text => read_file($info{disk_path}) );
}

sub comp_class {
    return 'HTML::Mason::Component::FileBased';
}

#
# Given a glob pattern of url_paths, return all existing url_paths for that glob.
#
sub glob_path {
    my ($self,$pattern) = @_;
    my @roots = map $_->[1], $self->comp_root_array or return;

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

#
# Given a filename, return the associated component path or undef if
# none exists. This is called for top-level web requests that resolve
# to a particular file.
#
sub file_to_path {
    my ($self,$file) = @_;

    foreach my $root (map $_->[1], $self->comp_root_array) {
	if (paths_eq($root,substr($file,0,length($root)))) {
	    my $path = substr($file, ($root eq '/' ? 0 : length($root)));
	    $path =~ s,\/$,, unless $path eq '/';
	    return $path;
	}
    }
    return undef;
}

1;

__END__

=head1 NAME

HTML::Mason::Resolver::File - translates component paths into filesystem paths

=head1 SYNOPSIS

  my $resolver = HTML::Mason::Resolver::File->new( comp_root => '/var/www/mason' );

  my %comp_info = $resolver->get_info('/some/comp.html');

  my $source = $resolver->get_source(%comp_info);

  my $comp_root = $resolver->comp_root;

  # return "/some/comp.html"
  my $comp_path = $resolver->file_to_path('/var/www/mason/some/comp.html');

=head1 DESCRIPTION

This HTML::Mason::Resolver subclass is used when components are stored
on the filesystem, which is the norm for most Mason-based applications.

=head1 CONSTRUCTOR

The C<new> method takes a single parameter.

=head1 ADDITIONAL METHODS

Besides, the methods documented

=cut
