# Copyright (c) 1998-2002 by Jonathan Swartz. All rights reserved.
# This program is free software; you can redistribute it and/or modify
# it under the same terms as Perl itself.

package HTML::Mason::Resolver::File;

use strict;

use Cwd qw(cwd);
use File::Spec;
use HTML::Mason::Tools qw(read_file);
use Params::Validate qw(:all);

use HTML::Mason::ComponentSource;
use HTML::Mason::Resolver;
use base qw(HTML::Mason::Resolver);

use HTML::Mason::Exceptions (abbr => ['param_error']);

__PACKAGE__->valid_params
    (
     comp_root    => { parse => 'list', type => SCALAR|ARRAYREF, default => File::Spec->rel2abs( File::Spec->rootdir ),
		       descr => "A string or array of arrays indicating the search path for component calls" },
    );

sub new {
    my $package = shift;
    my %p = @_;

    my $self = $package->SUPER::new(@_);

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

    return $self;
}

sub comp_root_array
{
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

sub get_info {
    my ($self, $path) = @_;

    foreach my $lref ($self->comp_root_array) {
	my ($key, $root) = @$lref;
	my $srcfile = File::Spec->canonpath( File::Spec->catfile( $root, $path ) );
	next unless -f $srcfile;
	my $modified = (stat _)[9];
	my $base = $key eq 'MAIN' ? '' : "/$key";
	$key = undef if $key eq 'MAIN';

	return HTML::Mason::ComponentSource->new( friendly_name => $srcfile,
						  comp_id => "$base$path",
						  last_modified => $modified,
						  comp_path => $path,
						  comp_class => 'HTML::Mason::Component::FileBased',
						  extra => { comp_root => $key },
						  source_callback => sub { read_file($srcfile) },
						);
    }
    return;
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

=over

=item comp_root

The C<new> method takes a single mandatory parameter, C<comp_root>.
This parameter may be either a scalar or an array reference.  If it is
a scalar, it should be a filesystem path indicating the component
root.

If it is an array reference, it should be of the following form:

 [ [ key1 => '/path/to/root' ],
   [ key2 => '/path/to/other/root' ] ]

The "keys" for each path must be unique names and their "values" must
be filesystem paths.  These paths will be searched in the provided
order whenever a component path must be resolved to a filesystem path.

=back

=head1 ADDITIONAL METHODS

Besides, the methods documented in the HTML::Mason::Resolver method,
this class provides one additional method.

=over 4

=item comp_root

This method returns the component root, which will either be a scalar
or an array reference, as documented in L<CONSTRUCTOR|"CONSTRUCTOR">.

=back

=head1 SEE ALSO

L<HTML::Mason|HTML::Mason>

=cut
