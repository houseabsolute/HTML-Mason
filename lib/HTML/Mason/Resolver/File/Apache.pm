# Copyright (c) 1998-2002 by Jonathan Swartz. All rights reserved.
# This program is free software; you can redistribute it and/or modify
# it under the same terms as Perl itself.

package HTML::Mason::Resolver::File::Apache;

use strict;

use HTML::Mason::Tools qw(paths_eq);

use HTML::Mason::Resolver::File;
use base qw(HTML::Mason::Resolver::File);

#
# Given an apache request object, return the associated component
# path or undef if none exists. This is called for top-level web
# requests that resolve to a particular file.
#
sub apache_request_to_comp_path {
    my ($self, $r) = @_;

    my $file = $r->filename;
    $file .= $r->path_info unless -f $file;

    foreach my $root (map $_->[1], $self->comp_root_array) {
	if (paths_eq($root, substr($file, 0, length($root)))) {
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

The C<new> method takes a single mandatory parameter, C<comp_root>.
This parameter may be either a scalar or an array reference.  If it is
a scalar, it should be a filesystem path indicating the component
root.

If it is an array reference, it should be of the following form:

 [ [ key1 => '/path/to/root' ],
   [ key2 => '/path/to/other/root' ] ]

The "keys" for each path must be unique names and their "values" must
be filesystem paths.  These paths will be searched in order whenever a
component path must be resolved to a filesystem path.

=head1 ADDITIONAL METHODS

Besides, the methods documented in the HTML::Mason::Resolver method,
this class provides one additional method.

=over 4

=item comp_root

This method returns the component root, which will either be a scalar
or an array reference, as documented in L<CONSTRUCTOR|CONSTRUCTOR>.

=back

=cut
