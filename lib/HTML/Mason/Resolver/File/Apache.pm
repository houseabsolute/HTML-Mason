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

HTML::Mason::Resolver::File::Apache - translates component paths into filesystem paths

=head1 SYNOPSIS

  my $resolver = HTML::Mason::Resolver::File::Apache->new( comp_root => '/var/www/mason' );

=head1 DESCRIPTION

This HTML::Mason::Resolver subclass is used when components are stored
on the filesystem and you are using C<HTML::Mason::ApacheHandler> to
process requests.  It adds an additional method,
C<apache_request_to_comp_path>, needed by
C<HTML::Mason::ApacheHandler>.

=cut
