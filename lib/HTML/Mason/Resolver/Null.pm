# Copyright (c) 1998-2001 by Jonathan Swartz. All rights reserved.
# This program is free software; you can redistribute it and/or modify
# it under the same terms as Perl itself.

package HTML::Mason::Resolver::Null;

use strict;

use HTML::Mason::Resolver;
use base qw(HTML::Mason::Resolver);

sub new {
    return bless {}, shift;
}

sub get_info {
    return;
}

sub get_source {
    return;
}

# Returns everything get_info() returns, plus the component source in a 'comp_text' entry.
sub resolve {
    return;
}

sub comp_class {
    return 'HTML::Mason::Component';
}

sub glob_path {
    return;
}

1;

__END__

=head1 NAME

HTML::Mason::Resolver::Null - a do-nothing resolver

=head1 SYNOPSIS

  my $resolver = HTML::Mason::Resolver::Null->new;

=head1 DESCRIPTION

This HTML::Mason::Resolver subclass is useful if you want to create
components via the C<< HTML::Mason::Interp->make_component >> method
and you never plan to interact with the filesystem.

Basically, it provides all of the necessary resolver methods but none
of them do anything.

=cut
