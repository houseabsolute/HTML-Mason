package Apache::Mason;
# Copyright (c) 1998 by Jonathan Swartz. All rights reserved.
# This program is free software; you can redistribute it and/or modify it
# under the same terms as Perl itself.

# This is a placeholder module whose sole purpose is to create a CPAN
# "alias" Apache::Mason -> HTML::Mason. Currently there is no way to use
# Mason with a "PerlModule" specification; instead you define your own
# handler which calls HTML::Mason functions directly.  See the
# HTML::Mason documentation to get started.

$VERSION = '0.4';

sub handler ($$) {
    return 1;
}

1;
