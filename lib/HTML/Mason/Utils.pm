# Copyright (c) 1998-2000 by Jonathan Swartz. All rights reserved.
# This program is free software; you can redistribute it and/or modify it
# under the same terms as Perl itself.

#
# Miscellaneous Mason-related utilities expected to be used by
# external applications.
#

package HTML::Mason::Utils;

use HTML::Mason::Tools qw(compress_path);
use strict;

require Exporter;

use vars qw(@ISA @EXPORT_OK);

@ISA = qw(Exporter);
@EXPORT_OK = qw(data_cache_namespace);

sub data_cache_namespace
{
    my ($path) = @_;
    return compress_path($path);
}
