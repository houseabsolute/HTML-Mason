# Copyright (c) 1998-2002 by Jonathan Swartz. All rights reserved.
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
@EXPORT_OK = qw(data_cache_namespace cgi_request_args);

sub data_cache_namespace
{
    my ($path) = @_;
    return compress_path($path);
}

sub cgi_request_args
{
    my ($q, $method) = @_;

    my %args;

    # Checking that there really is no query string when the method is
    # not POST is important because otherwise ->url_param returns a
    # parameter named 'keywords' with a value of () (empty array).
    # This is apparently a feature related to <ISINDEX> queries or
    # something (see the CGI.pm) docs.  It makes my head hurt. - dave
    my @methods = $method ne 'POST' || ! $ENV{QUERY_STRING} ? ( 'param' ) : ( 'param', 'url_param' );
    foreach my $key ( map { $q->$_() } @methods ) {
	next if exists $args{$key};
	my @values = map { $q->$_($key) } @methods;
	$args{$key} = @values == 1 ? $values[0] : \@values;
    }

    return %args;
}
