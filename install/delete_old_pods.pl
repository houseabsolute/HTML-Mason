#!/usr/bin/perl -w

use strict;

foreach ( qw( Interp ApacheHandler Request Component ) )
{
    my $pod_file = `perldoc -l HTML::Mason::$_`;
    chomp $pod_file;

    if ( $pod_file =~ /\.pod$/ )
    {
	print "Found $pod_file\n";
	unlink $pod_file or die "Cannot unlink $pod_file: $!";
    }
}
