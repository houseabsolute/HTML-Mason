#!/usr/bin/perl -wT

use strict;

$ENV{PATH} = '';

use lib 'blib/lib';
use HTML::Mason::Parser;

print "1..1\n";

my $parser = HTML::Mason::Parser->new;

my $alarm;
$SIG{ALRM} = sub { $alarm = 1; die "alarm"; };

my $comp;
eval { alarm 5; local $^W; $comp = $parser->parse_component( script_file => 't/taint.comp' ); };

if ( $alarm || $@ || ! defined $comp )
{
    print "Taint test failed: ";
    if ($alarm)
    {
	print "entered endless while loop\n";
    }
    elsif ($@)
    {
	print "gave error during test: $@\n";
    }
    else
    {
	print "returned an undefined value from parsing\n";
    }
    print "not ok 1\n";
}
else
{
    print "ok 1\n";
}
