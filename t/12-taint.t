#!/usr/bin/perl -wT

use strict;

BEGIN
{
    $ENV{PATH} = '';

    my $libs = 'use lib qw( ';
    $libs .= join ' ', "./blib/lib", "./t/lib";
    if ($ENV{PERL5LIB})
    {
	$libs .= ' ';
	$libs .= join ' ', (split /:|;/, $ENV{PERL5LIB});
    }
    $libs .= ' );';

    ($libs) = $libs =~ /(.*)/;

    # explicitly use these because otherwise taint mode causes them to
    # be ignored
    eval $libs;
}

use HTML::Mason::Parser;

# Clear alarms, and skip test if alarm not implemented
eval {alarm 0};
if ($@) {
    print "1..0\n";
    exit;
}

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

