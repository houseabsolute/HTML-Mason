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

use Cwd;
use File::Spec;

use HTML::Mason::Interp;
use HTML::Mason::Compiler::ToObject;
use HTML::Mason::Tools qw(read_file);

my $alarm_works;

# Clear alarms, and skip test if alarm not implemented
eval {alarm 0; $alarm_works = 1};
my $tests = $alarm_works ? 3 : 2;

print "1..$tests\n";

if ($alarm_works)
{
    my $compiler = HTML::Mason::Compiler::ToObject->new;

    my $alarm;
    $SIG{ALRM} = sub { $alarm = 1; die "alarm"; };

    my $comp = read_file('t/taint.comp');
    eval { alarm 5; local $^W; $comp = $compiler->compile( comp_source => $comp, name => 't/taint.comp' ); };

    if ( $alarm || $@ || ! defined $comp )
    {
	my $reason;
	if ($alarm)
	{
	    $reason = "entered endless while loop";
	}
	elsif ($@)
	{
	    $reason = "gave error during test: $@";
	}
	else
	{
	    $reason = "returned an undefined value from compiling";
	}
	print "not ok 1 - $reason\n";
    }
    else
    {
	print "ok 1\n";
    }
}

my $comp_root = File::Spec->catdir( getcwd(), 'mason_tests', 'comps' );
($comp_root) = $comp_root =~ /(.*)/;
my $data_dir = File::Spec->catdir( getcwd(), 'mason_tests', 'data' );
($data_dir) = $data_dir =~ /(.*)/;
my $interp = HTML::Mason::Interp->new( comp_root => $comp_root,
				       data_dir => $data_dir,
				     );

$data_dir = File::Spec->catdir( getcwd(), 'mason_tests', 'data' );

# this is tainted, as is anything with return val from getcwd()
my $comp2 = read_file('t/taint.comp');
eval { $interp->write_object_file( object_text => $comp2,
				   object_file => "$data_dir/taint_write_test",
				 ); };

if (! $@)
{
    print "ok 2\n";
}
else
{
    print "not ok 2 - Unable to write a tainted object file to disk: $@\n";
}

my $cwd = getcwd(); # tainted
my $code = "my \$x = '$cwd';"; # also tainted

eval { $interp->eval_object_code( object_code => $code ) };

if (! $@)
{
    print "ok 3\n";
}
else
{
    print "not ok 3 - Unable to eval a tainted object file: $@\n";
}
