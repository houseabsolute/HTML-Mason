#!/usr/bin/perl -wT

use strict;

BEGIN
{
    # Cwd in taint mode spits out weird errors with older Perls and
    # may or may not work at all
    if ( $] < 5.006 )
    {
        print "1..0\n";
        exit;
    }

    $ENV{PATH} = '';
}

# Cwd has to be loaded after sanitizing $ENV{PATH}
use Cwd;
use File::Spec;

BEGIN
{
    my $curdir = File::Spec->curdir;

    my $libs = 'use lib qw( ';
    $libs .=
        ( join ' ',
          File::Spec->catdir( $curdir, 'blib', 'lib' ),
          File::Spec->catdir( $curdir, 't', 'lib' )
        );

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

use HTML::Mason::Interp;
use HTML::Mason::Compiler::ToObject;
use HTML::Mason::Tools qw(read_file);

my $alarm_works;

# Clear alarms, and skip test if alarm not implemented
eval {alarm 0; $alarm_works = 1};
my $tests = $alarm_works ? 3 : 2;

print "1..$tests\n";

my $test_count = 1;
if ($alarm_works)
{
    my $compiler = HTML::Mason::Compiler::ToObject->new;

    my $alarm;
    $SIG{ALRM} = sub { $alarm = 1; die "alarm"; };

    my $comp = read_file( File::Spec->catfile( File::Spec->curdir, 't', 'taint.comp' ) );
    eval { alarm 5;
           local $^W;
           $comp = $compiler->compile( comp_source => $comp, name => 't/taint.comp' );
       };

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
	print "not ok $test_count - $reason\n";
    }
    else
    {
	print "ok $test_count\n";
    }

    $test_count++;
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
my $comp2 = read_file( File::Spec->catfile( File::Spec->curdir, 't', 'taint.comp' ) );

# This isn't a part of the documented interface, but we test it here anyway.
eval { $interp->write_object_file( object_code => \$comp2,
				   object_file =>
                                   File::Spec->catfile( $data_dir, 'taint_write_test' ),
				 ); };

if (! $@)
{
    print "ok $test_count\n";
}
else
{
    print "not ok $test_count - Unable to write a tainted object file to disk: $@\n";
}
$test_count++;

my $cwd = getcwd(); # tainted
# This isn't a part of the documented interface, but we test it here anyway.
my $code = "# MASON COMPILER ID: ". $interp->compiler->object_id ."\nmy \$x = '$cwd';"; # also tainted
eval { $interp->eval_object_code( object_code => \$code ) };

if (! $@)
{
    print "ok $test_count\n";
}
else
{
    print "not ok $test_count - Unable to eval a tainted object file: $@\n";
}
$test_count++;
