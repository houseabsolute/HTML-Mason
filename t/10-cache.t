#!/usr/bin/perl -w
use Cwd;
use strict;
use IO::File;
use vars (qw($root $branch $comp_root $data_dir));

$branch = "cache";
my $pwd = cwd();
$root = (-f "test-common.pl") ? "$pwd/.." : (-f "t/test-common.pl") ? "$pwd" : die "ERROR: cannot find test-common.pl\n";
unshift(@INC,"$root/lib");

# Skip if flock not implemented.
eval { my $fh = new IO::File "$root/t/test-common.pl"; flock($fh,1) };
if ($@) {
    print "1..0\n";
    exit;
}

require "$root/t/test-common.pl";
init();

try_exec_all();

1;
