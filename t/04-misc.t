#!/usr/bin/perl -w
use Cwd;
use strict;
use vars (qw($root $branch $comp_root $data_dir));

$branch = "misc";
my $pwd = cwd();
$root = (-f "test-common.pl") ? "$pwd/.." : (-f "t/test-common.pl") ? "$pwd" : die "ERROR: cannot find test-common.pl\n";
unshift(@INC,"$root/lib");

require "$root/t/test-common.pl";

try_exec_all();

1;
