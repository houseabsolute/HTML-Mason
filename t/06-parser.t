#!/usr/bin/perl -w
use Cwd;
use strict;
use vars (qw($root $branch $comp_root $data_dir));

$branch = "request";
my $pwd = cwd();
$root = (-f "test-common.pl") ? "$pwd/.." : (-f "t/test-common.pl") ? "$pwd" : die "ERROR: cannot find test-common.pl\n";
unshift(@INC,"$root/lib");

require "$root/t/test-common.pl";

try_parse({},'allow_globals');
try_parse({allow_globals=>qw($declared)},'allow_globals');
try_parse({},'ignore_warnings_expr');
try_parse({ignore_warnings_expr=>'Use of undefined value'},'ignore_warnings_expr');
try_parse({},'in_package');
try_parse({in_package=>'HTML::Mason::NewPackage'},'in_package');
try_exec({postamble=>'print "this is the postamble\n"'},'');	 
try_exec({preamble=>'print "this is the preamble"'},'');	 

1;
