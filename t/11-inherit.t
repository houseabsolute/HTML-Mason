#!/usr/bin/perl -w
use Cwd;
use vars (qw($root $branch $comp_root $data_dir));

$branch = "inherit";
my $pwd = cwd();
$root = (-f "test-common.pl") ? "$pwd/.." : (-f "t/test-common.pl") ? "$pwd" : die "ERROR: cannot find test-common.pl\n";
unshift(@INC,"$root/lib");

require "$root/t/test-common.pl";
init();

sub try_exec_with_interp {
    my ($test) = @_;
    # Create new interp based on options.
    my $interp = new HTML::Mason::Interp(comp_root => $comp_root, data_dir => $data_dir, allow_recursive_autohandlers => 1);
    try_exec($interp,$test);
}

print "1..3\n";
try_exec_with_interp('subdir/normal');
try_exec_with_interp('subdir/bypass');
try_exec_with_interp('subdir/ignore');

1;
