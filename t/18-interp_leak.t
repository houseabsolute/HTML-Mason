use strict;

use HTML::Mason::Interp;
use HTML::Mason::Tests;

my $destroy_count = 0;

sub HTML::Mason::Interp::DESTROY { $destroy_count++ }

print "1..2\n";

# this object is just used for convenience in order to find a proper
# comp root, write comps, etc.  It won't actually run any tests
my $group = HTML::Mason::Tests->new( name => 'interp_leaks',
                                     description => '',
                                   );

$group->add_support( path => '/test_comp',
                     component => <<'EOF',
Content is irrelevant
EOF
                   );

# I am evil
$group->_write_support_comps;

{
    # don't sent output to STDOUT
    my $buf;
    my $interp = HTML::Mason::Interp->new( data_dir => $group->data_dir,
                                           comp_root => $group->comp_root,
                                           out_method => \$buf,
                                         );

    $interp->exec('/interp_leaks/test_comp');
}

print $destroy_count == 1 ? "ok 1\n" : "not ok 1\n";


{
    my $buf;
    my $interp = HTML::Mason::Interp->new( data_dir => $group->data_dir,
                                           comp_root => $group->comp_root,
                                           out_method => \$buf,
                                         );

    $interp->exec('/interp_leaks/test_comp');
}

print $destroy_count == 2 ? "ok 2\n" : "not ok 2\n";

# I am very evil
$group->_cleanup unless $ENV{MASON_NO_CLEANUP};
