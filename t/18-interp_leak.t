use strict;

use Test;
BEGIN {plan tests => 2}

use HTML::Mason::Interp;
use HTML::Mason::Tests;

my $destroy_count = 0;

sub HTML::Mason::Interp::DESTROY { $destroy_count++ }

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

ok $destroy_count, 1;


{
    my $buf;
    my $interp = HTML::Mason::Interp->new( data_dir => $group->data_dir,
                                           comp_root => $group->comp_root,
                                           out_method => \$buf,
                                         );

    $interp->exec('/interp_leaks/test_comp');
}

ok $destroy_count, 2;

# I am very evil
$group->_cleanup unless $ENV{MASON_NO_CLEANUP};
