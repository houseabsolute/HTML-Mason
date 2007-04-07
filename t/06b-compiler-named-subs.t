#!/usr/bin/perl -w

use strict;

use HTML::Mason::Tests;


my $tests = make_tests();
$tests->run;

sub make_tests {
    my $group =
        HTML::Mason::Tests->tests_class->new
            ( name => 'compiler_named_subs',
              description => 'compiler with named subs in components' );

#------------------------------------------------------------

    $group->add_test( name => 'basic',
                      description => 'Make sure that named_component_subs_works',
                      interp_params => { named_component_subs => 1 },
                      component => <<'EOF',
This is a test
EOF
                      expect => <<'EOF',
This is a test
EOF
                    );

#------------------------------------------------------------

    $group->add_test( name => 'subcomps',
                      description => 'Make sure that named_component_subs_works with subcomps',
                      interp_params => { named_component_subs => 1 },
                      component => <<'EOF',
<& .subcomp &>
<%def .subcomp>
This is a subcomp
</%def>
EOF
                      expect => <<'EOF',

This is a subcomp
EOF
                    );

#------------------------------------------------------------

    $group->add_test( name => 'methods',
                      description => 'Make sure that named_component_subs_works with methods',
                      interp_params => { named_component_subs => 1 },
                      component => <<'EOF',
<& SELF:method &>
<%method method>
This is a method
</%method>
EOF
                      expect => <<'EOF',

This is a method
EOF
                    );

#------------------------------------------------------------

    return $group;
}
