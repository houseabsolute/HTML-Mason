#!/usr/bin/perl -w

use strict;
use HTML::Mason::Tests;

my $tests = make_tests();
$tests->run;

sub make_tests
{
    my $group =
         HTML::Mason::Tests->new( name => 'filter',
                                  description => 'Tests <%filter> specific problems' );

#------------------------------------------------------------

    $group->add_test( name => 'filter_and_shared',
                      description =>
                      'make sure <%filter> can see variables from <%shared>',
                      component => <<'EOF',
I am X
<%shared>
my $change_to = 'Y';
</%shared>
<%filter>
s/X/$change_to/;
</%filter>
EOF
                      expect => <<'EOF',
I am Y
EOF
                    );

#------------------------------------------------------------

    $group->add_test( name => 'filter_and_ARGS',
                      description =>
                      'make sure <%filter> can see variables %ARGS',
                      call_args => { change_to => 'Y' },
                      component => <<'EOF',
I am X
<%filter>
s/X/$ARGS{change_to}/;
</%filter>
EOF
                      expect => <<'EOF',
I am Y
EOF
                    );

#------------------------------------------------------------

        return $group;
}

