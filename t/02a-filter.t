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

    $group->add_test( name => 'filter_and_ARGS_assign',
                      description =>
                      'make sure <%filter> can see changes to %ARGS',
                      component => <<'EOF',
I am X
<%init>
$ARGS{change_to} = 'Y';
</%init>
<%filter>
s/X/$ARGS{change_to}/;
</%filter>
EOF
                      expect => <<'EOF',
I am Y
EOF
                    );

#------------------------------------------------------------

    $group->add_test( name => 'filter_and_args_section',
                      description =>
                      'make sure <%filter> can see variables from <%args> section',
                      component => <<'EOF',
I am X
<%args>
$change_to => 'Y'
</%args>
<%filter>
s/X/$change_to/;
</%filter>
EOF
                      expect => <<'EOF',
I am Y
EOF
                    );

#------------------------------------------------------------

    $group->add_support( path => '/support/has_filter',
			 component => <<'EOF',
lower case
<%filter>
$_ = uc $_;
</%filter>
EOF
		       );

    $group->add_test( name => 'filter_and_clear',
                      description => 'make sure <%filter> does not break $m->clear_buffer',
                      component => <<'EOF',
I should not show up.
<& support/has_filter &>
% $m->clear_buffer;
I should show up.
EOF
                      expect => <<'EOF',
I should show up.
EOF
                    );

#------------------------------------------------------------

        return $group;
}

