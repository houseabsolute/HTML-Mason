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

    $group->add_test( name => 'filters_in_subcomps',
                      description => 'test <%filter> sections in subcomps only',
                      component => <<'EOF',
Main Component
<& .sub1 &>
<& .sub2 &>

<%def .sub1>
Sub 1
<%filter>
s/Sub/Subcomponent/;
</%filter>
</%def>

<%def .sub2>
Subcomp 2
<%filter>
s/Subcomp/Subcomponent/;
</%filter>
</%def>

EOF
                      expect => <<'EOF',
Main Component

Subcomponent 1


Subcomponent 2
EOF
                    );

#------------------------------------------------------------

    $group->add_test( name => 'filters_in_comp_and_subcomps',
                      description => 'test <%filter> sections in both main comp and subcomps',
                      component => <<'EOF',
Main Component (lowercase)
<& .sub1 &>
<& .sub2 &>

<%def .sub1>
Sub 1
<%filter>
s/Sub/Subcomponent/;
</%filter>
</%def>

<%def .sub2>
Subcomp 2
<%filter>
s/Subcomp/Subcomponent/;
</%filter>
</%def>

<%filter>
$_ = lc($_);
</%filter>

EOF
                      expect => <<'EOF',
main component (lowercase)

subcomponent 1


subcomponent 2
EOF
                    );

#------------------------------------------------------------

    # The output we expect from this test depends on how we decide
    # flush should be handled in the presence of a filter.  The output
    # expected below assumes that flushes are ignored, but if we
    # decide otherwise that should be changed.
    $group->add_test( name => 'filter_and_flush',
                      description => 'test effect of flush with filter present',
                      component => <<'EOF',
foo
% $m->flush_buffer;
bar
<%filter>
s/(foo)\s+(\S+)/$2$1/;
</%filter>
EOF
                      expect => <<'EOF',
barfoo
EOF
                    );

#------------------------------------------------------------

        return $group;
}

