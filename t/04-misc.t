#!/usr/bin/perl -w

use strict;

use HTML::Mason::Tests;

my $tests = make_tests();
$tests->run;

sub make_tests
{
    my $group = HTML::Mason::Tests->new( name => 'misc',
					 description => 'autohandler and dhandler functionality' );

    $group->add_support( path => '/autohandler_test/autohandler',
			 component => <<'EOF',
<& header &>
Autohandler comp: <% $m->fetch_next->title %>
% my $buf;
<% $m->call_next (b=>$a*2) %>
<& footer &>

<%args>
$a=>5
</%args>
EOF
		       );

    $group->add_support( path => '/autohandler_test/header',
			 component => <<'EOF',
<body bgcolor=<% $bgcolor %>>
<h2>The Site</h2>

<%args>
$bgcolor=>'white'
</%args>
EOF
		       );

    $group->add_support( path => '/autohandler_test/footer',
			 component => <<'EOF',
<hr>
Copyright 1999 Schmoopie Inc.

EOF
		       );

    $group->add_test( name => 'autohandler',
		      path => '/autohandler_test/hello',
		      call_path => '/autohandler_test/hello',
		      description => 'autohandler test',
		      component => <<'EOF',
Hello World!
The answer is <% $b %>.
<%args>
$b
</%args>


EOF
		      expect => <<'EOF',
<body bgcolor=white>
<h2>The Site</h2>


Autohandler comp: /misc/autohandler_test/hello
Hello World!
The answer is 10.



<hr>
Copyright 1999 Schmoopie Inc.



EOF
		    );


    $group->add_support( path => '/dhandler_test/dhandler',
			 component => <<'EOF',
dhandler = <% $m->current_comp->title %>
dhandler arg = <% $m->dhandler_arg %>

<%args>
$decline=>0
</%args>
EOF
		       );

    $group->add_support( path => '/dhandler_test/subdir/dhandler',
			 component => <<'EOF',
% $m->decline if $m->dhandler_arg eq 'leaf3';
dhandler = <% $m->current_comp->title %>
dhandler arg = <% $m->dhandler_arg %>
EOF
		       );

    $group->add_test( name => 'dhandler1',
		      description => 'tests dhandler against nonexistent comp',
		      path => '',
		      call_path => '/dhandler_test/foo/bar',
		      component => '',
		      expect => <<'EOF',
dhandler = /misc/dhandler_test/dhandler
dhandler arg = foo/bar

EOF
		    );

    $group->add_test( name => 'dhandler2',
		      description => 'real comp to make sure the real comp is invoked, not the dhandler',
		      path => '/dhandler_test/subdir/leaf',
		      call_path => '/dhandler_test/subdir/leaf',
		      component => <<'EOF',
I'm leaf
EOF
		      expect => <<'EOF',
I'm leaf
EOF
		    );

    $group->add_test( name => 'dhandler3',
		      description => 'real comp declines the request to make sure the dhandler is invoked',
		      path => '/dhandler_test/subdir/leaf2',
		      call_path => '/dhandler_test/subdir/leaf2',
		      component => <<'EOF',
% $m->decline;
I'm leaf2
EOF
		      expect => <<'EOF',
dhandler = /misc/dhandler_test/subdir/dhandler
dhandler arg = leaf2

EOF
		    );

    $group->add_test( name => 'dhandler4',
		      description => 'declines twice to make sure higher level dhandler is called',
		      path => '/dhandler_test/subdir/leaf3',
		      call_path => '/dhandler_test/subdir/leaf3',
		      component => <<'EOF',
% $m->decline;
I'm leaf3
EOF
		      expect => <<'EOF',
dhandler = /misc/dhandler_test/dhandler
dhandler arg = subdir/leaf3

EOF
		    );

    return $group;
}
