#!/usr/bin/perl -w

use strict;
use File::Basename;
use HTML::Mason::Tests;

my $outside_comp_root_test_file;
my $tests = make_tests();
$tests->run;

sub make_tests
{

    my $group = HTML::Mason::Tests->tests_class->new( name => 'comp-calls',
						      description => 'Component call syntax' );
    $outside_comp_root_test_file = dirname($group->comp_root) . "/.outside_comp";

#------------------------------------------------------------

    $group->add_support( path => '/support/amper_test',
			 component => <<'EOF',
amper_test.<p>
% if (%ARGS) {
Arguments:<p>
%   foreach my $key (sort keys %ARGS) {
<b><% $key %></b>: <% $ARGS{$key} %><br>
%   }
% }
EOF
		       );

#------------------------------------------------------------

    $group->add_test( name => 'ampersand',
		      description => 'tests all variations of component call path syntax',
		      component => <<'EOF',
<&support/amper_test&>
<& support/amper_test &>
<&  support/amper_test, &>
<& support/amper_test
&>
<&
support/amper_test &>
<&
support/amper_test
&>
EOF
		      expect => <<'EOF',
amper_test.<p>

amper_test.<p>

amper_test.<p>

amper_test.<p>

amper_test.<p>

amper_test.<p>

EOF
		 );

#------------------------------------------------------------

    $group->add_test( name => 'ampersand_with_args',
		      description => 'tests variations of component calls with arguments',
		      component => <<'EOF',
<& /comp-calls/support/amper_test, message=>'Hello World!'  &>
<& support/amper_test, message=>'Hello World!',
   to=>'Joe' &>
<& "support/amper_test" &>
% my $dir = "support";
% my %args = (a=>17, b=>32);
<& $dir . "/amper_test", %args &>
EOF
		      expect => <<'EOF',
amper_test.<p>
Arguments:<p>
<b>message</b>: Hello World!<br>

amper_test.<p>
Arguments:<p>
<b>message</b>: Hello World!<br>
<b>to</b>: Joe<br>

amper_test.<p>

amper_test.<p>
Arguments:<p>
<b>a</b>: 17<br>
<b>b</b>: 32<br>

EOF
		 );

#------------------------------------------------------------

    $group->add_support( path => '/support/funny_-+=@~~~._name',
			 component => <<'EOF',
foo is <% $ARGS{foo} %>
EOF
		       );

#------------------------------------------------------------

    $group->add_test( name => 'ampersand_with_funny_name',
		      description => 'component with non-alphabetic characters',
		      component => <<'EOF',
<& support/funny_-+=@~~~._name, foo => 5 &>
EOF
		      expect => <<'EOF',
foo is 5
EOF
		 );

#------------------------------------------------------------

    # This only tests for paths passed through Request::fetch_comp,
    # not Interp::load.  Not sure how zealously we want to
    # canonicalize.
    #
    $group->add_test( name => 'canonicalize_paths',
		      description => 'test that various paths are canonicalized to the same component',
		      component => <<'EOF',
<%perl>
my $path1 = '///comp-calls/support//amper_test';
my $comp1 = $m->fetch_comp($path1)
  or die "could not fetch comp1";
my $path2 = './support/./././amper_test';
my $comp2 = $m->fetch_comp($path2)
  or die "could not fetch comp2";
my $path3 = './support/../support/../support/././amper_test';
my $comp3 = $m->fetch_comp($path3)
  or die "could not fetch comp3";
unless ($comp1 == $comp2 && $comp2 == $comp3) {
    die sprintf
	(
	 "different component objects for same canonical path:\n  %s (%s -> %s)\n  %s (%s -> %s)\n  %s (%s -> %s)",
	 $comp1, $path1, $comp1->path,
	 $comp2, $path2, $comp2->path,
	 $comp3, $path3, $comp3->path,
	 );
}
$m->comp($comp1);
$m->comp($comp2);
$m->comp($comp3);
</%perl>
EOF
		      expect => <<'EOF',
amper_test.<p>
amper_test.<p>
amper_test.<p>
EOF
		 );

#------------------------------------------------------------

    $group->add_test( name => 'outside_comp_root_prepare',
		      description => 'test that file exists in dist/t for next two tests',
		      pre_code => sub { local *F; open(F, ">$outside_comp_root_test_file"); print F "hi"; },
		      component => "test file '$outside_comp_root_test_file' <% -e '$outside_comp_root_test_file' ? 'exists' : 'does not exist' %>",
		      expect => "test file '$outside_comp_root_test_file' exists",
		 );

#------------------------------------------------------------

    $group->add_test( name => 'outside_comp_root_absolute',
		      description => 'cannot call components outside comp root with absolute path',
		      component => <<'EOF',
<& /../.outside_comp &>
EOF
		      expect_error => qr{could not find component for path '/../.outside_comp'},
		 );

#------------------------------------------------------------

    $group->add_test( name => 'outside_comp_root_relative',
		      description => 'cannot call components outside comp root with relative path',
		      component => <<'EOF',
<& ../../.outside_comp &>
EOF
		      expect_error => qr{could not find component for path '../../.outside_comp'},
		 );

#------------------------------------------------------------

    # put /../ in add_support path to put component right under comp root
    $group->add_support( path => '/../outside_comp_root_from_top',
			 component => <<'EOF',
<& ../.outside_comp &>
EOF
		       );

#------------------------------------------------------------

    $group->add_test( name => 'outside_comp_root_relative_from_top',
		      description => 'cannot call components outside comp root with relative path from component at top of root',
		      component => <<'EOF',
<& /outside_comp_root_from_top &>
EOF
		      expect_error => qr{could not find component for path '../.outside_comp'},
		 );

#------------------------------------------------------------

    return $group;
}
