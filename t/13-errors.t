#!/usr/bin/perl -w

use strict;

use HTML::Mason::Tests;

my $tests = make_tests();
$tests->run;

sub make_tests
{
    my $group = HTML::Mason::Tests->new( name => 'errors',
					 description => 'Test that errors are generated properly' );

    $group->add_support( path => 'call_with_eval',
			 component => <<'EOF',
% eval {
<& $comp, @args &>
% };
% if ($@) {
<& '/shared/check_error', error => $@, lines => $lines &>
% } else {
no error
% }
<%args>
$comp
$lines => 1
@args => ()
</%args>
EOF
		       );

    $group->add_support( path => '_make_error_support',
			 component => <<'EOF',
<%args>
foo
</%args>
EOF
		       );

    $group->add_test( name => '_make_error',
		      description => 'Exercise known failures for Parser.pm _make_error method',
		      component => <<'EOF',
<& call_with_eval, comp => '_make_error_support',  lines => 2 &>
EOF
		      expect => <<'EOF',
Error: Error during compilation of /home/autarch/mason-CVS/mason/dist/mason_tests/comps/errors/_make_error_support:
unknown type for argument/attribute 'foo': first character must be $, @, or %
EOF
		    );

    return $group;
}
