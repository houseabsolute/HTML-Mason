#!/usr/bin/perl -w

use strict;

use HTML::Mason::Tests;

my $tests = make_tests();
$tests->run;

sub make_tests
{
    my $group = HTML::Mason::Tests->new( name => 'errors',
					 description => 'Test that errors are generated properly' );

    $group->add_test( name => '_make_error',
		      description => 'Exercise possible failure for Parser.pm _make_error method',
		      component => <<'EOF',
<%args>
foo
</%args>
EOF
		      expect_error => quotemeta q|unknown type for argument/attribute 'foo': first character must be $, @, or %|
		    );

    return $group;
}
