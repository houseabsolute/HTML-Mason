#!/usr/bin/perl -w

use strict;

use File::Spec;
use HTML::Mason::Tests;

my $tests = make_tests();
$tests->run;

sub make_tests
{
    my $group = HTML::Mason::Tests->new( name => 'errors',
					 description => 'Test that errors are generated properly' );

#------------------------------------------------------------

    $group->add_support( path => '/support/error_helper',
			 component => <<'EOF',
<%init>
eval { $m->comp('error1')  };
$m->comp('error2');
</%init>
EOF
		       );

#------------------------------------------------------------

    $group->add_support( path => '/support/error1',
			 component => <<'EOF',
% die "terrible error";
EOF
		       );

#------------------------------------------------------------

    $group->add_support( path => '/support/error2',
			 component => <<'EOF',
% die "horrible error";
EOF
		       );

#------------------------------------------------------------

    $group->add_test( name => '_make_error',
		      description => 'Exercise possible failure for Parser.pm _make_error method',
		      component => <<'EOF',
<%args>
foo
</%args>
EOF
		      expect_error => qr|Invalid <%args> section line|
		    );

#------------------------------------------------------------

    $group->add_test( name => 'backtrace',
		      description => 'Make sure trace for second error is accurate when first error is caught by eval',
		      component => <<'EOF',
<%init>
$m->comp('support/error_helper');
</%init>
EOF
		      expect_error => q|horrible error.*|
		    );

#------------------------------------------------------------

    $group->add_support( path => '/support/unreadable',
			 component => <<'EOF',
unreadable
EOF
		       );

    my $file = File::Spec->catfile( $group->comp_root, 'errors', 'support', 'unreadable' );

    $group->add_test( name => 'cannot_read_source',
		      description => 'Make sure that Mason throws a useful error when it cannot read a source file',
		      component => <<"EOF",
<%init>
chmod 0222, '$file'
    or die "Cannot chmod $file to 0222: \$!";
\$m->comp('support/unreadable');
</%init>
EOF
		      expect_error => q|Permission denied|
		    );

#------------------------------------------------------------

    $group->add_support( path => '/support/zero_size',
			 component => '',
		       );

#------------------------------------------------------------

    $group->add_test( name => 'read_zero_size',
		      description => 'Make sure that Mason handles a zero length source file correctly',
		      component => <<'EOF',
zero[<& support/zero_size &>]zero
EOF
		      expect => <<'EOF'
zero[]zero
EOF
		    );

#------------------------------------------------------------

    $group->add_test( name => 'bad_source_callback',
		      description => 'Make sure that a bad source_callback for a ComponentSource object reports a useful error',
                      interp_params => { resolver_class => 'My::Resolver' },
		      component => <<'EOF',
does not matter
EOF
		      expect_error => qr/Undefined subroutine.*will_explode/,
		    );

#------------------------------------------------------------

    $group->add_test( name => 'bad_escape_flag',
		      description => 'Make sure that an invalid escape flag is reported properly',
		      component => <<'EOF',
<% 1234 | abc %>
EOF
		      expect_error => qr/Invalid escape flag: abc/,
		    );

#------------------------------------------------------------

    return $group;
}

package My::Resolver;

use base 'HTML::Mason::Resolver::File';

sub get_info
{
    my $self = shift;

    if ( my $source = $self->SUPER::get_info(@_) )
    {
        $source->{source_callback} = sub { will_explode() };

        return $source;
    }
}
