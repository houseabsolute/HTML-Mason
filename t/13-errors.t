#!/usr/bin/perl -w

use strict;

use File::Spec;
use HTML::Mason::Tests;
use HTML::Mason::Tools qw(load_pkg);

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

    $group->add_test( name => 'bad_args',
		      description => 'Make sure a bad args line is caught properly',
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

    # This fails as root because the file will always be readable, but
    # we can't know that it will fail until we're inside the test.  So
    # we'll just run this test for developers, not end users.
    if ( $ENV{MASON_MAINTAINER} )
    {
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
    or die "Cannot chmod file for " . '$file' . ": \$!";
\$m->comp('support/unreadable');
</%init>
EOF
                          expect_error => q|Permission denied|
                        );
    }

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

    # this is easy to check for as an exact string
    sub HTML::Mason::Exception::as_munged
    {
        my $err = shift->error;

        return $err =~ /^(.+?) at/ ? $1 : $err;
    }

    $group->add_test( name => 'error_in_subrequest',
		      description => 'Make sure that an error in a subrequest is propogated back to the main request',
                      interp_params => { error_format => 'munged',
                                         error_mode => 'output',
                                       },
		      component => <<'EOF',
Should not appear in output!
% $m->subexec( '/errors/support/error1' );
EOF
                      expect => <<'EOF',
terrible error
EOF
		    );

#------------------------------------------------------------

    if ( load_pkg('HTML::Entities') )
    {
        $group->add_test( name => 'check_error_format',
                          description => 'Make sure setting error_format => "html" works',
                          interp_params => { error_format => 'html',
                                             error_mode => 'output',
                                           },
                          component => <<'EOF',
% die("Horrible death");
EOF
                          expect => qr{^\s+<html>.*Horrible death}is,
                        );
    }

#------------------------------------------------------------

    $group->add_test( name => 'change_error_format',
		      description => 'Make sure setting $m->error_format($foo) works on the fly',
                      interp_params => { error_format => 'html',
					 error_mode => 'output',
                                       },
		      component => <<'EOF',
% $m->error_format('text');
% die("Horrible death");
EOF
                      expect => qr{^Horrible death},
		    );

#------------------------------------------------------------

    $group->add_test( name => 'check_error_format_brief',
		      description => 'Make sure setting error_format => "brief" works',
		      interp_params => { error_format => 'brief',
					 error_mode => 'output',
				       },
		      component => <<'EOF',
% die("Horrible death");
EOF
		      expect => qr{^Horrible death at .*check_error_format_brief line \d+\.$}s,
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
