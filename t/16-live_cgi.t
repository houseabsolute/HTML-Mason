#!/usr/bin/perl -w

use strict;

unless ($ENV{MASON_MAINTAINER} && -e "$ENV{APACHE_DIR}/CGIHandler.cgi")
{
    print "1..0\n";
    exit;
}

use vars qw($VERBOSE $DEBUG);

BEGIN
{
    $VERBOSE = $ENV{MASON_DEBUG} || $ENV{MASON_VERBOSE};
    $DEBUG = $ENV{MASON_DEBUG};
}

use File::Basename;
use File::Path;
use File::Spec;
use HTML::Mason::Tests;

use lib 'lib', File::Spec->catdir('t', 'lib');

require File::Spec->catfile( 't', 'live_server_lib.pl' );

use Apache::test qw(skip_test have_httpd have_module);
skip_test unless have_httpd;

local $| = 1;

kill_httpd(1);
test_load_apache();

print "1..7\n";

print STDERR "\n";

write_test_comps();

run_tests();

sub write_test_comps
{
    write_comp( 'basic', <<'EOF',
Basic test.
2 + 2 = <% 2 + 2 %>.
EOF
	      );

    write_comp( 'cgi_foo_param', <<'EOF',
CGI foo param is <% $r->query->param('foo') %>
EOF
	      );

    write_comp( 'print', <<'EOF',
This is first.
% print "This is second.\n";
This is third.
EOF
	      );

    write_comp( 'redirect', <<'EOF',
<%init>
$m->redirect('/comps/basic');
</%init>
EOF
	      );
}

sub run_tests
{
    start_httpd('CGIHandler');

    {
	my $path = '/comps/basic';
	my $response = Apache::test->fetch($path);
	my $success = HTML::Mason::Tests->check_output( actual => $response->content,
							expect => <<'EOF',
Basic test.
2 + 2 = 4.
EOF
						      );

	ok($success);
    }

    {
	my $path = '/comps/print';
	my $response = Apache::test->fetch($path);
	my $success = HTML::Mason::Tests->check_output( actual => $response->content,
							expect => <<'EOF',
This is first.
This is second.
This is third.
EOF
						      );

	ok($success);
    }

    {
	my $path = '/comps/print/autoflush';
	my $response = Apache::test->fetch($path);
	my $success = HTML::Mason::Tests->check_output( actual => $response->content,
							expect => <<'EOF',
This is first.
This is second.
This is third.
EOF
						      );

	ok($success);
    }

    {
	my $path = '/comps/print/handle_comp';
	my $response = Apache::test->fetch($path);
	my $success = HTML::Mason::Tests->check_output( actual => $response->content,
							expect => <<'EOF',
This is first.
This is second.
This is third.
EOF
						      );

	ok($success);
    }

    {
	my $path = '/comps/print/handle_cgi_object';
	my $response = Apache::test->fetch($path);
	my $success = HTML::Mason::Tests->check_output( actual => $response->content,
							expect => <<'EOF',
This is first.
This is second.
This is third.
EOF
						      );

	ok($success);
    }

    {
	my $path = '/comps/cgi_foo_param/handle_cgi_object';
	my $response = Apache::test->fetch($path);
	my $success = HTML::Mason::Tests->check_output( actual => $response->content,
							expect => <<'EOF',
CGI foo param is bar
EOF
						      );

	ok($success);
    }

    {
	my $path = '/comps/redirect';
	my $response = Apache::test->fetch($path);
	my $success = HTML::Mason::Tests->check_output( actual => $response->content,
							expect => <<'EOF',
Basic test.
2 + 2 = 4.
EOF
						      );

	ok($success);
    }


    kill_httpd();
}
