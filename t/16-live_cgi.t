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
use Test;

use lib 'lib', File::Spec->catdir('t', 'lib');

require File::Spec->catfile( 't', 'live_server_lib.pl' );

use Apache::test qw(skip_test have_httpd have_module);
skip_test unless have_httpd;

kill_httpd(1);
test_load_apache();

plan(tests => 7);

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
	ok $response->content, <<'EOF';
Basic test.
2 + 2 = 4.
EOF
    }

    {
	my $path = '/comps/print';
	my $response = Apache::test->fetch($path);
	ok $response->content, <<'EOF';
This is first.
This is second.
This is third.
EOF
    }

    {
	my $path = '/comps/print/autoflush';
	my $response = Apache::test->fetch($path);
	ok $response->content, <<'EOF';
This is first.
This is second.
This is third.
EOF
    }

    {
	my $path = '/comps/print/handle_comp';
	my $response = Apache::test->fetch($path);
	ok $response->content, <<'EOF';
This is first.
This is second.
This is third.
EOF
    }

    {
	my $path = '/comps/print/handle_cgi_object';
	my $response = Apache::test->fetch($path);
	ok $response->content, <<'EOF';
This is first.
This is second.
This is third.
EOF
    }

    {
	my $path = '/comps/cgi_foo_param/handle_cgi_object';
	my $response = Apache::test->fetch($path);
	ok $response->content, <<'EOF';
CGI foo param is bar
EOF
    }

    {
	my $path = '/comps/redirect';
	my $response = Apache::test->fetch($path);
	ok $response->content, <<'EOF';
Basic test.
2 + 2 = 4.
EOF
    }


    kill_httpd();
}
