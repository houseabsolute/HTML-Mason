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

use File::Spec;
use HTML::Mason::Tests;
use Test;

use lib 'lib', File::Spec->catdir('t', 'lib');

require File::Spec->catfile( 't', 'live_server_lib.pl' );

use Apache::test qw(skip_test have_httpd have_module);
skip_test unless have_httpd;

kill_httpd(1);
test_load_apache();

plan(tests => 8);

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

    write_comp( 'params', <<'EOF',
% foreach (sort keys %ARGS) {
<% $_ %>: <% ref $ARGS{$_} ? join ', ', sort @{ $ARGS{$_} }, 'array' : $ARGS{$_} %>
% }
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


    {
        my $path = '/comps/params?qs1=foo&qs2=bar&mixed=A';
        my $response = Apache::test->fetch( { uri => $path,
                                              method => 'POST',
                                              content => 'post1=a&post2=b&mixed=B',
                                            } );
        ok $response->content, <<'EOF',
mixed: A, B, array
post1: a
post2: b
qs1: foo
qs2: bar
EOF
    }

    kill_httpd();
}
