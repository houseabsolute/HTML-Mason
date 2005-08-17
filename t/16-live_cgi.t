#!/usr/bin/perl -w

use strict;

use Module::Build;

my $test_data = Module::Build->current->notes('test_data');

unless ($test_data->{is_maintainer} &&
        $test_data->{apache_dir} &&
        -e "$test_data->{apache_dir}/CGIHandler.cgi")
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

# needed for Apache::test->fetch to work
local $ENV{PORT} = $test_data->{port};

kill_httpd(1);
test_load_apache();

plan(tests => 13);

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

    write_comp( 'error_as_html', <<'EOF',
% my $x = undef; @$x;
EOF
              );

    write_comp( 'abort_with_ok', <<'EOF',
All is well
% $m->abort(200);
Will not be seen
EOF
              );

    write_comp( 'abort_with_not_ok', <<'EOF',
All is well
% $m->abort(500);
Will not be seen
EOF
              );

    write_comp( 'foo/dhandler', <<'EOF',
dhandler
% $m->decline;
EOF
              );

    write_comp( 'cgi_dh/dhandler', <<'EOF' );
dhandler
dhandler_arg = <% $m->dhandler_arg %>
EOF

    write_comp( 'cgi_dh/file', <<'EOF' );
file
dhandler_arg = <% $m->dhandler_arg %>
path_info = <% $ENV{PATH_INFO} %>
EOF

    write_comp( 'cgi_dh/dir/file', '' );

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

    {
        my $path = '/comps/error_as_html';
        my $response = Apache::test->fetch($path);
        ok $response->content, qr{<b>error:</b>.*Error during compilation}s;
    }

    {
        my $path = '/comps/abort_with_ok';
        my $response = Apache::test->fetch($path);
        ok $response->content, <<'EOF';
All is well
EOF
    }

    {
        my $path = '/comps/abort_with_not_ok';
        my $response = Apache::test->fetch($path);
        ok $response->content, <<'EOF';
All is well
EOF
    }

    # Having decline generate an error like this is bad, but there's
    # not much else we can do without rewriting more of CGIHandler,
    # which isn't a good idea for stable, methinks.
    {
        my $path = '/comps/foo/will_decline';
        my $response = Apache::test->fetch($path);
        ok $response->content, qr{could not find component for initial path}is;
    }

## CGIHandler.pm does not do this the same as ApacheHandler.pm
## but we do not want to rewrite CGIHandler in stable
#    {
#       my $path = '/comps/cgi_dh/file/extra/stuff';
#        my $response = Apache::test->fetch($path);
#        ok $response->content, <<'EOF';
#file
#dhandler_arg = 
#path_info = /extra/stuff
#EOF
#    }

    {
        my $path = '/comps/cgi_dh/dir/extra/stuff';
        my $response = Apache::test->fetch($path);
        ok $response->content, <<'EOF';
dhandler
dhandler_arg = dir/extra/stuff
EOF
    }

    kill_httpd();
}
