#!/usr/bin/perl -w

# Skip test if no mod_perl
eval { require mod_perl };
# need to use it twice to avoid annoying warning
unless ($mod_perl::VERSION || $mod_perl::VERSION)
{
    print "1..0\n";
    exit;
}

use strict;

use vars qw($VERBOSE $DEBUG);

BEGIN
{
    $VERBOSE = $ENV{MASON_DEBUG} || $ENV{MASON_VERBOSE};
    $DEBUG = $ENV{MASON_DEBUG};
}

use File::Basename;
use File::Path;
use HTML::Mason::Tests;

use lib 'lib', 't/lib';

use Apache::test qw(skip_test have_httpd);
skip_test unless have_httpd;

# We'll repeat all the tests with an Apache::Request-using
# ApacheHandler if the user has the Apache::Request module installed.
my $has_apache_request = 1;
eval { require Apache::Request; };
$has_apache_request = 0 if $@;

local $| = 1;

{
    my $both_tests = 12;
    my $cgi_only_tests = 1;
    my $apr_only_tests = 1;

    my $total = $both_tests;
    $total += $cgi_only_tests;
    if ($has_apache_request)
    {
	$total += $both_tests;
	$total += $apr_only_tests;
    }

    print "1..$total\n";
}

write_test_comps();

print STDERR "\n";
cgi_tests();

if ($has_apache_request)
{
    print STDERR "Waiting for previous httpd to shut down\n";
    my $x = 0;
    while ( -e "$ENV{APACHE_DIR}/httpd.pid" )
    {
	sleep (1);
	$x++;
	if ( $x > 10 )
	{
	    die "$ENV{APACHE_DIR}t/httpd.pid file still exists after 10 seconds.  Exiting.";
	}
    }

    apache_request_tests();
}

sub write_test_comps
{
    write_comp( 'basic', <<'EOF',
Basic test.
2 + 2 = <% 2 + 2 %>.
uri = <% $r->uri =~ /basic$/ ? '/basic' : $r->uri %>.
method = <% $r->method %>.


EOF
	      );

    write_comp( 'headers', <<'EOF',


% $r->header_out('X-Mason-Test' => 'New value 2');
Blah blah
blah
% $r->header_out('X-Mason-Test' => 'New value 3');
<%init>
$r->header_out('X-Mason-Test' => 'New value 1');
$m->abort if $blank;
</%init>
<%args>
$blank=>0
</%args>
EOF
	      );

    write_comp( 'cgi_object', <<'EOF',
<% UNIVERSAL::isa(eval { $m->cgi_object }, 'CGI') ? 'CGI' : 'NO CGI' %>
EOF
	      );

    write_comp( 'params', <<'EOF',
% foreach (sort keys %ARGS) {
<% $_ %>: <% ref $ARGS{$_} ? join ', ', sort @{ $ARGS{$_} }, 'array' : $ARGS{$_} %>
% }
EOF
	      );

    write_comp( '_underscore', <<'EOF',
I am underscore.
EOF
	      );

    write_comp( 'dhandler/dhandler', <<'EOF',
I am the dhandler.
EOF
	      );

    write_comp( 'die', <<'EOF',
% die 'Mine heart is pierced';
EOF
	      );

    if ($has_apache_request)
    {
	write_comp( 'apache_request', <<'EOF',
<% ref $r %>
EOF
		  );
    }
}

sub write_comp
{
    my $name = shift;
    my $comp = shift;

    my $file = "$ENV{APACHE_DIR}/comps/$name";
    my $dir = dirname($file);
    mkpath( $dir, 0, 0755 ) unless -d $dir;

    open F, ">$file"
	or die "Can't write to '$file': $!";

    print F $comp;

    close F;
}


sub cgi_tests
{
    start_httpd('CGI');

    standard_tests();

    my $response = Apache::test->fetch( '/ah=0/comps/cgi_object' );
    my $actual = filter_response($response);
    my $success = HTML::Mason::Tests->check_output( actual => $actual,
						    expect => <<'EOF',
X-Mason-Test: Initial value
CGI
Status code: 0
EOF
						  );
    ok($success);

    kill_httpd();
}

sub apache_request_tests
{
    start_httpd('mod_perl');

    standard_tests();

    my $response = Apache::test->fetch( '/ah=0/comps/apache_request' );
    my $actual = filter_response($response);
    my $success = HTML::Mason::Tests->check_output( actual => $actual,
						    expect => <<'EOF',
X-Mason-Test: Initial value
Apache::Request
Status code: 0
EOF
						  );
    ok($success);

    kill_httpd();
}

sub standard_tests
{
    my $response = Apache::test->fetch( "/mason/comps/basic" );
    my $actual = filter_response($response);
    my $success = HTML::Mason::Tests->check_output( actual => $actual,
						    expect => <<'EOF',
X-Mason-Test: Initial value
Basic test.
2 + 2 = 4.
uri = /basic.
method = GET.


Status code: 0
EOF
						  );
    ok($success);

    $response = Apache::test->fetch( "/mason/comps/headers" );
    $actual = filter_response($response);
    $success = HTML::Mason::Tests->check_output( actual => $actual,
						 expect => <<'EOF',
X-Mason-Test: New value 3


Blah blah
blah
Status code: 0
EOF
					       );
    ok($success);

    $response = Apache::test->fetch( "/mason_stream/comps/headers" );
    $actual = filter_response($response);
    $success = HTML::Mason::Tests->check_output( actual => $actual,
						 expect => <<'EOF',
X-Mason-Test: New value 2


Blah blah
blah
Status code: 0
EOF
					       );
    ok($success);

    $response = Apache::test->fetch( "/mason/comps/headers?blank=1" );
    $actual = filter_response($response);
    $success = HTML::Mason::Tests->check_output( actual => $actual,
						 expect => <<'EOF',
X-Mason-Test: New value 1
Status code: 0
EOF
					       );
    ok($success);

    $response = Apache::test->fetch( "/mason_stream/comps/headers?blank=1" );
    $actual = filter_response($response);
    $success = HTML::Mason::Tests->check_output( actual => $actual,
						 expect => <<'EOF',
X-Mason-Test: New value 1
Status code: 0
EOF
					       );
    ok($success);
}

sub standard_tests
{
    my $response = Apache::test->fetch( "/ah=0/comps/basic" );
    my $actual = filter_response($response);
    my $success = HTML::Mason::Tests->check_output( actual => $actual,
						    expect => <<'EOF',
X-Mason-Test: Initial value
Basic test.
2 + 2 = 4.
uri = /basic.
method = GET.


Status code: 0
EOF
						  );
    ok($success);

    $response = Apache::test->fetch( "/ah=0/comps/headers" );
    $actual = filter_response($response);
    $success = HTML::Mason::Tests->check_output( actual => $actual,
						 expect => <<'EOF',
X-Mason-Test: New value 3


Blah blah
blah
Status code: 0
EOF
					       );
    ok($success);

    $response = Apache::test->fetch( "/ah=1/comps/headers" );
    $actual = filter_response($response);
    $success = HTML::Mason::Tests->check_output( actual => $actual,
						 expect => <<'EOF',
X-Mason-Test: New value 2


Blah blah
blah
Status code: 0
EOF
					       );
    ok($success);

    $response = Apache::test->fetch( "/ah=0/comps/headers?blank=1" );
    $actual = filter_response($response);
    $success = HTML::Mason::Tests->check_output( actual => $actual,
						 expect => <<'EOF',
X-Mason-Test: New value 1
Status code: 0
EOF
					       );
    ok($success);

    $response = Apache::test->fetch( "/ah=1/comps/headers?blank=1" );
    $actual = filter_response($response);
    $success = HTML::Mason::Tests->check_output( actual => $actual,
						 expect => <<'EOF',
X-Mason-Test: New value 1
Status code: 0
EOF
					       );
    ok($success);

    $response = Apache::test->fetch( "/ah=0/comps/_underscore" );
    $actual = filter_response($response);
    $success = HTML::Mason::Tests->check_output( actual => $actual,
						 expect => <<'EOF',
X-Mason-Test: Initial value
I am underscore.
Status code: 0
EOF
					       );
    ok($success);

    # top_level_predicate should reject this request.
    $response = Apache::test->fetch( "/ah=2/comps/_underscore" );
    $actual = filter_response($response);
    $success = HTML::Mason::Tests->check_output( actual => $actual,
						 expect => <<'EOF',
X-Mason-Test: 
Status code: 404
EOF
					       );
    ok($success);


    # error_mode is html so we get lots of stuff
    $response = Apache::test->fetch( "/ah=0/comps/die" );
    $actual = filter_response($response);
    ok( $actual =~ m,error while executing /die:\s+Mine heart is pierced, );

    # error_mode is fatal so we just get a 500
    $response = Apache::test->fetch( "/ah=4/comps/die" );
    $actual = filter_response($response);
    ok( $actual =~ m,500 Internal Server Error, );

    # params in query string only
    $response = Apache::test->fetch( '/ah=0/comps/params?qs1=foo&qs2=bar&foo=A&foo=B' );
    $actual = filter_response($response);
    $success = HTML::Mason::Tests->check_output( actual => $actual,
						 expect => <<'EOF',
X-Mason-Test: Initial value
foo: A, B, array
qs1: foo
qs2: bar
Status code: 0
EOF
						  );
    ok($success);

    # params as POST only
    $response = Apache::test->fetch( { uri =>  '/ah=0/comps/params',
				       method => 'POST',
				       content => 'post1=foo&post2=bar&foo=A&foo=B',
				     } );
    $actual = filter_response($response);
    $success = HTML::Mason::Tests->check_output( actual => $actual,
						 expect => <<'EOF',
X-Mason-Test: Initial value
foo: A, B, array
post1: foo
post2: bar
Status code: 0
EOF
						  );
    ok($success);

    # params mixed in query string and POST
    $response = Apache::test->fetch( { uri =>  '/ah=0/comps/params?qs1=foo&qs2=bar&mixed=A',
				       method => 'POST',
				       content => 'post1=a&post2=b&mixed=B',
				     } );
    $actual = filter_response($response);
    $success = HTML::Mason::Tests->check_output( actual => $actual,
						 expect => <<'EOF',
X-Mason-Test: Initial value
mixed: A, B, array
post1: a
post2: b
qs1: foo
qs2: bar
Status code: 0
EOF
						  );
    ok($success);
}


# We're not interested in headers that are always going to be
# different (like date or server type).
sub filter_response
{
    my $response = shift;

    # because the header or content may be undef
    local $^W = 0;
    my $actual = join "\n", ( 'X-Mason-Test: ' . $response->headers->header('X-Mason-Test'),
			      $response->content );
    return $actual;
}

sub start_httpd
{
    my $def = shift;

    my $cmd ="$ENV{APACHE_DIR}/httpd -D$def -f $ENV{APACHE_DIR}/httpd.conf";
    print STDERR "Executing $cmd\n";
    system ($cmd)
	and die "Can't start httpd server as '$cmd': $!";

    my $x = 0;
    print STDERR "Waiting for httpd to start.\n";
    until ( -e 't/httpd.pid' )
    {
	sleep (1);
	$x++;
	if ( $x > 10 )
	{
	    die "No t/httpd.pid file has appeared after 10 seconds.  Exiting.";
	}
    }
}

sub kill_httpd
{
    open PID, "$ENV{APACHE_DIR}/httpd.pid"
	or die "Can't open '$ENV{APACHE_DIR}/httpd.pid': $!";
    my $pid = <PID>;
    close PID;
    chomp $pid;

    print STDERR "Killing httpd process ($pid)\n";
    kill 15, $pid
	or die "Can't kill process $pid: $!";
}

use vars qw($TESTS);

sub ok
{
    my $ok = !!shift;
    print $ok ? 'ok ' : 'not ok ';
    print ++$TESTS, "\n";
}


