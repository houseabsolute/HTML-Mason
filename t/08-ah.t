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

use vars qw($VERBOSE $DEBUG $TEST_NUM $CONF_DIR $COMP_ROOT $DATA_DIR %APACHE_PARAMS);

BEGIN
{
    $VERBOSE = $ENV{MASON_DEBUG} || $ENV{MASON_VERBOSE};
    $DEBUG = $ENV{MASON_DEBUG};
}

use HTML::Mason::Tests;

use lib 'lib', 't/lib';

use Apache::test qw(skip_test have_httpd);
skip_test unless have_httpd;

# We'll repeat all the tests with an Apache::Request using
# ApacheHandler if the user has the Apache::Request module installed.
my $has_apache_request = 1;
eval { require Apache::Request; };
$has_apache_request = 0 if $@;

local $| = 1;

print '1..';
print $has_apache_request ? 12 : 6;
print "\n";

$TEST_NUM = 0;

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

sub cgi_tests
{
    start_httpd('CGI');

    standard_tests();

    my $response = Apache::test->fetch( '/mason/comps/cgi_object' );
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

    my $response = Apache::test->fetch( '/mason/comps/apache_request' );
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

# We're not interested in headers that are always going to be
# different (like date or server type).
sub filter_response
{
    my $response = shift;

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

sub ok
{
    my $ok = !!shift;
    print $ok ? 'ok ' : 'not ok ';
    print ++$TEST_NUM, "\n";
}

