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
    my $both_no_handler_tests = 8;
    my $cgi_only_no_handler_tests = 1;
    $cgi_only_no_handler_tests++ if $mod_perl::VERSION >= 1.24;
    my $apr_only_no_handler_tests = 3;
    my $multi_conf_tests = 4;

    my $total = $both_tests + $both_no_handler_tests;
    $total += $cgi_only_tests + $cgi_only_no_handler_tests;
    if ($has_apache_request)
    {
	$total += $both_tests + $both_no_handler_tests;
	$total += $apr_only_tests;
	$total += $apr_only_no_handler_tests;
    }

    $total += $multi_conf_tests;

    print "1..$total\n";
}

print STDERR "\n";

write_test_comps();

cleanup_data_dir();
cgi_tests(1);

cleanup_data_dir();
cgi_tests(0);


if ($has_apache_request)
{
    cleanup_data_dir();
    apache_request_tests(1);

    cleanup_data_dir();
    apache_request_tests(0);
}

cleanup_data_dir();

# This is a hack but otherwise the following tests fail if the Apache
# server runs as any user other than root.  In real life, a user using
# the multi-config option with httpd.conf must handle the file
# permissions manually.
if ( $> == 0 || $< == 0 )
{
    chmod 0777, "$ENV{APACHE_DIR}/data";
}
multi_conf_tests();

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

    write_comp( 'multiconf1/foo', <<'EOF',
I am foo in multiconf1
comp root is <% $m->interp->comp_root =~ m,/comps/multiconf1$, ? 'multiconf1' : $m->interp->comp_root %>
EOF
	      );

    write_comp( 'multiconf1/autohandler', <<'EOF'
<& $m->fetch_next, autohandler => 'present' &>
EOF
	      );

    write_comp( 'multiconf1/autohandler_test', <<'EOF'
<%args>
$autohandler => 'absent'
</%args>
autohandler is <% $autohandler %>
EOF
	      );


    write_comp( 'multiconf2/foo', <<'EOF',
I am foo in multiconf2
comp root is <% $m->interp->comp_root =~ m,/comps/multiconf2$, ? 'multiconf2' : $m->interp->comp_root %>
EOF
	      );

    write_comp( 'multiconf2/dhandler', <<'EOF',
This should not work
EOF
	      );

    write_comp( 'allow_globals', <<'EOF',
% $foo = 1;
% @bar = ( qw( a b c ) );
$foo is <% $foo %>
@bar is <% @bar %>
EOF
	      );

    write_comp( '__top_level_predicate', <<'EOF',
Shouldn't ever run
EOF
	      );

    write_comp( 'decline_dirs', <<'EOF',
decline_dirs is <% $HTML::Mason::ApacheHandler::AH->decline_dirs %>
EOF
	      );
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

# by wiping out the subdirectories here we can catch permissions
# issues if some of the tests can't write to the data dir.
sub cleanup_data_dir
{
    local *DIR;
    opendir DIR, "$ENV{APACHE_DIR}/data"
	or die "Can't open $ENV{APACHE_DIR}/data dir: $!";
    foreach ( grep { -d "$ENV{APACHE_DIR}/data/$_" && $_ !~ /^\./ } readdir DIR )
    {
	rmtree("$ENV{APACHE_DIR}/data/$_");
    }
    closedir DIR;
}

sub cgi_tests
{
    my $with_handler = shift;

    my $def = $with_handler ? 'CGI' : 'CGI_no_handler';
    start_httpd($def);

    standard_tests($with_handler);

    my $path = '/comps/cgi_object';
    $path = "/ah=0$path" if $with_handler;

    my $response = Apache::test->fetch($path);
    my $actual = filter_response($response, $with_handler);
    my $success = HTML::Mason::Tests->check_output( actual => $actual,
						    expect => <<'EOF',
X-Mason-Test: Initial value
CGI
Status code: 0
EOF
						  );
    ok($success);

    if (! $with_handler && $mod_perl::VERSION >= 1.24)
    {
	# test that MasonAllowGlobals works (testing a list parameter
	# from httpd.conf)
	my $response = Apache::test->fetch('/comps/allow_globals');
	my $actual = filter_response($response, 0);
	my $success = HTML::Mason::Tests->check_output( actual => $actual,
							expect => <<'EOF',
X-Mason-Test: Initial value
$foo is 1
@bar is abc
Status code: 0
EOF
						      );
	ok($success);
    }

    kill_httpd(1);
}

sub apache_request_tests
{
    my $with_handler = shift;

    my $def = $with_handler ? 'mod_perl' : 'mod_perl_no_handler';
    start_httpd($def);

    standard_tests($with_handler);

    my $path = '/comps/apache_request';
    $path = "/ah=0$path" if $with_handler;

    my $response = Apache::test->fetch($path);
    my $actual = filter_response($response, $with_handler);
    my $success = HTML::Mason::Tests->check_output( actual => $actual,
						    expect => <<'EOF',
X-Mason-Test: Initial value
Apache::Request
Status code: 0
EOF
						  );
    ok($success);

    unless ($with_handler)
    {
	# test that MasonDieHandler works (testing a code parameter
	# from httpd.conf)
	my $response = Apache::test->fetch('/comps/__top_level_predicate');
	my $actual = filter_response($response, 0);
	ok( $actual =~ /404 not found/,
	    'top level predicate should have refused request' );

	$response = Apache::test->fetch('/comps/decline_dirs');
	$actual = filter_response($response, $with_handler);
	$success = HTML::Mason::Tests->check_output( actual => $actual,
						     expect => <<'EOF',
X-Mason-Test: Initial value
decline_dirs is 0
Status code: 0
EOF
						   );
	ok($success);
    }

    kill_httpd(1);
}

sub standard_tests
{
    my $with_handler = shift;

    my $path = '/comps/basic';
    $path = "/ah=0$path" if $with_handler;

    my $response = Apache::test->fetch($path);
    my $actual = filter_response($response, $with_handler);
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

    $path = '/comps/headers';
    $path = "/ah=0$path" if $with_handler;

    $response = Apache::test->fetch($path);
    $actual = filter_response($response, $with_handler);
    $success = HTML::Mason::Tests->check_output( actual => $actual,
						 expect => <<'EOF',
X-Mason-Test: New value 3


Blah blah
blah
Status code: 0
EOF
					       );
    ok($success);

    if ($with_handler)
    {
	$response = Apache::test->fetch( "/ah=1/comps/headers" );
	$actual = filter_response($response, $with_handler);
	$success = HTML::Mason::Tests->check_output( actual => $actual,
						     expect => <<'EOF',
X-Mason-Test: New value 2


Blah blah
blah
Status code: 0
EOF
						   );
	ok($success);
    }

    $path = '/comps/headers?blank=1';
    $path = "/ah=0$path" if $with_handler;

    $response = Apache::test->fetch($path);
    $actual = filter_response($response, $with_handler);
    $success = HTML::Mason::Tests->check_output( actual => $actual,
						 expect => <<'EOF',
X-Mason-Test: New value 1
Status code: 0
EOF
					       );
    ok($success);

    if ($with_handler)
    {
	$response = Apache::test->fetch( "/ah=1/comps/headers?blank=1" );
	$actual = filter_response($response, $with_handler);
	$success = HTML::Mason::Tests->check_output( actual => $actual,
						     expect => <<'EOF',
X-Mason-Test: New value 1
Status code: 0
EOF
						   );
	ok($success);
    }

    $path = '/comps/_underscore';
    $path = "/ah=0$path" if $with_handler;

    $response = Apache::test->fetch($path);
    $actual = filter_response($response, $with_handler);
    $success = HTML::Mason::Tests->check_output( actual => $actual,
						 expect => <<'EOF',
X-Mason-Test: Initial value
I am underscore.
Status code: 0
EOF
					       );
    ok($success);

    if ($with_handler)
    {
	# top_level_predicate should reject this request.
	$response = Apache::test->fetch( "/ah=2/comps/_underscore" );
	$actual = filter_response($response, $with_handler);
	$success = HTML::Mason::Tests->check_output( actual => $actual,
						     expect => <<'EOF',
X-Mason-Test: 
Status code: 404
EOF
						   );
	ok($success);
    }

    $path = '/comps/die';
    $path = "/ah=0$path" if $with_handler;

    # error_mode is html so we get lots of stuff
    $response = Apache::test->fetch($path);
    $actual = filter_response($response, $with_handler);
    ok( $actual =~ m|error while executing /die:\s+Mine heart is pierced|,
	"Error should have said 'Mine heart is pierced'" );

    if ($with_handler)
    {
	# error_mode is fatal so we just get a 500
	$response = Apache::test->fetch( "/ah=4/comps/die" );
	$actual = filter_response($response, $with_handler);
	ok( $actual =~ m|500 Internal Server Error|,
	    "die should have generated 500 error" );
    }

    $path = '/comps/params?qs1=foo&qs2=bar&foo=A&foo=B';
    $path = "/ah=0$path" if $with_handler;

    # params in query string only
    $response = Apache::test->fetch($path);
    $actual = filter_response($response, $with_handler);
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

    $path = '/comps/params';
    $path = "/ah=0$path" if $with_handler;

    # params as POST only
    $response = Apache::test->fetch( { uri => $path,
				       method => 'POST',
				       content => 'post1=foo&post2=bar&foo=A&foo=B',
				     } );
    $actual = filter_response($response, $with_handler);
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

    $path = '/comps/params?qs1=foo&qs2=bar&mixed=A';
    $path = "/ah=0$path" if $with_handler;

    # params mixed in query string and POST
    $response = Apache::test->fetch( { uri => $path,
				       method => 'POST',
				       content => 'post1=a&post2=b&mixed=B',
				     } );
    $actual = filter_response($response, $with_handler);
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

sub multi_conf_tests
{
    start_httpd('multi_config');

    my $response = Apache::test->fetch('/comps/multiconf1/foo');
    my $actual = filter_response($response, 0);
    my $success = HTML::Mason::Tests->check_output( actual => $actual,
						    expect => <<'EOF',
X-Mason-Test: Initial value
I am foo in multiconf1
comp root is multiconf1
Status code: 0
EOF
						  );
    ok($success);

    $response = Apache::test->fetch('/comps/multiconf1/autohandler_test');
    $actual = filter_response($response, 0);
    $success = HTML::Mason::Tests->check_output( actual => $actual,
						 expect => <<'EOF',
X-Mason-Test: Initial value
autohandler is absent
Status code: 0
EOF
						  );
    ok($success);

    $response = Apache::test->fetch('/comps/multiconf2/foo');
    $actual = filter_response($response, 0);
    $success = HTML::Mason::Tests->check_output( actual => $actual,
						 expect => <<'EOF',
X-Mason-Test: Initial value
I am foo in multiconf2
comp root is multiconf2
Status code: 0
EOF
					       );
    ok($success);

    $response = Apache::test->fetch('/comps/multiconf2/dhandler_test');
    $actual = filter_response($response, 0);
    ok( $actual =~ /404 not found/i,
	"Attempt to request a non-existent component should not work with dhandlers turned off" );

    kill_httpd(1);
}

# We're not interested in headers that are always going to be
# different (like date or server type).
sub filter_response
{
    my $response = shift;

    my $with_handler = shift;

    # because the header or content may be undef
    local $^W = 0;
    my $actual = join "\n", ( 'X-Mason-Test: ' .
			      # hack until I make a separate test
			      # suite for the httpd.conf configuration
			      # stuff
			      ( $with_handler ?
				$response->headers->header('X-Mason-Test') :
				( $response->headers->header('X-Mason-Test') ?
				  $response->headers->header('X-Mason-Test') :
				  'Initial value' ) ),
			      $response->content );

    my $code = $response->code == 200 ? 0 : $response->code;
    $actual .= "Status code: $code" unless $with_handler;

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
    my $wait = shift;

    open PID, "$ENV{APACHE_DIR}/httpd.pid"
	or die "Can't open '$ENV{APACHE_DIR}/httpd.pid': $!";
    my $pid = <PID>;
    close PID;
    chomp $pid;

    print STDERR "Killing httpd process ($pid)\n";
    kill 15, $pid
	or die "Can't kill process $pid: $!";

    if ($wait)
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
    }
}

use vars qw($TESTS);

sub ok
{
    my $ok = !!shift;
    print $ok ? 'ok ' : 'not ok ';
    print ++$TESTS, "\n";
}
