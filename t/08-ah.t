#!/usr/bin/perl -w
BEGIN { $HTML::Mason::IN_DEBUG_FILE = 1 if !$HTML::Mason::IN_DEBUG_FILE }
use Cwd;
use CGI;
use strict;
use vars (qw($root $branch $comp_root $data_dir));

# Skip test if no mod_perl
eval { require mod_perl };
my @use = ($mod_perl::VERSION);
unless ($mod_perl::VERSION) {
    print "1..0\n";
    exit;
}

$branch = "ah";
my $pwd = cwd();
$root = (-f "test-common.pl") ? "$pwd/.." : (-f "t/test-common.pl") ? "$pwd" : die "ERROR: cannot find test-common.pl\n";
unshift(@INC,"$root/lib");

require "$root/t/test-common.pl";

require HTML::Mason::ApacheHandler;

init();

sub fake_apache {
    my ($options) = @_;
    die "must specify uri" if !$options->{uri};
    my $dref = {
	'method' => 'GET',
	'document_root' => "$comp_root/ah",
	'header_only' => 0,
	'args@' => [],
	'connection' => {
	    'remote_ip' => '127.0.0.1',
	},
	'uri' => $options->{uri},
	'headers_in' => {
	    'User-Agent' => 'Mozilla/4.61 [en] (X11; I; Linux 2.2.12-20 i686)',
	},
	'protocol' => 'HTTP/1.0',
	'dir_config' => {},
	'server' => {
	    'server_hostname' => 'www.foo.com',
	},
	'content_type' => 'text/html',
	'ENV' => {
	},
	"args\$" => undef,
	%$options
    };
    $dref->{filename} = $dref->{document_root}.$dref->{uri};
    my $r = HTML::Mason::ApacheHandler::simulate_debug_request($dref);
    return $r;
}

sub try_exec_with_ah {
    my ($uri,$test_name,$ah_options,$r_options) = @_;
    my $buf;

    # Create standard interpreter.
    my $interp = new HTML::Mason::Interp(comp_root => $comp_root, data_dir => $data_dir, out_method=>\$buf);
    
    # Create new ApacheHandler based on options.
    undef *Apache::Status::status_mason if *Apache::Status::status_mason;
    my $ah = new HTML::Mason::ApacheHandler(interp=>$interp, error_mode=>'fatal', apache_status_title=>"mason_$test_name", %$ah_options);

    # Create fake Apache request.
    my $r = fake_apache ({uri=>$uri, %$r_options});
    undef $ENV{SERVER_SOFTWARE} if defined($ENV{SERVER_SOFTWARE});

    # Override send header function, and supply default header value.
    $r->{headers_out_method} = sub { $buf .= $_[0] };
    $r->headers_out('X-Mason-Test' => 'Initial value');

    # Stop CGI from reinitializing with same params
    CGI::initialize_globals();

    # Handle request.
    my $retval = eval { $ah->handle_request($r) };
    if (my $err = $@) {
	print "ERROR:\n$err\nnot ok\n";
	return;
    }
    $buf .= "Status code: $retval\n";

    # Compare current results with stored results.
    compare_results ($test_name, $buf);
}

print "1..6\n";

try_exec_with_ah('/basic','basic',{},{});

try_exec_with_ah('/headers','headers-batch',{output_mode=>'batch'},{});
try_exec_with_ah('/headers','headers-stream',{output_mode=>'stream'},{});
{ my $qs = 'blank=1';
  local $ENV{QUERY_STRING} = $qs;
  local $ENV{REQUEST_METHOD} = 'GET';
  try_exec_with_ah('/headers','headers-batch-blank',{output_mode=>'batch'},{"args\$" => $qs});
  try_exec_with_ah('/headers','headers-stream-blank',{output_mode=>'stream'},{"args\$" => $qs}); }

{ my $qs = 'scalar=5&list=a&list=b&hash=key&hash=value';
  local $ENV{QUERY_STRING} = $qs;
  local $ENV{REQUEST_METHOD} = 'GET';
  try_exec_with_ah('/args','args',{},{"args\$" => $qs}); }

1;
