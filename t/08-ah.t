#!/usr/bin/perl -w
use Cwd;
use strict;
use vars (qw($root $branch $comp_root $data_dir));

$branch = "ah";
my $pwd = cwd();
$root = (-f "test-common.pl") ? "$pwd/.." : (-f "t/test-common.pl") ? "$pwd" : die "ERROR: cannot find test-common.pl\n";
unshift(@INC,"$root/lib");

require "$root/t/test-common.pl";
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
    my $ah = new HTML::Mason::ApacheHandler(interp=>$interp, error_mode=>'fatal', %$ah_options);

    # Create fake Apache request.
    my $r = fake_apache ({uri=>$uri, %$r_options});

    # Override send header function, and supply default header value.
    *HTML::Mason::FakeApache::send_http_header = sub { $buf .= "X-Mason-Test: ".$_[0]->header_out('X-Mason-Test')."\n\n" };
    HTML::Mason::FakeApache::send_http_header() if 0;
    $r->headers_out('X-Mason-Test' => 'Initial value');

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

print "1..5\n";

try_exec_with_ah('/basic','basic',{},{});

try_exec_with_ah('/headers','headers-batch',{output_mode=>'batch'},{});
try_exec_with_ah('/headers','headers-stream',{output_mode=>'stream'},{});

{ my $qs = 'scalar=5&list=a&list=b&hash=key&hash=value';
  local $ENV{QUERY_STRING} = $qs;
  local $ENV{REQUEST_METHOD} = 'GET';
  try_exec_with_ah('/args','args',{},{"args\$" => $qs}); }

try_exec_with_ah('/decline','decline',{},{});

1;
