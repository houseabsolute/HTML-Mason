# Copyright (c) 1998 by Jonathan Swartz. All rights reserved.
# This program is free software; you can redistribute it and/or modify it
# under the same terms as Perl itself.

require 5.004;

#
# Apache-specific Request object
#
package HTML::Mason::Request::ApacheHandler;
require Exporter;
use vars qw(@ISA);
@ISA = qw(HTML::Mason::Request);

my %reqfields =
    (ah => undef,
     http_input => undef,
     apache_req => undef,
     );
# Create accessor routines
foreach my $f (keys(%reqfields)) {
    no strict 'refs';
    *{$f} = sub {my $s=shift; return @_ ? ($s->{$f}=shift) : $s->{$f}};
}

sub new
{
    my ($class,%options) = @_;
    my $interp = $options{interp} or die "HTML::Mason::Request::ApacheHandler::new: must specify interp\n";
    delete $options{interp};
    my $self = HTML::Mason::Request::new($class,interp=>$interp);
    while (my ($key,$value) = each(%options)) {
	if (exists($reqfields{$key})) {
	    $self->{$key} = $value;
	} else {
	    die "HTML::Mason::Request::ApacheHandler::new: invalid option '$key'\n";
	}
    }
    return $self;
}

#
# ApacheHandler object
#
package HTML::Mason::ApacheHandler;
require Exporter;
@ISA = qw(Exporter);
@EXPORT = qw();
@EXPORT_OK = qw();

use strict;
#JS - 6/30 - seems to infinite loop when using debug...help?!
#use Apache::Constants qw(OK DECLINED SERVER_ERROR NOT_FOUND);
sub OK { return 0 }
sub DECLINED { return -1 }
sub SERVER_ERROR { return 500 }
sub NOT_FOUND { return 404 }
use Data::Dumper;
use File::Basename;
use File::Path;
use HTML::Mason::Interp;
use HTML::Mason::Commands;
use HTML::Mason::FakeApache;
use HTML::Mason::Tools qw(html_escape url_unescape pkg_installed);
use HTML::Mason::Utils;
use Apache::Status;
use CGI qw(-private_tempfiles);

my @used = ($HTML::Mason::IN_DEBUG_FILE);
	    
my %fields =
    (
     interp => undef,
     output_mode => 'batch',
     error_mode => 'html',
     top_level_predicate => sub { return 1 },
     decline_dirs => 1,
     debug_mode => 'none',
     debug_perl_binary => '/usr/bin/perl',
     debug_handler_script => undef,
     debug_handler_proc => undef,
     debug_dir_config_keys => [],
     apache_status_title => 'mason',
     );
# Minor speedup: create anon. subs to reduce AUTOLOAD calls
foreach my $f (keys %fields) {
    no strict 'refs';
    *{$f} = sub {my $s=shift; return @_ ? ($s->{$f}=shift) : $s->{$f}};
}

sub new
{
    my $class = shift;
    my $self = {
	_permitted => \%fields,
	request_number => 0,
	%fields,
    };
    my (%options) = @_;
    while (my ($key,$value) = each(%options)) {
	if (exists($fields{$key})) {
	    $self->{$key} = $value;
	} else {
	    die "HTML::Mason::ApacheHandler::new: invalid option '$key'\n";
	}
    }
    die "HTML::Mason::ApacheHandler::new: must specify value for interp" if !$self->{interp};
    bless $self, $class;
    $self->_initialize;
    return $self;
}

sub _initialize {
    my ($self) = @_;

    my $interp = $self->interp;

    # ----------------------------
    # Add an HTML::Mason menu item to the /perl-status page. Things we report:
    # -- Interp properties
    # -- loaded (cached) components
    my $name = $self->{apache_status_title};
    my $title;
    if ($name eq 'mason') {
        $title='HTML::Mason status';    #item for HTML::Mason module
    } 
    else {
        $title=$name;
        $name=~s/\W/_/g;
    }

    my $statsub = sub {
	my($r,$q) = @_; #request and CGI objects
	my(@strings);

	push (@strings,
	      qq(<FONT size="+2"><B>$self->{apache_status}</B></FONT><BR><BR>),
	      $self->interp_status);

	return \@strings;     #return an array ref
    };
    Apache::Status->menu_item ($name,$title,$statsub) if $Apache::Status::VERSION;
    
    #
    # Create data subdirectories if necessary. mkpath will die on error.
    #
    foreach my $subdir (qw(debug preview)) {
	my @newdirs = mkpath($interp->data_dir."/$subdir",0,0775);
	$interp->push_files_written(@newdirs);
    }

    #
    # Allow global $r in components
    #
    $interp->parser->allow_globals(qw($r));
}

#
# Generate an array that describes Interp's current status
#
sub interp_status
{
    my ($interp) = $_[0]->interp;

    my @strings;
    push @strings,
        qq(<DL><DT><FONT SIZE="+1"><B>Interp object properties</B></FONT>\n),
        qq(<DT><B>Startup options</B>\n);


    push @strings,
        map {"<DD><TT>$_ = ".(defined($interp->{$_}) ? 
                                $interp->{$_} : '<I>undef</I>'
                             )."</TT>\n" 
            } grep ref $interp->{$_} eq '', sort keys %{$interp->{_permitted}};

    push @strings, '</DL>',
            '<DL><DT><FONT SIZE="+1"><B>Cached components</B></FONT><DD>';

    if(%{$interp->{code_cache}})
    {     
        push (@strings, map("<TT>$_</TT><BR>\n", 
                            sort keys %{$interp->{code_cache}} 
                           )
             );
    } else {
        push @strings, '<I>None</I>';
    }     
    push @strings, '</DL>';
    return @strings;
}         

#
# Standard entry point for handling request
#
sub handle_request {

    #
    # Why do we use $req instead of $r here? A scoping bug in certain
    # versions of Perl 5.005 was getting confused about $r being used
    # in components, and the easiest workaround was to rename "$r" to
    # something else in this routine.  Go figure...
    # -jswartz 5/23
    #
    my ($self,$req) = @_;
    my ($outsub, $retval, $argString, $debugMsg, $q);
    my $outbuf = '';
    my $interp = $self->interp;

    #
    # construct (and truncate if necessary) the request to log at start
    #
    if ($interp->system_log_event_check('REQ_START')) {
	my $rstring = $req->server->server_hostname . $req->uri;
	$rstring .= "?".scalar($req->args) if defined(scalar($req->args));
	$rstring = substr($rstring,0,150).'...' if length($rstring) > 150;
	$interp->write_system_log('REQ_START', ++$self->{request_number},
				  $rstring);
    }

    #
    # If output mode is 'batch', collect output in a buffer and
    # print at the end. If output mode is 'stream', send output
    # to client as it is produced.
    #
    if ($self->output_mode eq 'batch') {
        $outsub = sub { $outbuf .= $_[0] if defined($_[0]) };
	$interp->out_method($outsub);
    } elsif ($self->output_mode eq 'stream') {
	$outsub = sub { print($_[0]) };
	$interp->out_method($outsub);
    }

    #
    # Create query object and get argument string.
    # Special case for debug files w/POST -- standard input not available
    # for CGI to read in this case.
    #
    if ($HTML::Mason::IN_DEBUG_FILE && $req->method eq 'POST') {
	$q = new CGI ($req->content);
    } else {
	$q = new CGI;
    }
    $argString = $q->query_string;

    my $debugMode = $self->debug_mode;
    $debugMode = 'none' if (ref($req) eq 'HTML::Mason::FakeApache');

    my $debugState = $self->capture_debug_state($req,$argString)
	if ($debugMode eq 'all' or $debugMode eq 'error');
    $debugMsg = $self->write_debug_file($req,$debugState) if ($debugMode eq 'all');

    eval('$retval = handle_request_1($self, $req, $argString, $q)');
    my $err = $@;
    my $err_status = $err ? 1 : 0;

    if ($err) {
	#
	# Take out date stamp and (eval nnn) prefix
	# Add server name, uri, referer, and agent
	#
	$err =~ s@^\[[^\]]*\] \(eval [0-9]+\): @@mg;
	$err = html_escape($err);
	my $referer = $req->header_in('Referer') || '<none>';
	my $agent = $req->header_in('User-Agent') || '';
	$err = sprintf("while serving %s %s (referer=%s, agent=%s)\n%s",$req->server->server_hostname,$req->uri,$referer,$agent,$err);

	if ($self->error_mode eq 'fatal') {
	    die ("System error:\n$err\n");
	} elsif ($self->error_mode eq 'html') {
	    if (!http_header_sent($req)) {
		$req->content_type('text/html');
		$req->send_http_header();
	    }
	    print("<h3>System error</h3><p><pre><font size=-1>$err</font></pre>\n");
	    $debugMsg = $self->write_debug_file($req,$debugState) if ($debugMode eq 'error');
	    print("<pre><font size=-1>\n$debugMsg\n</font></pre>\n") if defined($debugMsg);
	}
    } else {
	print("\n<!--\n$debugMsg\n-->\n") if defined($debugMsg) && http_header_sent($req) && !$req->header_only && $req->header_out("Content-type") =~ /text\/html/;
	print($outbuf) if $self->output_mode eq 'batch';
    }

    $interp->write_system_log('REQ_END', $self->{request_number}, $err_status);
    return ($err) ? &OK : (defined($retval)) ? $retval : &OK;
}

#
# Shorthand for various data subdirectories and files.
#
sub debug_dir { return shift->interp->data_dir . "/debug" }
sub preview_dir { return shift->interp->data_dir . "/preview" }

sub write_debug_file
{
    my ($self, $r, $dref) = @_;
    my $user = $r->connection->user || 'anon';
    my $outFile = sprintf("%d",int(rand(20))+1);
    my $outDir = $self->debug_dir . "/$user";
    if (!-d $outDir) {
	mkpath($outDir,0,0755) or die "cannot create debug directory '$outDir'";
    }
    my $outPath = "$outDir/$outFile";
    my $outfh = new IO::File ">$outPath";
    if (!$outfh) {
	$r->warn("cannot open debug file '$outPath' for writing");
	return;
    }

    my $d = new Data::Dumper ([$dref],['dref']);
    my $o = '';
    $o .= "#!".$self->debug_perl_binary."\n";
    $o .= <<'PERL';
# -----------------------------
# Read command-line options for repeat counts (-rX) and profiling via
# Devel::DProf (-p). As component runs in profile mode, component
# coderefs are accumulated in %CODEREF_NAME
# -----------------------------
BEGIN {
    use IO::File;
    use File::Copy;
    use Getopt::Std;
    getopt('r');   # r=repeat count, p=user profile req, P=re-entrant profile call
    $opt_r ||= 1;
    
    if ($opt_p) {
	print STDERR "Profiling request ...";
	# re-enter with different option (no inf. loops, please)
	system ("perl", "-d:DProf", $0, "-P", "-r$opt_r");
    
# -----------------------------
# When done, merge named coderefs in tmon.mason with those in tmon.out,
# then run dprofpp
# -----------------------------
	my $fh = new IO::File '< ./tmon.mason' or die "Missing file: tmon.mason";
	foreach (<$fh>) { chomp;  my ($k,$v) = split(/\t/);  $::CODEREF_NAME{$k} = $v; }
	$fh->close;
    
	my $tmonout = new IO::File '< ./tmon.out' or die "Missing file: tmon.out";
	my $tmontmp = new IO::File '> ./tmon.tmp' or die "Couldn't write file: tmon.tmp";
	my $regex = quotemeta(join('|', keys %::CODEREF_NAME));
	$regex =~ s/\\\|/|/g;   #un-quote the pipe chars
	while (<$tmonout>) {
	    s/HTML::Mason::Commands::($regex)/$::CODEREF_NAME{$1}/;
	    print $tmontmp $_
	}
	$tmonout->close; $tmontmp->close;
	copy('tmon.tmp' => 'tmon.out') or die "$!";
	unlink('tmon.tmp');
        print STDERR "\nRunning dprofpp ...\n";
	exec('dprofpp') or die "Couldn't execute dprofpp";
    }
}

PERL
    $o .= "BEGIN { \$HTML::Mason::IN_DEBUG_FILE = 1; require '".$self->debug_handler_script."' }\n\n";
    $o .= <<'PERL';
if ($opt_P) {
    open SAVEOUT, ">&STDOUT";    # stifle component output while profiling
    open STDOUT, ">/dev/null";
}
for (1 .. $opt_r) {
print STDERR '.' if ($opt_P and $opt_r > 1);
PERL
    $o .= "my ";
    $o .= $d->Dumpxs;
    $o .= 'my $r = HTML::Mason::ApacheHandler::simulate_debug_request($dref);'."\n";
    $o .= 'local %ENV = (%ENV,%{$dref->{ENV}});'."\n";
    $o .= 'my $status = '.$self->debug_handler_proc."(\$r);\n";
    $o .= 'print "return status: $status\n";'."\n}\n\n";
    $o .= <<'PERL';
if ($opt_P) {
    my $fh = new IO::File '>./tmon.mason' or die "Couldn't write tmon.mason";
    print $fh map("$_\t$HTML::Mason::CODEREF_NAME{$_}\n", keys %HTML::Mason::CODEREF_NAME);
    $fh->close;
}
PERL
    $outfh->print($o);
    $outfh->close();
    chmod(0775,$outPath);

    my $debugMsg = "Debug file is '$outFile'.\nFull debug path is '$outPath'.\n";
    return $debugMsg;
}

sub capture_debug_state
{
    my ($self, $r, $argString) = @_;
    my (%d,$expr);

    $expr = '';
    foreach my $field (qw(allow_options auth_name auth_type bytes_sent no_cache content_encoding content_languages content_type document_root filename header_only method method_number path_info protocol proxyreq requires status status_line the_request uri as_string get_remote_host get_remote_logname get_server_port is_initial_req is_main)) {
	$expr .= "\$d{$field} = \$r->$field;\n";
    }
    eval($expr);
    warn "error creating debug file: $@\n" if $@;

    if (pkg_installed('Apache::Table')) {
	$expr = "my \$href;\n";
	foreach my $field (qw(headers_in headers_out err_headers_out notes dir_config subprocess_env)) {
	    $expr .= "\$href = scalar(\$r->$field); \$d{$field} = {\%\$href};\n";
	}
	eval($expr);
	warn "error creating debug file: $@\n" if $@;
    } else {
	foreach my $field (qw(headers_in headers_out err_headers_out notes dir_config subprocess_env)) {
	    $d{$field} = {};
	}
    }
    
    $d{'args@'} = [$r->args];
    
    $d{'args$'} = scalar($r->args);
    
    if ($r->method eq 'POST') {
	$d{content} = $argString;
    }

    $expr = '';
    $d{server} = {};
    foreach my $field (qw(server_admin server_hostname port is_virtual names)) {
	$expr .= "\$d{server}->{$field} = \$r->server->$field;\n";
    }
    eval($expr);
    
    $expr = '';
    $d{connection} = {};
    foreach my $field (qw(remote_host remote_ip local_addr remote_addr remote_logname user auth_type aborted)) {
	$expr .= "\$d{connection}->{$field} = \$r->connection->$field;\n";
    }
    eval($expr);

    $d{ENV} = {%ENV};

    return {%d};
}

sub handle_request_1
{
    my ($self,$r,$argString,$q) = @_;
    my $interp = $self->interp;
    my $compRoot = $interp->comp_root;

    #
    # If filename is a directory, then either decline or simply reset
    # the content type, depending on the value of decline_dirs.
    #
    if (-d $r->filename) {
	if ($self->decline_dirs) {
	    return DECLINED;
	} else {
	    $r->content_type(undef);
	}
    }
    
    #
    # Compute the component path by deleting the component root
    # directory from the front of Apache's filename.  If the
    # substitute fails, we must have an URL outside Mason's component
    # space; return not found.
    #
    my $compPath = $r->filename;
    if (!($compPath =~ s/^$compRoot//)) {
	$r->warn("Mason: filename (\"".$r->filename."\") is outside component root (\"$compRoot\"); returning 404.");
	return NOT_FOUND;
    }
    $compPath =~ s@/$@@ if $compPath ne '/';
    while ($compPath =~ s@//@/@) {}

    #
    # Try to load the component; if not found, try dhandlers
    # ("default handlers"); otherwise return not found.
    #
    my ($comp,$dhandlerArg);
    if (!($comp = $interp->load($compPath))) {
	if ($interp->dhandler_name && $comp = $interp->find_comp_upwards($compPath,$interp->dhandler_name)) {
	    my $remainder = ($comp->parent_path eq '/') ? $compPath : substr($compPath,length($comp->parent_path));
	    my $pathInfo = $remainder.$r->path_info;
	    $r->path_info($pathInfo);
	    $dhandlerArg = substr($pathInfo,1);
	} else {
	    $r->warn("Mason: no component corresponding to filename \"".$r->filename."\", comp path \"$compPath\"; returning 404.");
	    return NOT_FOUND;
	}
    }
    my $srcfile = $comp->source_file;

    #
    # Decline if file does not pass top level predicate.
    #
    if (!$self->top_level_predicate->($srcfile)) {
	$r->warn("Mason: component file \"$srcfile\" does not pass top-level predicate; returning 404.");
	return NOT_FOUND;
    }
    
    #
    # Parse arguments into key/value pairs. Represent multiple valued
    # keys with array references.
    #
    my (%args);

    foreach my $key ( $q->param ) {
      foreach my $value ( $q->param($key) ) {
        if (exists($args{$key})) {
          if (ref($args{$key})) {
            $args{$key} = [@{$args{$key}}, $value];
          } else {
            $args{$key} = [$args{$key}, $value];
          }
        } else {
          $args{$key} = $value;
        }
      }
    }

    $argString = '' if !defined($argString);

    #
    # Send HTTP headers when the primary section is reached.
    #
    my $hdrsub = sub {
	my ($interp) = @_;
	$r->send_http_header() if !http_header_sent($r);
	$interp->abort() if $r->header_only;
	$interp->suppress_hook(name=>'http_header',type=>'start_primary');
    };
    $interp->add_hook(name=>'http_header',type=>'start_primary',code=>$hdrsub);

    #
    # Create an Apache-specific request with additional slots.
    #
    my $request = new HTML::Mason::Request::ApacheHandler
	(ah=>$self,
	 interp=>$interp,
	 http_input=>$argString,
	 apache_req=>$r
	 );

    $request->dhandler_arg($dhandlerArg) if (defined($dhandlerArg));

    #
    # Set up interpreter global variables.
    #
    $interp->set_global(r=>$r);
    
    return $interp->exec($comp, REQ=>$request, %args);
}

sub simulate_debug_request
{
    my ($infoRef) = @_;
    my %info = %$infoRef;
    my $r = new HTML::Mason::FakeApache;

    while (my ($key,$value) = each(%{$info{server}})) {
	$r->{server}->{$key} = $value;
    }
    while (my ($key,$value) = each(%{$info{connection}})) {
	$r->{connection}->{$key} = $value;
    }
    delete($info{server});
    delete($info{connection});
    while (my ($key,$value) = each(%info)) {
	$r->{$key} = $value;
    }
    
    return $r;
}

#
# Determines whether the http header has been sent.
#
sub http_header_sent
{
    my ($r) = @_;
    my $sent = $r->header_out("Content-type");
    return $sent;
}

#
# Apache-specific Mason commands
#
package HTML::Mason::Commands;
use vars qw($REQ);
sub mc_suppress_http_header
{
    my $interp = $REQ->interp;
    if ($_[0]) {
	$interp->suppress_hook(name=>'http_header',type=>'start_primary');
    } else {
	$interp->unsuppress_hook(name=>'http_header',type=>'start_primary');
    }
}

1;
