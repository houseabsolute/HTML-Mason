# Copyright (c) 1998-99 by Jonathan Swartz. All rights reserved.
# This program is free software; you can redistribute it and/or modify it
# under the same terms as Perl itself.

require 5.004;

#----------------------------------------------------------------------
#
# APACHE-SPECIFIC REQUEST OBJECT
#
package HTML::Mason::Request::ApacheHandler;
require Exporter;
use vars qw(@ISA);
@ISA = qw(HTML::Mason::Request);

# Fields that can be set in new method, with defaults
my %reqfields =
    (ah => undef,
     http_input => undef,
     apache_req => undef,
     );

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

# Create generic read-write accessor routines

sub ah { my $s=shift; return @_ ? ($s->{ah}=shift) : $s->{ah} }
sub http_input { my $s=shift; return @_ ? ($s->{http_input}=shift) : $s->{http_input} }
sub apache_req { my $s=shift; return @_ ? ($s->{apache_req}=shift) : $s->{apache_req} }

# Override flush_buffer to also call $r->rflush
sub flush_buffer
{
    my ($self, $content) = @_;
    $self->SUPER::flush_buffer($content);
    $self->apache_req->rflush;
}


#----------------------------------------------------------------------
#
# APACHEHANDLER OBJECT
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
use HTML::Mason::Tools qw(dumper_method html_escape url_unescape pkg_installed);
use HTML::Mason::Utils;
use Apache::Status;
use CGI qw(-private_tempfiles);

my @used = ($HTML::Mason::IN_DEBUG_FILE);
	    
# Fields that can be set in new method, with defaults
my %fields =
    (
     apache_status_title => 'mason',
     auto_send_headers => 1, 
     decline_dirs => 1,
     error_mode => 'html',
     interp => undef,
     output_mode => undef,    # deprecated - now interp->out_mode
     top_level_predicate => undef,
     debug_mode => 'none',
     debug_perl_binary => '/usr/bin/perl',
     debug_handler_script => undef,
     debug_handler_proc => undef,
     debug_dir_config_keys => [],
     );

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

    # Add an HTML::Mason menu item to the /perl-status page. Things we report:
    # -- Interp properties
    # -- loaded (cached) components
    my $name = $self->{apache_status_title};
    my $title;
    if ($name eq 'mason') {
        $title='HTML::Mason status';    #item for HTML::Mason module
    } else {
        $title=$name;
        $name=~s/\W/_/g;
    }

    my $statsub = sub {
	my($r,$q) = @_; #request and CGI objects
	return [] if !defined($r);
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
	my $string;
	foreach my $key (sort keys %{$interp->{code_cache}}) {
	    $string .= sprintf("<TT>%s (%s)</TT><BR>\n",$key,scalar(localtime($interp->{code_cache}->{$key}->{lastmod})));
	}
	push (@strings, $string);
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
    # Why do we use $apreq instead of $r here? A scoping bug in certain
    # versions of Perl 5.005 was getting confused about $r being used
    # in components, and the easiest workaround was to rename "$r" to
    # something else in this routine.  Go figure...
    # -jswartz 5/23
    #
    my ($self,$apreq) = @_;
    my ($outsub, $retval);
    my $outbuf = '';
    my $interp = $self->interp;
    $self->{request_number}++;

    #
    # Construct (and truncate if necessary) the request to log at start
    #
    if ($interp->system_log_event_check('REQ_START')) {
	my $rstring = $apreq->server->server_hostname . $apreq->uri;
	$rstring .= "?".scalar($apreq->args) if defined(scalar($apreq->args));
	$rstring = substr($rstring,0,150).'...' if length($rstring) > 150;
	$interp->write_system_log('REQ_START', $self->{request_number},
				  $rstring);
    }

    #
    # Determine debug file mode. Turn it off regardless if we are
    # already operating from a debug file.
    #
    my $debugMode = $self->debug_mode;
    $debugMode = 'none' if $HTML::Mason::IN_DEBUG_FILE;

    #
    # Capture debug state as early as possible, before we start messing with $apreq.
    #
    my $debugState;
    $debugState = $self->capture_debug_state($apreq)
	if ($debugMode eq 'all' or $debugMode eq 'error');

    #
    # Write debug file as early as possible if debug mode is
    # 'all'. However, we must unfortunatly wait in the case of POSTs
    # because we don't yet have the stdin content.
    #
    my $debugMsg;
    $debugMsg = $self->write_debug_file($apreq,$debugState)
	if ($debugMode eq 'all' && $apreq->method ne 'POST');

    eval { $retval = handle_request_1($self, $apreq, $debugState) };
    my $err = $@;
    my $err_status = $err ? 1 : 0;

    if ($err) {
	#
	# Take out date stamp and (eval nnn) prefix
	# Add server name, uri, referer, and agent
	#
	$err =~ s@^\[[^\]]*\] \(eval [0-9]+\): @@mg;
	$err = html_escape($err);
	my $referer = $apreq->header_in('Referer') || '<none>';
	my $agent = $apreq->header_in('User-Agent') || '';
	$err = sprintf("while serving %s %s (referer=%s, agent=%s)\n%s",$apreq->server->server_hostname,$apreq->uri,$referer,$agent,$err);

	if ($self->error_mode eq 'fatal') {
	    die ("System error:\n$err\n");
	} elsif ($self->error_mode eq 'html') {
	    if (!http_header_sent($apreq)) {
		$apreq->content_type('text/html');
		$apreq->send_http_header();
	    }
	    print("<h3>System error</h3><p><pre><font size=-1>$err</font></pre>\n");
	    $debugMsg = $self->write_debug_file($apreq,$debugState) if ($debugMode eq 'error' || ($debugMode eq 'all' && $apreq->method eq 'POST'));
	    print("<pre><font size=-1>\n$debugMsg\n</font></pre>\n") if defined($debugMsg);
	}
    } else {
	$debugMsg = $self->write_debug_file($apreq,$debugState) if ($debugMode eq 'all' && $apreq->method eq 'POST');
	print "\n<!--\n$debugMsg\n-->\n" if (defined($debugMsg) && http_header_sent($apreq) && !$apreq->header_only && $apreq->header_out("Content-type") =~ /text\/html/);
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
    $o .= dumper_method($d);
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
    my ($self, $r) = @_;
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
    my ($self,$r,$debugState) = @_;
    my $interp = $self->interp;

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
    # Compute the component path via the resolver.
    #
    my $compPath = $interp->resolver->file_to_path($r->filename,$interp);
    return NOT_FOUND unless $compPath;

    $compPath =~ s@/$@@ if $compPath ne '/';
    while ($compPath =~ s@//@/@) {}

    #
    # Try to load the component; if not found, try dhandlers
    # ("default handlers"); otherwise return not found.
    #
    my ($comp,$dhandlerArg);
    if (!($comp = $interp->load($compPath))) {
	if ($interp->dhandler_name and $comp = $interp->find_comp_upwards($compPath,$interp->dhandler_name)) {
	    my $remainder = ($comp->dir_path eq '/') ? $compPath : substr($compPath,length($comp->dir_path));
	    my $pathInfo = $remainder.$r->path_info;
	    $r->path_info($pathInfo);
	    $dhandlerArg = substr($pathInfo,1);
	} else {
	    return NOT_FOUND;
	}
    }

    #
    # Decline if file does not pass top level predicate.
    #
    if (defined($self->{top_level_predicate})) {
	my $srcfile = $comp->source_file;
	if (!$self->{top_level_predicate}->($srcfile)) {
	    return NOT_FOUND;
	}
    }
    
    #
    # Create query object and get argument string. Skip if GET with no
    # query string.  Special case for debug files w/POST -- standard
    # input not available for CGI to read in this case.
    #
    my $q;
    unless ($r->method eq 'GET' && !scalar($r->args)) {
	if ($HTML::Mason::IN_DEBUG_FILE && $r->method eq 'POST') {
	    $q = new CGI ($r->content);
	} else {
	    $q = new CGI;
	}
	# For POSTs, we finally store the content in the debug state
	# if debug mode is on.
	$debugState->{content} = $q->query_string if $debugState && $r->method eq 'POST';
    }

    #
    # Parse arguments into key/value pairs. Represent multiple valued
    # keys with array references.
    #
    my (%args);
    if ($q) {
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
    }

    #
    # Create an Apache-specific request with additional slots.
    #
    my $request = new HTML::Mason::Request::ApacheHandler
	(ah=>$self,
	 interp=>$interp,
	 http_input=>($q ? $q->query_string : ''),
	 apache_req=>$r
	 );

    $request->{dhandler_arg} = $dhandlerArg if (defined($dhandlerArg));

    #
    # Deprecated output_mode parameter - just pass to request out_mode.
    #
    if (my $mode = $self->output_mode) {
	$request->{out_mode} = $mode;
    }

    #
    # Set up interpreter global variables.
    #
    $interp->set_global(r=>$r);

    #
    # Craft the out method for this request to handle automatic http
    # headers.
    #
    if ($self->auto_send_headers) {
	my $headers_sent = 0;
	my $delay_buf = '';
	my $out_method = sub {
	    # Check to see if the header has been sent, first via a fast
	    # flag, then via a slightly slower $r test.
	    unless ($headers_sent) {
		unless (http_header_sent($r)) {
		    # If in stream mode and header has not been sent,
		    # buffer initial whitespace so as to delay headers.
		    if ($_[0] !~ /\S/ and $request->out_mode eq 'stream') {
			$delay_buf .= $_[0];
			return;
		    } else {
			$r->send_http_header();
			$request->abort() if $r->header_only;
		    }
		}
		unless ($delay_buf eq '') {
		    $interp->out_method->($delay_buf);
		    $delay_buf = '';
		}
		$headers_sent = 1;
	    }
	    $interp->out_method->($_[0]);

	    # A hack, but good for efficiency in stream mode: change the
	    # current sink of the request so all this is bypassed for the
	    # remainder of this component and its children.
	    $request->top_stack->{sink} = $interp->out_method if $request->out_mode eq 'stream' and $request->top_stack->{sink} eq $request->out_method;
	};
	$request->{out_method} = $out_method;

	my $retval = $request->exec($comp, %args);

	# Send headers and any buffered whitespace if it has not
	# already been sent (as in a page with just headers).
	if ($request->out_mode eq 'stream' and !$headers_sent) {
	    $r->send_http_header() unless http_header_sent($r);
	    $interp->out_method($delay_buf) unless $delay_buf eq '';
	}

	return $retval;
    } else {
	$request->exec($comp, %args);
    }
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
sub http_header_sent { shift->header_out("Content-type") }

# Create generic read-write accessor routines

sub apache_status_title { my $s=shift; return @_ ? ($s->{apache_status_title}=shift) : $s->{apache_status_title} }
sub auto_send_headers { my $s=shift; return @_ ? ($s->{auto_send_headers}=shift) : $s->{auto_send_headers} }
sub decline_dirs { my $s=shift; return @_ ? ($s->{decline_dirs}=shift) : $s->{decline_dirs} }
sub error_mode { my $s=shift; return @_ ? ($s->{error_mode}=shift) : $s->{error_mode} }
sub interp { my $s=shift; return @_ ? ($s->{interp}=shift) : $s->{interp} }
sub output_mode { my $s=shift; return @_ ? ($s->{output_mode}=shift) : $s->{output_mode} }
sub top_level_predicate { my $s=shift; return @_ ? ($s->{top_level_predicate}=shift) : $s->{top_level_predicate} }
sub debug_mode { my $s=shift; return @_ ? ($s->{debug_mode}=shift) : $s->{debug_mode} }
sub debug_perl_binary { my $s=shift; return @_ ? ($s->{debug_perl_binary}=shift) : $s->{debug_perl_binary} }
sub debug_handler_script { my $s=shift; return @_ ? ($s->{debug_handler_script}=shift) : $s->{debug_handler_script} }
sub debug_handler_proc { my $s=shift; return @_ ? ($s->{debug_handler_proc}=shift) : $s->{debug_handler_proc} }
sub debug_dir_config_keys { my $s=shift; return @_ ? ($s->{debug_dir_config_keys}=shift) : $s->{debug_dir_config_keys} }

#----------------------------------------------------------------------
#
# APACHEHANDLER MASON COMMANDS
#
package HTML::Mason::Commands;
use vars qw($m);
# no longer needed
sub mc_suppress_http_header {}

1;
