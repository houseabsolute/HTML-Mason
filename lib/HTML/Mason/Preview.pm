# Copyright (c) 1998-99 by Jonathan Swartz. All rights reserved.
# This program is free software; you can redistribute it and/or modify it
# under the same terms as Perl itself.

use strict;

#
# Subclass Apache::Server mainly to allow server_hostname to be set.
#
package HTML::Mason::Preview::Apache::Server;
use vars qw($AUTOLOAD);
sub new {
    my ($class, $pr) = @_;
    my $self = {
	true_server => $pr->{r}->server,
	server_hostname => $pr->{r}->server->server_hostname
	};
    bless $self, $class;
    return $self;
}
sub server_hostname {
    my ($self) = shift;
    if (@_) {
	$self->{server_hostname} = shift;
    }
    return $self->{server_hostname};
}
sub AUTOLOAD {
    my $self = shift;
    my $name = $AUTOLOAD;
    $name =~ s/.*://;   # strip fully-qualified portion
    return if $name eq 'DESTROY';
    my $s = $self->{true_server};
    eval(sprintf('$s->%s(@_)',$name));
}

#
# Subclass Apache object to override various functions with their
# preview values.
#
package HTML::Mason::Preview::Apache;
use vars qw($AUTOLOAD);
sub new {
    my ($class, $r, $conf) = @_;
    my $self = {
	r => $r,
	server => {},
	env => {},
	dir_config => {}
    };
    bless $self, $class;

    $self->{server} = new HTML::Mason::Preview::Apache::Server ($self);
    return $self;
}

sub server { return $_[0]->{server} }

sub document_root {
    my ($self) = shift;
    if (@_) {
	$self->{document_root} = shift;
    }
    return defined($self->{document_root}) ? $self->{document_root} : $self->{r}->document_root;
}

sub cgi_env {
    my ($self) = @_;
    my ($key,$value);
    my %env = $self->{r}->cgi_env();
    while (($key,$value) = each(%{$self->{env}})) {
	$env{$key} = $value;
    }
    return %env;
}

sub dir_config {
    my ($self,$key) = @_;
    return $self->{dir_config}->{$key};
}

sub AUTOLOAD {
    my $self = shift;
    my $name = $AUTOLOAD;
    $name =~ s/.*://;   # strip fully-qualified portion
    return if $name eq 'DESTROY';
    my $realr = $self->{r};
    eval(sprintf('$realr->%s(@_)',$name));
}

#
# Subclass HTML::Mason::Preview::Apache for debug mode, mainly to suppress
# http header.
#
package HTML::Mason::Preview::Apache::Debug;
use vars (qw(@ISA));
sub new {
    my $class = shift;
    my $self = new HTML::Mason::Preview::Apache(@_);
    bless $self, $class;
    return $self;
}
@ISA = qw(HTML::Mason::Preview::Apache);

# Don't actually send http header
sub send_http_header {
}

package HTML::Mason::Preview;
use HTML::Mason::ApacheHandler;
use HTML::Mason::Config;
use HTML::Mason::Tools qw(date_delta_to_secs html_escape);
use MLDBM ($HTML::Mason::Config{mldbm_use_db}, $HTML::Mason::Config{mldbm_serializer});
use IO::File qw(!/^SEEK/);
use POSIX;

sub open_preview_settings
{
    my ($previewDir, $user, $write) = @_;
    my (%h);
    my $previewFile = "$previewDir/$user";
    if ($write || !-e $previewFile) {
	tie (%h, 'MLDBM', $previewFile, O_RDWR|O_CREAT, 0664)
	    or die "cannot create/open preview file '$previewFile' for writing\n";
	untie(%h) if !$write;
    }
    if (!$write) {
	tie (%h, 'MLDBM', $previewFile, O_RDONLY, 0)
	    or die "cannot open preview file '$previewFile' for reading\n";
    }
    return \%h;
}

sub handle_preview_request
{
    my $result = eval { handle_preview_request_1(@_) };
    my $err = $@;
    if ($err) {
	my ($r) = @_;
	$r->content_type('text/html');
	$r->header_out(Pragma=>'no-cache');
	$r->send_http_header();
	print("<h2>Previewer Error</h2>\n<pre>\n$err\n</pre>\n");
	return;
    }
    return $result;
}

sub handle_preview_request_1
{
    my ($r, $ah, %options) = @_;
    my (%simhdr);
    my $cmHome = $options{cm_home};
    
    #
    # Determine user name and port.
    #
    my $userName = $r->connection->user || 'anonymous';
    my $host = $r->header_in('Host');
    my ($port) = ($host =~ /:([0-9]+)/);
    die "cannot detect port" if !defined($port);

    #
    # Find configuration for user & port.
    #
    my $in = open_preview_settings($ah->preview_dir,$userName,0);
    my $href = $in->{$port};
    die "Cannot find preview configuration for user '$userName', port $port\n. Go to the main previewer page to set the configuration for this port.\n" if (!$href);
    my $conf = $href->{request};

    my $interp = $ah->interp;

    #
    # Create subclassed Apache object.
    #
    my $pr;
    if ($conf->{output_type} eq 'HTML') {
	$pr = new HTML::Mason::Preview::Apache ($r, $conf);
    } elsif ($conf->{output_type} eq 'Debug') {
	$pr = new HTML::Mason::Preview::Apache::Debug ($r, $conf);
    } else {
	die "unknown output type '$conf->{output_type}'";
    }
    die "cannot create preview apache object" if !$pr;

    #
    # Interpreter time
    #
    if (exists($conf->{'time'})) {
	my $timetype = $conf->{'time'}->{type};
	my $time;
	if ($timetype ne 'real') {
	    if ($timetype eq 'relative') {
		$time = time() + date_delta_to_secs($conf->{'time'}->{delta});
	    } elsif ($timetype eq 'absolute') {
		$time = $conf->{'time'}->{value};
	    }
	    $interp->current_time($time);
	}
    }
    
    if (defined($conf->{interp})) {
	while (my ($key,$value) = each(%{$conf->{interp}})) {
	    eval("\$interp->$key(\$value)");
	}
        # cmp specific, must remove
	if ($conf->{alternate_source}) {
	    my $altpath = "/VERSIONS/".$conf->{alternate_source};
	    my $altsub = sub {
		if ($_[0] eq 'comp') {
		    return ($altpath.$_[1]);
		} else {
		    return ();
		}
	    };
	    $interp->alternate_sources($altsub);
	}
	$interp->_initialize;	
    }
    
    if (defined($conf->{headers_in})) {
	while (my ($hdr,$value) = each(%{$conf->{headers_in}})) {
	    $pr->header_in($hdr,$value);
	    my $envname = uc($hdr);
	    $envname =~ s/\-/_/;
	    $pr->{env}->{$envname} = $value;
	    $simhdr{$hdr} = 1;
	}
    }
    if (defined($conf->{r})) {
	while (my ($key,$value) = each(%{$conf->{r}})) {
	    eval("\$pr->$key(\$value)");
	}
    }
    if (defined($conf->{server})) {
	while (my ($key,$value) = each(%{$conf->{server}})) {
	    eval("\$pr->server->$key(\$value)");
	}
    }
    if (defined($conf->{connection})) {
	while (my ($key,$value) = each(%{$conf->{connection}})) {
	    eval("\$pr->connection->$key(\$value)");
	}
    }
    if (defined($conf->{env})) {
	while (my ($key,$value) = each(%{$conf->{env}})) {
	    $pr->{env}->{$key} = $value;
	}
    }
    if (defined($conf->{dir_config})) {
	while (my ($key,$value) = each(%{$conf->{dir_config}})) {
	    $pr->{dir_config}->{$key} = $value;
	}
    }
    if (defined($conf->{apache_handler})) {
	while (my ($key,$value) = each(%{$conf->{apache_handler}})) {
	    eval("\$ah->$key(\$value)");
	}
	$ah->_initialize;
    }

    if ($conf->{output_type} eq 'HTML') {
        return $ah->handle_request($pr);
    } elsif ($conf->{output_type} eq 'Debug') {

	#
	# Start document
	#
	$pr->content_type('text/html');
	HTML::Mason::Preview::Apache::send_http_header($pr);
	$pr->print("<body bgcolor=#ffffff>");
	$pr->print("<script language=javascript>\n<!--\ndocument.write(\"<title>\" + \"Preview \" + window.name.substring(11,12) + \"</title>\")\n// -->\n</script>\n");

	#
	# Set up hooks to record events. An event is the starting
	# or ending of an $m->comp or $m->file. When an event occurs,
	# place an event code in the content (EVENT# surrounded by
	# ctrl-A), and push a hash of information onto the event list.
	#
	my ($content,$i,@compEvents,$event,$key,$value,$trace);
	my $eventnum = 0;
	my $startCompHook = sub {
	    my ($req) = @_;
	    $content .= "\cAEVENT$eventnum\cA";
	    my $path = $req->current_comp->title;
	    $compEvents[$eventnum++] = {type=>'startComp',path=>$path,comp=>($req->current_comp)};
	};
	my $endCompHook = sub {
	    my ($req) = @_;
	    $content .= "\cAEVENT$eventnum\cA";
	    my $path = $req->current_comp->title;
	    $compEvents[$eventnum++] = {type=>'endComp',path=>$path};
	};
	my $startFileHook = sub {
	    my ($req,$file) = @_;
	    $content .= "\cAEVENT$eventnum\cA";
	    $compEvents[$eventnum++] = {type=>'startFile',path=>$file,start=>length($content)};
	};
	my $endFileHook = sub {
	    my ($req,$file) = @_;
	    my $end = length($content);
	    $content .= "\cAEVENT$eventnum\cA";
	    $compEvents[$eventnum++] = {type=>'endFile',path=>$file,end=>$end};
	};

	#
	# Create a trace of objects (components/files), with one
	# object per line.
	#
	my $createTraceSub = sub {
	    my (@objects) = @_;
	    my ($trace,$obj);
	    foreach $obj (@objects) {
		next if !defined($obj);
		$trace .= sprintf('%s<a href="%s">%s</a>  %s%s',
				  (' ' x (4-length($obj->{label}))),
				  $obj->{srclink},$obj->{label},$obj->{display},$obj->{repeat}>1 ? " (repeats ".$obj->{repeat}." times)": "");
		$trace .= "\n";
	    }
	    return $trace;
	};

	#
	# Make HTTP request, trap output in $content
	#
	$interp->add_hook(name=>'preview',type=>'start_comp',code=>$startCompHook);
	$interp->add_hook(name=>'preview',type=>'end_comp',code=>$endCompHook);
	$interp->add_hook(name=>'preview',type=>'start_file',code=>$startFileHook);
	$interp->add_hook(name=>'preview',type=>'end_file',code=>$endFileHook);
	$interp->out_method(sub { $content .= $_[0] });
	$ah->output_mode(undef);
        my $statuscode = $ah->handle_request($pr);

	#
	# Validate file events. For each startFile event,
	# check that the following content matches the file.
	#
	for (my $e=0; $e < @compEvents; $e++) {
	    my $event = $compEvents[$e];
	    my ($type,$path) = ($event->{type},$event->{path});
	    if ($type eq 'startFile') {
		next if ($e == $#compEvents);
		my $nextEvent = $compEvents[$e+1];
		if ($nextEvent->{type} ne 'endFile' || $nextEvent->{path} ne $path) {
		    undef($compEvents[$e]);
		    next;
		}
		my $valid = 0;
		my $nextEventStr = sprintf("\cAEVENT%d\cA",$e+1);
		if (-f $path) {
		    my $start = $event->{start};
		    if (substr($content,$start,length($nextEventStr)) eq $nextEventStr) {
			$start += length($nextEventStr);
			my $length = [stat($path)]->[7];
			my $fh = new IO::File $path;
			if ($length < 1024) {
			    local $/ = undef;
			    my $filetext = <$fh>;
			    $valid = ($filetext eq substr($content,$start,$length));
			} else {
			    my $buf;
			    read($fh,$buf,128);
			    if ($buf eq substr($content,$start,128)) {
				$fh->seek(-128,&SEEK_END);
				read($fh,$buf,128);
				$valid = ($buf eq substr($content,$start+$length-128,128));
			    }
			}
			if ($valid) {
			    $content = substr($content,0,$start-length($nextEventStr)) . substr($content,$start,$length) . $nextEventStr . substr($content,$start+$length);
			}
		    }
		}
		# leave "invalid" file events in?
		#if (!$valid) {
		if (0) {
		    undef($compEvents[$e]);
		    undef($compEvents[$e+1]);
		    $e++;
		}
	    }
	}

	#
	# Analyze event information. Create a list of objects
	# (distinct component and $m->file invocations)
	# and assign to each event the object that follows it.
	#
	my (@objects,@stack);
	my $objcount = 0;
	my $fileroot = $interp->static_file_root();
	foreach $event (@compEvents) {
	    next if !defined($event);
	    my ($type,$path) = ($event->{type},$event->{path});
	    if ($type =~ /^(startComp|startFile)$/) {
		my ($objtext,$objsrclink,$objlabel,$objtype,$objdisplay);
		$objcount++;
		if ($type eq 'startComp') {
		    $objtext = "$path";
		    $objlabel = "$objcount";
		    $objsrclink = "#object$objcount";
		    $objtype = "comp";
		} else {
		    my ($dir,$file) = ($path =~ m@^(.*)/([^/]*)$@);
		    my $p = $path;
		    $p =~ s@^$fileroot/@@;
		    $objtext = "file: $p";
		    $objlabel = "F$objcount";
		    $objsrclink = "#object$objcount";
		    $objtype = "file";
		}
		my $lastobj = $objects[$objcount-1];

		# Look for repeated entries (equal text and stack
		# depth) and combine into one line
		if ($objtext eq $lastobj->{text} && $objtype eq $lastobj->{type} && scalar(@stack) == $lastobj->{depth}) {
		    $objcount--;
		    $objects[$objcount]->{repeat}++;
		} else {
		    my $objdisplay = '';

		    # If Content Management home specified, and we
		    # have a file based component or a static file
		    # inside the file root, create content management
		    # view/edit links.
		    if ($cmHome &&
			(($objtype eq 'comp' and $event->{comp}->is_file_based) or
			 ($objtype eq 'file' and $path =~ m{^$fileroot}))) {
			my $branch;
			my $comproot = $interp->first_comp_root();
			if ($objtype eq 'comp') {
			    $path = $comproot.$path;
			    $branch = 'Components';
			} else {
			    $path =~ s{^$fileroot} {};
			    $objtext =~ s{^$fileroot} {};
			    $branch = 'Content';
			}
			my $spacer = ('  ' x (scalar(@stack)+1));
			$cmHome =~ s/\/$//;
			$objdisplay = (qq{<a href="$cmHome/textView?branch=$branch&path=$path">info</a> <a href="$cmHome/editComp?branch=$branch&path=$path">edit</a>}.$spacer.$objtext);
		    } else {
			$objdisplay = (('  ' x scalar(@stack)) . $objtext);
		    }
		    # Store object info. Some of this is used for repeat comparison.
		    $objects[$objcount] = {count=>$objcount,type=>$objtype,display=>$objdisplay,label=>$objlabel,text=>$objtext,color=>'003399',srclink=>$objsrclink,depth=>scalar(@stack),repeat=>1};
		}
		push(@stack,$objcount);
	    } elsif ($type =~ /^(endComp|endFile)$/) {
		pop(@stack);
	    }
	    $event->{objnum} = $stack[-1];
	}

	#
	# Initial component trace
	#
	if (@objects) {
	    $trace = &$createTraceSub(@objects);
	    $pr->print("<h2>Component trace</h2>\n<pre>\n$trace</pre>\n");
	}

	#
	# Content
	#
	if ($content) {
	    $pr->print("<h2>Content</h2>\n<pre>\n");
	    $content =~ s/\cM//g;
	    ($content) = html_escape($content);
	    $content =~ s/\cAEVENT/\n\cAEVENT/g;
	    my ($obj);
	    while ($content =~ /(.*)/g) {
		my $line = $1;
		my $beginFlag = 0;
		if ($line =~ /\cAEVENT([0-9]+)\cA/) {
		    $eventnum = $1;
		    my $event = $compEvents[$eventnum];
		    $line =~ s/\cAEVENT[0-9]+\cA//;
		    if (defined($event)) {
			if ($event->{type} =~ /^end/ && $obj) {
			    $pr->print(sprintf("<font color=#%s>%5s &lt;&lt;</font>\n",$obj->{color},$obj->{label}));
			}
			$obj = $objects[$event->{objnum}];
			if ($event->{type} =~ /^start/) {
			    $pr->print("<a name=object$obj->{count}>");
			    $beginFlag = 1;
			}
		    }
		}
		next if ($line !~ /\S/ && !$obj);
		my $depth = 0;
		$pr->print(sprintf("<font color=#%s>%s%5s %s</font>%s%s\n",$obj->{color},(' ' x $depth),$obj->{label},($beginFlag ? '&gt;&gt;' : '  '),(' ' x (4-$depth)),$line));
	    }
	    $pr->print("\n</pre>\n");
	}

	#
	# Status
	#
	$pr->print("<h2>Status: $statuscode</h2>\n") if $statuscode && $statuscode != 200;
	
        #
	# Headers in
	#
	$pr->print("<h2>Headers in</h2>\n<ul>\n");
	my %inHeaders = $r->headers_in();
	while (($key,$value) = each(%inHeaders)) {
	    if ($simhdr{$key}) {
		$pr->print("<li type=circle><b>$key:</b>  $value\n");
	    } else {
		$pr->print("<li type=disc><b>$key:</b>  $value\n");
	    }
	}
	$pr->print("</ul>");

	#
	# Headers out
	#
    	$pr->print("<h2>Headers out</h2>\n<ul>");
	my %outHeaders = $r->headers_out();
	my $gmtdate = gmtime;
	$pr->print("<li type=circle><b>Date:</b>  $gmtdate\n");
	my $serverType = $ENV{SERVER_SOFTWARE} || 'Apache/1.2 mod_perl/1.0'; 
	$pr->print("<li type=circle><b>Server:</b>  $serverType\n");
	while (($key,$value) = each(%outHeaders)) {
	    $pr->print("<li type=disc><b>$key:</b>  $value\n");
	}
	$pr->print("</ul><p>\n");
	$pr->print("<li type=circle><i>simulated or estimated headers</i></font><br>\n<p>\n");
    }
}

1;
