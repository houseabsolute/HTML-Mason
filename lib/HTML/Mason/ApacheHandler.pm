# Copyright (c) 1998-2000 by Jonathan Swartz. All rights reserved.
# This program is free software; you can redistribute it and/or modify it
# under the same terms as Perl itself.

use strict;
require 5.004;

#----------------------------------------------------------------------
#
# APACHE-SPECIFIC REQUEST OBJECT
#
package HTML::Mason::Request::ApacheHandler;
use vars qw(@ISA);
@ISA = qw(HTML::Mason::Request);

use HTML::Mason::MethodMaker
    ( read_write => [ qw( ah apache_req ) ] );

# Fields that can be set in new method, with defaults
my %reqfields =
    (ah => undef,
     apache_req => undef,
     cgi_object => undef,
     );

sub new
{
    my ($class,%options) = @_;
    my $interp = $options{interp} or die "HTML::Mason::Request::ApacheHandler::new: must specify interp\n";
    delete $options{interp};
    my $self = $class->SUPER::new(interp=>$interp);
    while (my ($key,$value) = each(%options)) {
	if (exists($reqfields{$key})) {
	    $self->{$key} = $value;
	} else {
	    die "HTML::Mason::Request::ApacheHandler::new: invalid option '$key'\n";
	}
    }
    return $self;
}

# Override flush_buffer to also call $r->rflush
sub flush_buffer
{
    my ($self, $content) = @_;
    $self->SUPER::flush_buffer($content);
    $self->apache_req->rflush;
}

sub cgi_object
{
    my ($self) = @_;

    if ($HTML::Mason::ApacheHandler::ARGS_METHOD ne '_cgi_args') {
	die "Can't call cgi_object method unless CGI.pm was used to handle incoming arguments.\n";
    } elsif (defined($_[1])) {
	$self->{cgi_object} = $_[1];
    } else {
	# We may not have created a CGI object if, say, request was a
	# GET with no query string. Create one on the fly if necessary.
	$self->{cgi_object} ||= CGI->new('');
    }

    return $self->{cgi_object};
}

#----------------------------------------------------------------------
#
# APACHEHANDLER OBJECT
#
package HTML::Mason::ApacheHandler;

#JS - 6/30 - seems to infinite loop when using debug...help?!
#use Apache::Constants qw(OK DECLINED SERVER_ERROR NOT_FOUND);
sub OK { return 0 }
sub DECLINED { return -1 }
sub SERVER_ERROR { return 500 }
sub NOT_FOUND { return 404 }
use Data::Dumper;
use File::Path;
use File::Spec;
use HTML::Mason::Interp;
use HTML::Mason::Commands;
use HTML::Mason::FakeApache;
use HTML::Mason::Tools qw(dumper_method html_escape pkg_installed);
use HTML::Mason::Utils;
use Apache::Status;

use HTML::Mason::MethodMaker
    ( read_write => [ qw( apache_status_title
			  auto_send_headers

			  decline_dirs
			  error_mode
			  interp
			  output_mode
			  top_level_predicate ) ]
      );

# use() params. Assign defaults, in case ApacheHandler is only require'd.
use vars qw($LOADED $ARGS_METHOD);

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
     );

sub import
{
    shift; # class not needed

    return if $LOADED;
    my %params = @_;

    # safe default.
    $params{args_method} ||= 'CGI';
    if ($params{args_method} eq 'CGI')
    {
	eval 'use CGI';
	die $@ if $@;
	$ARGS_METHOD = '_cgi_args';
    }
    elsif ($params{args_method} eq 'mod_perl')
    {
	eval 'use Apache::Request;';
	die $@ if $@;
	$ARGS_METHOD = '_mod_perl_args';
    }
    else
    {
	die "Invalid args_method parameter ('$params{args_method}') given to HTML::Mason::ApacheHandler in 'use'\n";
    }

    $LOADED = 1;
}

sub new
{
    my $class = shift;
    my $self = {
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

my %status_sub_defined = ();

sub _initialize {
    my ($self) = @_;

    my $interp = $self->interp;

    if ($Apache::Status::VERSION) {
	# Add an HTML::Mason menu item to the /perl-status page. Things we report:
	# -- Interp properties
	# -- loaded (cached) components
	my $name = $self->apache_status_title;
	unless ($status_sub_defined{$name}) {

	    my $title;
	    if ($name eq 'mason') {
		$title='HTML::Mason status';
	    } else {
		$title=$name;
		$name=~s/\W/_/g;
	    }

	    my $statsub = sub {
		my ($r,$q) = @_; # request and CGI objects
		return [] if !defined($r);
		my @strings = ();

		push (@strings,
		      qq(<FONT size="+2"><B>) . $self->apache_status_title . qq(</B></FONT><BR><BR>),
		      $self->interp_status);

		return \@strings;     # return an array ref
	    };
	    Apache::Status->menu_item ($name,$title,$statsub);
	    $status_sub_defined{$name}++;
	}
    }

    #
    # Create data subdirectories if necessary. mkpath will die on error.
    #
    foreach my $subdir (qw(preview)) {
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
            } grep ! ref $interp->{$_}, sort keys %$interp;

    push @strings, '</DL>',
            '<DL><DT><FONT SIZE="+1"><B>Cached components</B></FONT><DD>';

    if(my $cache = $interp->code_cache)
    {
	my $string;
	foreach my $key (sort keys %$cache) {
	    $string .= sprintf("<TT>%s (%s)</TT><BR>\n",$key,scalar(localtime($cache->{$key}->{lastmod})));
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

    if (lc($apreq->dir_config('Filter')) eq 'on') {
	$apreq = $apreq->filter_register;
    }

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
    # Create an Apache-specific request with additional slots.
    #
    my $request = new HTML::Mason::Request::ApacheHandler
	(ah=>$self,
	 interp=>$interp,
	 apache_req=>$apreq,
	 );
    
    eval { $retval = $self->handle_request_1($apreq, $request) };
    my $err = $@;
    my $err_code = $request->error_code;
    undef $request;  # ward off memory leak
    my $err_status = $err ? 1 : 0;

    if ($err) {
	#
	# If first component was not found, return NOT_FOUND. In case
	# of POST we must trick Apache into not reading POST content
	# again. Wish there were a more standardized way to do this...
	#
	# This $err_code stuff is really only used to communicate found
	# errors; it will be replaced with exceptions
	#
	if (defined($err_code) and $err_code eq 'top_not_found') {
	    if ($apreq->method eq 'POST') {
		$apreq->method('GET');
		$apreq->headers_in->unset('Content-length');
	    }
	    
	    # Log the error the same way that Apache does (taken from default_handler in http_core.c)
	    $apreq->log_error("[Mason] File does not exist: ",$apreq->filename . ($apreq->path_info ? $apreq->path_info : ""));
	    return NOT_FOUND;
	}

	#
	# Take out date stamp and (eval nnn) prefix
	# Add server name, uri
	#
	$err =~ s@^\[[^\]]*\] \(eval [0-9]+\): @@mg;
	$err = html_escape($err);
	$err = sprintf("while serving %s %s\n%s",$apreq->server->server_hostname,$apreq->uri,$err);

	if ($self->error_mode eq 'fatal') {
	    die ("System error:\n$err\n");
	} elsif ($self->error_mode eq 'html') {
	    if (!http_header_sent($apreq)) {
		$apreq->content_type('text/html');
		$apreq->send_http_header();
	    }
	    print("<h3>System error</h3><p><pre><font size=-1>$err</font></pre>\n");
	}
    }

    $interp->write_system_log('REQ_END', $self->{request_number}, $err_status);
    return ($err) ? &OK : (defined($retval)) ? $retval : &OK;
}

#
# Shorthand for various data subdirectories and files.
#
sub preview_dir { return shift->interp->data_dir . "/preview" }

sub handle_request_1
{
    my ($self,$r,$request) = @_;
    my $interp = $self->interp;

    #
    # If filename is a directory, then either decline or simply reset
    # the content type, depending on the value of decline_dirs.
    #
    # ** We should be able to use $r->finfo here, but finfo is broken
    # in some versions of mod_perl (e.g. see Shane Adams message on
    # mod_perl list on 9/10/00)
    #
    my $is_dir = -d $r->filename;
    my $is_file = -f _;

    if ($is_dir) {
	if ($self->decline_dirs) {
	    return DECLINED;
	} else {
	    $r->content_type(undef);
	}
    }

    #
    # Append path_info if filename does not represent an existing file
    # (mainly for dhandlers).
    #
    my $pathname = $r->filename;
    $pathname .= $r->path_info unless $is_file;

    #
    # Compute the component path via the resolver. Return NOT_FOUND on failure.
    #
    my $comp_path = $interp->resolver->file_to_path($pathname,$interp);
    unless ($comp_path) {
	$r->warn("[Mason] Cannot resolve file to component: $pathname (is file outside component root?)");
	return NOT_FOUND;
    }

    #
    # Return NOT_FOUND if file does not pass top level predicate.
    #
    if ($is_file and defined($self->top_level_predicate) and !$self->top_level_predicate->($r->filename)) {
	$r->warn("[Mason] File fails top level predicate: ".$r->filename);
	return NOT_FOUND;
    }

    #
    # Parse arguments. $ARGS_METHOD is set by the import subroutine
    # (_cgi_args or _mod_perl_args).  We pass a reference to $r because
    # _mod_perl_args upgrades $r to the Apache::Request object.
    # 
    my %args;
    die "ARGS_METHOD not defined! Did you 'use HTML::Mason::ApacheHandler'?" unless defined($ARGS_METHOD);
    %args = $self->$ARGS_METHOD(\$r,$request);
    
    #
    # Deprecated output_mode parameter - just pass to request out_mode.
    #
    if (my $mode = $self->output_mode) {
	$request->out_mode($mode);
    }

    #
    # Set up interpreter global variables.
    #
    $interp->set_global(r=>$r);

    #
    # Craft the out method for this request to handle automatic http
    # headers.
    #
    my $retval;
    if ($self->auto_send_headers) {
	my $headers_sent = 0;
	my $delay_buf = '';
	my $out_method = sub {
	    # Check to see if the headers have been sent, first by fast
	    # variable check, then by slightly slower $r check.
	    unless ($headers_sent) {
		unless (http_header_sent($r)) {
		    # If header has not been sent, buffer initial whitespace
		    # so as to delay headers.
		    if ($_[0] !~ /\S/) {
			$delay_buf .= $_[0];
			return;
		    } else {
			$r->send_http_header();
			
			# If this is a HEAD request and our Mason request is
			# still active, abort it.
			if ($r->header_only) {
			    $request->abort if $request->depth > 0;
			    return;
			}
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
	$request->out_method($out_method);

	$retval = $request->exec($comp_path, %args);

	# On a success code, send headers and any buffered whitespace
	# if it has not already been sent. On an error code, leave it
	# to Apache to send the headers.
	if (!$headers_sent and (!$retval or $retval==200)) {
	    $r->send_http_header() unless http_header_sent($r);
	    $interp->out_method->($delay_buf) unless $delay_buf eq '';
	}
    } else {
	$retval = $request->exec($comp_path, %args);
    }
    undef $request; # ward off leak
    return $retval;
}

#
# Get %args hash via CGI package
#
sub _cgi_args
{
    my ($self, $rref, $request) = @_;

    my $r = $$rref;

    # For optimization, don't bother creating a CGI object if request
    # is a GET with no query string
    return if $r->method eq 'GET' && !scalar($r->args);

    my $q = CGI->new;
    $request->cgi_object($q);

    my %args;
    my $methods = $r->method eq 'GET' ? [ 'param' ] : [ 'param', 'url_param' ];
    foreach my $method (@$methods) {
	foreach my $key ( $q->$method() ) {
	    foreach my $value ( $q->$method($key) ) {
		if (exists($args{$key})) {
		    if (ref($args{$key}) eq 'ARRAY') {
			push @{ $args{$key} }, $value;
		    } else {
			$args{$key} = [$args{$key}, $value];
		    }
		} else {
		    $args{$key} = $value;
		}
	    }
	}
    }

    return %args;
}

#
# Get %args hash via Apache::Request package. As a side effect, assign the
# new Apache::Request package back to $r, unless $r is already an Apache::Request.
#
sub _mod_perl_args
{
    my ($self, $rref, $request) = @_;

    my $apr = $$rref;
    unless (UNIVERSAL::isa($apr, 'Apache::Request')) {
	$apr = Apache::Request->new($apr);
	$$rref = $apr;
    }
    
    return unless $apr->param;

    my %args;
    foreach my $key ( $apr->param ) {
	foreach my $value ( $apr->param($key) ) {
	    if (exists($args{$key})) {
		if (ref($args{$key}) eq 'ARRAY') {
		    push @{ $args{$key} }, $value;
		} else {
		    $args{$key} = [$args{$key}, $value];
		}
	    } else {
		$args{$key} = $value;
	    }
	}
    }

    return %args;
}

#
# Determines whether the http header has been sent.
#
sub http_header_sent { shift->header_out("Content-type") }

1;
