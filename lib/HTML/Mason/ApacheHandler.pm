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
    my $interp = $options{interp} or
	HTML::Mason::Exception::Params->throw( error => "HTML::Mason::Request::ApacheHandler->new: must specify interp\n" );
    delete $options{interp};
    my $self = $class->SUPER::new(interp=>$interp);
    while (my ($key,$value) = each(%options)) {
	if (exists($reqfields{$key})) {
	    $self->{$key} = $value;
	} else {
	    HTML::Mason::Exception::Params->throw( error => "HTML::Mason::Request::ApacheHandler->new: invalid option '$key'\n" );
	}
    }
    return $self;
}

# Override flush_buffer to also call $r->rflush
sub flush_buffer
{
    my ($self) = @_;
    $self->SUPER::flush_buffer;
    $self->apache_req->rflush;
}

sub cgi_object
{
    my ($self) = @_;

    HTML::Mason::Exception->throw( error => "Can't call cgi_object method unless CGI.pm was used to handle incoming arguments.\n" )
	unless $CGI::VERSION;

    if (defined($_[1])) {
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
use Apache::Request;
use Data::Dumper;
use File::Path;
use File::Spec;
use HTML::Mason::Interp;
use HTML::Mason::Error qw(error_process error_display_html);
use HTML::Mason::Tools qw(dumper_method html_escape make_fh pkg_installed);
use HTML::Mason::Utils;
use Params::Validate qw(:all);
Params::Validate::set_options( on_fail => sub { HTML::Mason::Exception::Params->throw( error => join '', @_ ) } );

use Apache;
use Apache::Status;

use HTML::Mason::MethodMaker
    ( read_write => [ qw( apache_status_title
                          args_method
			  auto_send_headers
			  decline_dirs
			  error_mode
			  interp
			  output_mode
			  top_level_predicate ) ]
      );

# use() params. Assign defaults, in case ApacheHandler is only require'd.
use vars qw($AH $VERSION);

$VERSION = sprintf '%2d.%02d', q$Revision$ =~ /(\d+)\.(\d+)/;

# Fields that can be set in new method, with defaults
my %fields =
    (
     apache_status_title => 'mason',
     args_method => 'mod_perl',
     auto_send_headers => 1,
     decline_dirs => 1,
     error_mode => 'html',
     interp => undef,
     output_mode => undef,    # deprecated - now interp->out_mode
     top_level_predicate => undef,
     );

# This is my best guess as to whether we are being configured via the
# conf file or not.  Without a comp root it will blow up sooner or
# later anyway.  This may not be the case in the future though.
sub _in_apache_conf_file
{
    # We don't want to try to read the configuration til we're in a
    # request if mod_perl <= 1.21
    return 0 if $mod_perl::VERSION <= 1.21 && ( $Apache::Server::Starting || $Apache::ServerStarting ||
						$Apache::Server::ReStarting || $Apache::ServerReStarting );
    return $ENV{MOD_PERL} && ( _get_list_param('CompRoot') ||
			       _get_boolean_param('MultipleConfig') );
}

sub _make_ah
{
    return $AH if $AH && ! _get_boolean_param('MultipleConfig');

    my %p;

    $p{apache_status_title} = _get_string_param('ApacheStatusTitle');
    $p{auto_send_headers} = _get_boolean_param('ArgsMethod');
    $p{auto_send_headers} = _get_boolean_param('AutoSendHeaders');
    $p{debug_handler_proc} = _get_string_param('DebugHandlerProc');
    $p{debug_handler_script} = _get_string_param('DebugHandlerScript');
    $p{debug_mode} = _get_string_param('DebugMode');
    $p{debug_perl_binary} = _get_string_param('DebugPerlBinary');
    $p{decline_dirs} = _get_boolean_param('DeclineDirs');
    $p{error_mode} = _get_string_param('ErrorMode');
    $p{top_level_predicate} = _get_code_param('TopLevelPredicate');

    foreach (keys %p)
    {
	delete $p{$_} unless defined $p{$_};
    }

    $AH = HTML::Mason::ApacheHandler->new( interp => _make_interp(),
					   %p,
					 );

    return $AH;
}

sub _make_interp
{
    my %p;

    $p{allow_recursive_autohandlers} = _get_boolean_param('AllowRecursiveAutohandlers');
    $p{autohandler_name}    = _get_string_param('AutohandlerName');
    $p{code_cache_max_size} = _get_string_param('CodeCacheMaxSize');
    $p{current_time}        = _get_string_param('CurrentTime');
    $p{data_cache_dir}      = _get_string_param('DataCacheDir');
    $p{dhandler_name}       = _get_string_param('DhandlerName');
    $p{die_handler}         = _get_code_param('DieHandler');
    $p{max_recurse}         = _get_string_param('MaxRecurse');
    $p{out_method}          = _get_code_param('OutMethod');
    $p{out_mode}            = _get_string_param('OutMode');
    $p{preloads}              = [ _get_list_param('Preloads') ];
    delete $p{preloads} unless @{ $p{preloads} };
    $p{static_file_root}      = _get_string_param('StaticFileRoot');
    $p{system_log_events}     = _get_string_param('SystemLogEvents');
    $p{system_log_file}       = _get_string_param('SystemLogFile');
    $p{system_log_separator}  = _get_string_param('SystemLogSepartor');
    $p{use_autohandlers}      = _get_boolean_param('UseAutohandlers');
    $p{use_data_cache}        = _get_boolean_param('UseDataCache');
    $p{use_dhandlers}         = _get_boolean_param('UseDhandlers');
    $p{use_object_files}      = _get_boolean_param('UseObjectFiles');
    $p{use_reload_file}       = _get_boolean_param('UseReloadFile');

    my @comp_root = _get_list_param('CompRoot', 1);
    if (@comp_root == 1 && $comp_root[0] !~ /=>/)
    {
	$p{comp_root} = $comp_root[0];
    }
    else
    {
	my @root;
	foreach my $root (@comp_root)
	{
	    my ($k, $v) = split /\s*=>\s*/, $root;
	    HTML::Mason::Exception::Params->throw( error => "Configuration parameter MasonCompRoot must be either a singular value or a multiple 'hash' values like 'foo => /home/mason/foo'" )
		unless defined $k && defined $v;
	    push @{ $p{comp_root} }, [ $k => $v ];
	}
    }
    $p{data_dir} = _get_string_param('DataDir', 1);

    # If not defined we'll use the defaults
    foreach (keys %p)
    {
	delete $p{$_} unless defined $p{$_};
    }

    my $interp = HTML::Mason::Interp->new( compiler => _make_compiler(),
					   %p,
					 );

    # if version <= 1.21 then these files shouldn't be created til
    # after a fork so they should have the right ids anyway
    if ($interp->files_written && $mod_perl::VERSION > 1.21 && ! ($> || $<))
    {
	chown Apache->server->uid, Apache->server->gid, $interp->files_written
	    or HTML::Mason::Exception::System->throw( error => "Can't change ownership of files written by interp object\n" );
    }

    return $interp;
}

sub _make_compiler
{
    my %p;
    $p{allowed_globals} = [ _get_list_param('AllowedGlobals') ];
    delete $p{allowed_globals} unless @{ $p{allowed_globals} };

    $p{default_escape_flags} = _get_string_param('DefaultEscapeFlags');
    $p{ignore_warnings_expr} = _get_string_param('IgnoreWarningsExpr');
    $p{in_package} = _get_string_param('InPackage');
    $p{postamble} = _get_string_param('Postamble');
    $p{preamble} = _get_string_param('Preamble');
    $p{use_strict} = _get_boolean_param('UseStrict');

    $p{preprocess} = _get_code_param('Preprocess');
    $p{postprocess_perl} = _get_code_param('PostprocessPerl');
    $p{postprocess_text} = _get_code_param('PostprocessText');

    # If not defined we'll use the defaults
    foreach (keys %p)
    {
	delete $p{$_} unless defined $p{$_};
    }

    require HTML::Mason::Compiler::ToObject;
    return HTML::Mason::Compiler::ToObject->new(%p);
}

sub _get_string_param
{
    my ($p, $val) = _get_val(@_[0, 1]);

    return $val;
}

sub _get_boolean_param
{
    my ($p, $val) = _get_val(@_[0, 1]);

    return $val;
}

sub _get_code_param
{
    my ($p, $val) = _get_val(@_[0, 1]);

    return unless $val;

    my $sub_ref = eval $val;

    HTML::Mason::Exception::Params->throw( error => "Configuration parameter '$p' is not valid perl:\n$@\n" )
	if $@;

    return $sub_ref;
}

sub _get_list_param
{
    my ($p, @val) = _get_val(@_[0,1], 1);
    if (@val == 1 && ! defined $val[0])
    {
	@val = ();
    }

    return @val;
}

sub _get_val
{
    my ($p, $required, $wantarray) = @_;
    $p = "Mason$p";

    if ( $mod_perl::VERSION <= 1.21 && ( $Apache::Server::Starting || $Apache::ServerStarting ||
					 $Apache::Server::ReStarting || $Apache::ServerReStarting ) )
    {
	HTML::Mason::Exception->throw( error => "Can't get configuration info during server startup with mod_perl <= 1.21" );
    }
    my $c = Apache->request ? Apache->request : Apache->server;

    my @val = $mod_perl::VERSION < 1.24 ? $c->dir_config($p) : $c->dir_config->get($p);

    HTML::Mason::Exception::Params->throw( error => "Only a single value is allowed for configuration parameter '$p'\n" )
	if @val > 1 && ! $wantarray;

    HTML::Mason::Exception::Params->throw( error => "Configuration parameter '$p' is required\n" )
	if $required && ! defined $val[0];

    return ($p, $wantarray ? @val : $val[0]);
}

sub new
{
    my $class = shift;
    my $self = {
	request_number => 0,
	%fields,
    };

    validate( @_,
	      { apache_status_title => { type => SCALAR, optional => 1 },
		args_method => { type => SCALAR, optional => 1 },
		auto_send_headers => { type => SCALAR | UNDEF, optional => 1 },
		decline_dirs => { type => SCALAR | UNDEF, optional => 1 },
		error_mode => { type => SCALAR, optional => 1 },
		output_mode => { type => SCALAR | UNDEF, optional => 1 },
		top_level_predicate => { type => CODEREF | UNDEF, optional => 1 },
		debug_mode => { type => SCALAR, optional => 1 },
		debug_perl_binary => { type => SCALAR, optional => 1 },
		debug_handler_script => { type => SCALAR, optional => 1 },
		debug_handler_proc => { type => SCALAR, optional => 1 },
		debug_dir_config_keys => { type => ARRAYREF, optional => 1 },

		# the only required param
		interp => { isa => 'HTML::Mason::Interp' },
	      }
	    );

    my (%options) = @_;

    HTML::Mason::Exception::Params->throw( error => "args_method parameter must be either 'CGI' or 'mod_perl'\n" )
	if exists $options{args_method} && $options{args_method} !~ /^(?:CGI|mod_perl)$/;

    HTML::Mason::Exception::Params->throw( error => "error_mode parameter must be one of 'html', 'fatal', 'raw_html', or 'raw_fatal'\n" )
	if exists $options{error_mode} && $options{error_mode} !~ /^(?:raw_)?(?:html|fatal)$/;

    while (my ($key,$value) = each(%options)) {
	$self->{$key} = $value;
    }

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
    $interp->compiler->set_allowed_globals(qw($r));
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
	# Do not process error at all in raw mode or if die handler was overriden.
	#
	my $raw_err = $err;
	unless ($self->error_mode =~ /^raw_/ or $interp->die_handler_overridden) {
	    $err = error_process ($err, $request);
	}

	#
	# In fatal/raw_fatal mode, compress error to one line (for Apache logs) and die.
	# In html/raw_html mode, call error_display_html and print result.
	# The raw_ modes correspond to pre-1.02 error formats.
	#
	# [This is a load of spaghetti. It will be cleaned up in 1.2 when we lose
	# debug mode and standardize error handling.]
	#
	if ($self->error_mode eq 'fatal') {
	    unless ($interp->die_handler_overridden) {
		$err =~ s/\n/\t/g;
		$err =~ s/\t$//g;
		$err .= "\n" if $err !~ /\n$/;
	    }
	    die $err;
	} elsif ($self->error_mode eq 'raw_fatal') {
	    die ("System error:\n$raw_err\n");
	} elsif ($self->error_mode =~ /html$/) {
	    unless ($interp->die_handler_overridden) {
		my $debug_msg;
		if ($self->error_mode =~ /^raw_/) {
		    $err .= "$debug_msg\n" if $debug_msg;
		    $err = "<h3>System error</h3><p><pre><font size=-1>$raw_err</font></pre>\n";
		} else {
		    $err .= "Debug info: $debug_msg\n" if $debug_msg;
		    $err = error_display_html($err,$raw_err);
		}
	    }
	    # Send HTTP headers if they have not been sent.
	    if (!http_header_sent($apreq)) {
		$apreq->content_type('text/html');
		$apreq->send_http_header();
	    }
	    print($err);
	}
    }
    undef $request;  # ward off memory leak

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
    my $comp_path = $interp->resolver->file_to_path($pathname);
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

    my %args;
    my $args_method = $self->args_method eq 'mod_perl' ? '_mod_perl_args' : '_cgi_args';
    %args = $self->$args_method(\$r,$request);

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

    # Checking scalar $r->args when the method is POST is important
    # because otherwise ->url_param returns a parameter named
    # 'keywords' with a value of () (empty array).  This is apparently
    # a feature related to <ISINDEX> queries or something (see the
    # CGI.pm) docs.  It makes my head heart. - dave
    my @methods = $r->method eq 'GET' || ! scalar $r->args ? ( 'param' ) : ( 'param', 'url_param' );
    foreach my $key ( map { $q->$_() } @methods ) {
	next if exists $args{$key};
	my @values = map { $q->$_($key) } @methods;
	$args{$key} = @values == 1 ? $values[0] : \@values;
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

    my %args;
    foreach my $key ( $apr->param ) {
	my @values = $apr->param($key);
	$args{$key} = @values == 1 ? $values[0] : \@values;
    }

    return %args;
}

#
# Determines whether the http header has been sent.
#
sub http_header_sent { shift->header_out("Content-type") }

#
# PerlHandler HTML::Mason::ApacheHandler
#
sub handler
{
    my $r = shift;

    my $ah = _make_ah;

    return $ah->handle_request($r);
}

1;
