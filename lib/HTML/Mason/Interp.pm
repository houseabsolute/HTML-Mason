# Copyright (c) 1998 by Jonathan Swartz. All rights reserved.
# This program is free software; you can redistribute it and/or modify it
# under the same terms as Perl itself.

package HTML::Mason::Interp;
require 5.004;
require Exporter;
@ISA = qw(Exporter);
@EXPORT = qw();
@EXPORT_OK = qw();

use strict;
use Carp;
use File::Path;
use File::Basename;
use File::Find;
use IO::File;
use IO::Seekable;
use HTML::Mason::Parser;
use HTML::Mason::Tools qw(read_file pkg_loaded);
use HTML::Mason::Commands qw();
use HTML::Mason::Config;
require Time::HiRes if $HTML::Mason::Config{use_time_hires};

use vars qw($AUTOLOAD $_SUB %_ARGS);

my %fields =
    (alternate_sources => undef,
     comp_root => undef,
     code_cache_mode => 'all',
     current_time => 'real',
     data_dir => undef,
     system_log_file => undef,
     system_log_events => '',
     system_log_separator => "\cA",
     max_recurse => 16,
     parser => undef,
     preloads => [],
     static_file_root => '',
     use_data_cache => 1,
     use_object_files => 1,
     use_reload_file => 0,
     verbose_compile_error => 0,
     data_cache_dir => '',
     );
# Minor speedup: create anon. subs to reduce AUTOLOAD calls
foreach my $f (keys %fields) {
    next if $f =~ /^current_time|system_log_events$/;  # don't overwrite real sub.
    no strict 'refs';
    *{$f} = sub {my $s=shift; return @_ ? ($s->{$f}=shift) : $s->{$f}};
}

sub new
{
    my $class = shift;
    my $self = {
	_permitted => \%fields,
	%fields,
	data_cache_store => {},
        code_cache => {},
	files_written => [],
	hooks => [],
	hook_index => {},
	last_reload_time => 0,
	last_reload_file_pos => 0,
	out_method => sub { print $_[0] },
	system_log_fh => undef
    };
    my (%options) = @_;
    my ($rootDir,$outMethod,$systemLogEvents);
    while (my ($key,$value) = each(%options)) {
	if (exists($fields{$key})) {
	    $self->{$key} = $value;
	} elsif ($key eq 'out_method') {
	    $outMethod = $value;
	} elsif ($key eq 'system_log_events') {
	    $systemLogEvents = $value;
	} else {
	    die "HTML::Mason::Interp::new: invalid option '$key'\n";
	}
    }
    $self->{data_cache_dir} ||= ($self->{data_dir} . "/cache");
    die "HTML::Mason::Interp::new: must specify value for comp_root\n" if !$self->{comp_root};
    die "HTML::Mason::Interp::new: must specify value for data_dir\n" if !$self->{data_dir};
    bless $self, $class;
    $self->out_method($outMethod) if ($outMethod);
    $self->system_log_events($systemLogEvents) if ($systemLogEvents);
    $self->_initialize;
    return $self;
}

sub _initialize
{
    my ($self) = shift;
    $self->{code_cache} = {};
    $self->{data_cache_store} = {};

    #
    # Create parser if not provided
    #
    if (!$self->parser) {
	my $p = new HTML::Mason::Parser;
	$self->parser($p);
    }

    #
    # Create data subdirectories if necessary. mkpath will die on error.
    #
    foreach my $subdir (qw(obj cache cache/locks etc)) {
	my @newdirs = mkpath($self->data_dir."/$subdir",0,0775);
	$self->push_files_written(@newdirs);
    }
    
    #
    # Open system log file
    #
    if ($self->system_log_events) {
	$self->system_log_file($self->data_dir . "/etc/system.log") if !$self->system_log_file;
	my $fh = new IO::File ">>".$self->system_log_file
	    or die "Couldn't open system log file ".$self->{system_log_file}." for append";
	$fh->autoflush(1);
	$self->{system_log_fh} = $fh;
    }
    
    #
    # Preloads
    #
    if ($self->preloads) {
	(my $savemode,$self->{code_cache_mode}) = ($self->{code_cache_mode},'all');
	my $slen = length($self->comp_root);
	foreach my $p (@{$self->preloads}) {
	    next if ($p !~ m@^/@);
	    my $fullPath = $self->comp_root . $p;
	    $fullPath =~ s@/$@@g;
	    if (-d $fullPath) {
		my $sub = sub {
		    if (-f) {
			my $file = $_;
			$file =~ s/^\./$fullPath/;
			my $compPath = substr($file,$slen);
			$self->load($compPath);
		    }
		};
		find($sub,$fullPath);
	    } elsif (-f $fullPath) {
		my $compPath = substr($fullPath,$slen);
		$self->load($compPath);
	    }
	}
	$self->{code_cache_mode} = $savemode;	
    }

    #
    # Adjust to current size of reload file
    #
    if ($self->use_reload_file && -f $self->reload_file) {
	$self->{last_reload_file_pos} = [stat($self->reload_file)]->[7];
	$self->{last_reload_time} = [stat($self->reload_file)]->[9];
    }
}

sub data_cache_filename
{
    my ($self, $path) = @_;
    my $cacheFile = $path;
    $cacheFile =~ s@^/@@;
    $cacheFile =~ s@/@::@g;
    $cacheFile = $self->data_cache_dir."/".$cacheFile;
    return $cacheFile;
}

#
# Shorthand for various data subdirectories and files.
#
sub object_dir { return shift->data_dir . "/obj" }
sub reload_file { return shift->data_dir . "/etc/reload.lst" }
#this is now explicit
#sub data_cache_dir { return shift->data_dir . "/cache" }

#
# exec is the initial entry point for executing a component.
# It initializes globals like stack and exec_state. It is
# non-reentrant (i.e. it should be not be called from inside
# another exec).
#
# exec_next is the reentrant continuation function. For example,
# it is called by mc_comp from inside components.
#
sub exec {
    my ($self, $callPath, %args) = @_;
    my ($result);

    $self->{stack} = [];
    $self->{exec_state} = {
	suppressed_hooks => {},
	abort_flag => 0,
	abort_retval => undef,
	error_flag => 0,
	request_code_cache => {}
    };
    $self->check_reload_file if ($self->use_reload_file);
    return $self->exec_next($callPath,%args);
}

#
# Check if reload file has changed. If so, read paths from last read
# position to end of file and delete those paths from the cache.
#
sub check_reload_file {
    my ($self) = @_;
    my $reloadFile = $self->reload_file;
    return if (!-f $reloadFile);
    my $lastmod = [stat($reloadFile)]->[9];
    if ($lastmod > $self->{last_reload_time}) {
	my ($block);
	my $length = [stat($reloadFile)]->[7];
	my $fh = new IO::File $reloadFile;
	return if !$fh;
	my $pos = $self->{last_reload_file_pos};
	$fh->seek(&SEEK_SET,$pos);
	read($fh,$block,$length-$pos);
	$self->{last_reload_time} = $lastmod;
	$self->{last_reload_file_pos} = $fh->tell;
	my @lines = split("\n",$block);
	foreach my $compPath (@lines) {
	    delete($self->{code_cache}->{$compPath});
	}
    }
}

sub exec_next {
    my ($self, $callPath, %args) = @_;
    my ($err);

    $callPath =~ s@/$@@ if ($callPath ne '/');
    my $allowHandlers = $args{ALLOW_HANDLERS};
    my $store = $args{STORE};
    delete($args{ALLOW_HANDLERS});
    delete($args{STORE});

    #
    # Load the component into a subroutine. If code caching is
    # per-request, then check and modify the per-request cache as
    # necessary.
    #
    my (@info);
    if ($self->{code_cache_mode} eq 'request') {
	if (my $inforef = $self->{exec_state}->{request_code_cache}->{$callPath}) {
	    @info = @$inforef;
	} else {
	    (@info) = $self->load($callPath);
	    $self->{exec_state}->{request_code_cache}->{$callPath} = [@info];
	}
    } else {
	(@info) = $self->load($callPath);
    }
    if (!@info) {
	if (!$allowHandlers) {
	    die "could not find component for path '$callPath'\n";
	} else {
	    #
	    # This hack implements the ALLOW_HANDLERS flag for
	    # backward compatibility with Scribe.  Looks for home and
	    # dhandler files when component not found.  Hopefully can
	    # remove someday soon.
	    #
	    my $pathInfo = '';
	    my $p = $callPath;
	    if (!(@info = $self->load("$p/home"))) {
		while (!(@info = $self->load("$p/dhandler"))) {
		    my ($basename,$dirname) = fileparse($p);
		    $pathInfo = "/$basename$pathInfo";
		    $p = substr($dirname,0,-1);
		    die "could not find component for path '$callPath'\n" if $p !~ /\S/;
		}
		$callPath = "$p/dhandler";
		my $r = $self->vars('server') or die "No ALLOW_HANDLERS outside of Apache";
		$r->path_info($pathInfo);
	    } else {
		$callPath = "$p/home";
	    }
	}
    }
    my ($sub,$sourceFile) = @info;

    #
    # Push an empty locals record on the stack.
    #
    unshift(@{$self->{stack}},{});
    
    #
    # $INTERP is a global containing this interpreter. This needs to
    # be defined in the HTML::Mason::Commands package, as well
    # as the component package if that is different.
    #
    local $HTML::Mason::Commands::INTERP = $self;
    $self->set_global(INTERP=>$self) if ($self->{parser}->{in_package} ne 'HTML::Mason::Commands');

    #
    # Check for maximum recursion.
    #
    my $depth = $self->depth;
    if ($depth > $self->max_recurse) {
	my $max = $self->max_recurse;
	shift(@{$self->{stack}});
        die ">$max levels deep in component stack (infinite recursive call?)\n";
    }

    #
    # Set up stack locals.
    #
    my $locals = {};
    $locals->{callPath} = $callPath;
    $locals->{truePath} = $callPath;
    $locals->{sourceFile} = $sourceFile;
    ($locals->{parentPath}) = ($callPath =~ m@^(.*)/([^/]*)$@);
    $locals->{parentPath} = '/' if ($locals->{parentPath} !~ /\S/);
    $locals->{callFunc} = $sub;
    $locals->{callArgs} = \%args;
    if ($store) {
	die "exec_next: STORE is not a scalar reference" if ref($store) ne 'SCALAR';
	$$store = '';
	$locals->{sink} = sub { $$store .= $_[0] }
    } elsif ($depth==1) {
	$locals->{sink} = $self->out_method;
    } else {
	$locals->{sink} = $self->{stack}->[1]->{sink};
    }
    $self->{stack}->[0] = $locals;

    #
    # Call start_comp hooks.
    #
    $self->call_hooks(type=>'start_comp');

    #
    # CODEREF_NAME maps component coderefs to component names (for profiling)
    #
    $HTML::Mason::CODEREF_NAME{$sub} = $sourceFile if $::opt_P;

    #
    # Call component subroutine in an eval context.
    #
    my ($result, @result);
    {
	local $_SUB = $sub;
	local %_ARGS = %args;
	local $SIG{'__DIE__'} = sub { confess($_[0]) };
	if (wantarray) {
	    @result = eval {$sub->(%args);};
	} else {
	    $result = eval {$sub->(%args);};
	}
    }

    #
    # If an error occurred...
    #
    $err = $@;
    if ($err) {
	
	#
	# If we are in an abort state (e.g. user called mc_abort),
	# just die up til top level.
	#
        if ($self->{exec_state}->{abort_flag}) {
            shift(@{$self->{stack}});
            if (scalar(@{$self->{stack}}) > 0) {
                die "aborted";
            } else {
                return $self->{exec_state}->{abort_retval};
            }
        }

	#
	# If this is the first level to process the error,
	# add some information and clean it up.
	#
        if (!$self->{exec_state}->{error_flag}) {
	    $self->{exec_state}->{error_flag} = 1;
            my $i = index($err,'HTML::Mason::Interp::exec');
            $err = substr($err,0,$i)."..." if $i!=-1;
            $err = "error while executing $callPath:\n$err";
            if ($self->depth > 1) {
                $err .= "backtrace: ";
                $err .= join(" <= ",map($_->{callPath},@{$self->{stack}}));
            }
	}
	$err =~ s@\[[^\]]*\] ([^:]+:) @@mg;

	#
	# Pass error along to next level.
	#
	shift(@{$self->{stack}});
	die ("$err\n");
    }

    #
    # Call end_comp hooks.
    #
    $self->call_hooks(type=>'end_comp');

    #
    # Pop stack and return.
    #
    shift(@{$self->{stack}});
    return wantarray ? @result : $result;
}

sub debug_hook
{
    1;
}

#
# Subroutine called for pure text components. Read and print out source.
#
sub pure_text_handler
{
    my $interp = $HTML::Mason::Commands::INTERP;
    $interp->call_hooks(type=>'start_primary');
    my $srcfile = $interp->locals->{sourceFile};
    my $srctext = read_file($srcfile);
    $interp->locals->{sink}->($srctext);
    $interp->call_hooks(type=>'end_primary');
    return undef;
}

#
# Load the component <$path> into a subroutine, possibly parsing
# the source and/or caching the code. Returns a list containing
# the code reference and source filename, or an empty list if the
# component was not found.
#
sub load {
    my ($self,$path) = @_;
    my ($sub,$err,$maxfilemod,$srcfile,$objfile,$objfilemod,$srcfilemod) =
       (undef);  # kludge to load $sub, prevent "use of uninit .." error
    my (@srcstat, $srcisfile, @objstat, $objisfile);
    my $codeCache = $self->{code_cache};
    my $compRoot = $self->comp_root;
    $srcfile = $compRoot . $path;

    #
    # If using reload file, assume that we have a cached subroutine or
    # object file (also assume we're using obj files). If no obj file
    # exists, this is likely a dhandler request.
    #
    if ($self->use_reload_file) {
	return @{$codeCache->{$path}->{info}} if exists($codeCache->{$path});

	$objfile = $self->object_dir . substr($srcfile,length($compRoot));
	return () unless (-f $objfile);   # component not found
	
	$self->write_system_log('COMP_LOAD', $path);	# log the load event
	$self->parser->evaluate(script_file=>$objfile,code=>\$sub,error=>\$err);
	if ($err) {
	    $sub = sub { die "Error while loading '$objfile' at runtime:\n$err\n" };
	} elsif (!$sub) {
	    $sub = \&pure_text_handler;
	}
	
	my @info = ($sub,$srcfile);
	if ($self->{code_cache_mode} eq 'all') {
	    $codeCache->{$path}->{info} = \@info;
	}
	return @info;
    }
    
    #
    # Determine source and (possibly) object filename.
    # If alternate sources are defined, check those first.
    #
    if (defined($self->alternate_sources)) {
	my @alts = $self->alternate_sources->('comp',$path);
	foreach my $alt (@alts) {
	    @srcstat = stat ($compRoot . $alt);
	    if (-f _) {
		$srcfile = $compRoot . $alt;
		last;
	    }
	}
    }
    @srcstat = stat $srcfile unless @srcstat;
    $srcfilemod = $srcstat[9];
    $srcisfile = -f _;
    return () unless ($srcisfile);
    
    if ($self->use_object_files) {
	$objfile = $self->object_dir . substr($srcfile,length($compRoot));
	@objstat = stat $objfile;
	$objisfile = -f _;
    }
    
    #
    # If code cache contains an up to date entry for this path,
    # use the cached sub.
    #
    if (exists($codeCache->{$path})                    and
	$codeCache->{$path}->{lastmod} >= $srcfilemod  and
	$codeCache->{$path}->{info}->[1] eq $srcfile) {
	return @{$codeCache->{$path}->{info}};
    } else {
	$objfilemod = (defined($objfile) and $objisfile) ? $objstat[9] : 0;
	
	#
	# Determine a subroutine.
	#
	# If an object file exists, and it is up to date with the
	# source file, eval the file. A blank object file signifies
	# a pure text component.
	#
	$self->write_system_log('COMP_LOAD', $path);	# log the load event

	if ($objfile and $objfilemod >= $srcfilemod) {
	    $self->parser->evaluate(script_file=>$objfile,code=>\$sub,error=>\$err);
	    if ($err) {
		$sub = sub { die "Error while loading '$objfile' at runtime:\n$err\n" };
	    } elsif (!$sub) {
		$sub = \&pure_text_handler;
	    }
	#
	# Parse the source file, and possibly save result in object file.
	#
	} else {
	    my ($script,$objtext,$errmsg,$pureTextFlag);
	    my $success = $self->parser->parse(script_file=>$srcfile,
					       result_text=>\$objtext,
					       error=>\$errmsg,
					       result_code=>\$sub,
					       pure_text_flag=>\$pureTextFlag);
	    $sub = \&pure_text_handler if ($pureTextFlag);
	    if (!$success) {
		$errmsg = "Error during compilation of $srcfile:\n$errmsg\n";
		if ($objtext && $self->verbose_compile_error) {
		    my @lines = split(/\n/,$objtext);
		    for (my $l = 0; $l < @lines; $l++) {
			$errmsg .= sprintf("%4d %s\n",$l+1,$lines[$l]);
		    }
		}
		$errmsg =~ s/\'/\\\'/g;
		$errmsg =~ s/\(eval [0-9]\) //g;
		($errmsg) = ($errmsg =~ /^(.*)$/s);
		my $script = "sub { die '$errmsg' };";
		$sub = eval($script);
	    }
	    if ($objfile) {
		# Create object file.  Note: we attempt to handle
		# several cases in which a file already exists and we
		# wish to create a directory, or vice versa.  However,
		# not every case is handled; to be complete, mkpath
		# would have to unlink any existing file in its way.
		my ($dirname) = dirname($objfile);
		if (!-d $dirname) {
		    unlink($dirname) if (-e $dirname);
		    my @newdirs = mkpath($dirname,0,0775);
		    $self->push_files_written(@newdirs);
		}
		rmtree($objfile) if (-d $objfile);
		my $outfh = new IO::File ">$objfile" or die "Interp::load: cannot open '$objfile' for writing\n";
		$self->push_files_written($objfile);
		$outfh->print($objtext) if (!$pureTextFlag);
	    }
	}
	
	#
	# Cache code in memory
	#
	my @info = ($sub,$srcfile);
	if ($self->{code_cache_mode} eq 'all') {
	    $codeCache->{$path}->{lastmod} = $srcfilemod;
	    $codeCache->{$path}->{info} = \@info;
	}
	return @info;
    }
}

#
# Return a reference to the hash at the top of the stack.
#
sub locals {
    my ($self) = @_;
    return $self->{stack}->[0];
}

#
# Return the current number of stack levels. 1 is top level.
#
sub depth {
    my ($self) = @_;
    return scalar(@{$self->{stack}});
}

#
# Return a reference to the entire stack.
#
sub stack {
    my ($self) = @_;
    return $self->{stack};
}

#
# Set or fetch the current time value.
#
sub current_time {
    my $self = shift;
    if (@_) {
	my $newtime = shift;
	die "Interp->current_time: invalid value '$newtime' - must be 'real' or a numeric time value" if $newtime ne 'real' && $newtime !~ /^[0-9]+$/;
	return $self->{current_time} = $newtime;
    } else {
	return $self->{current_time};
    }
}

#
# Get or set a user-defined variable.
#
sub vars
{
    my ($self, $field, $value) = @_;
    if (defined($value)) {
        return $self->{vars}->{$field} = $value;
    } else {
        return $self->{vars}->{$field};
    }
}

sub set_global
{
    my ($self, $decl, @values) = @_;
    my ($prefix, $name) = ($decl =~ /^[\$@%]/) ? (substr($decl,0,1),substr($decl,1)) : ("\$",$decl);

    my $varname = sprintf("%s::%s",$self->{parser}->{in_package},$name);
    if ($prefix eq "\$") {
	no strict 'refs'; $$varname = $values[0];
    } elsif ($prefix eq "\@") {
	no strict 'refs'; @$varname = @values;
    } else {
	no strict 'refs'; %$varname = @values;
    }
}

#
# Allow scalar or hash reference as argument to system_log_events.
#
sub system_log_events
{
    my ($self, $value) = @_;
    if (defined($value)) {
	if (ref($value) eq 'SCALAR') {
	    $value =~ s/\s//g;
	    my %opts = map( ($_, 1), split /\|/, $value);
	    @opts{qw(REQUEST CACHE COMP_LOAD)} = (1,1,1) if $opts{ALL};
	    @opts{qw(CACHE_READ CACHE_WRITE)} = (1,1) if $opts{CACHE};
	    @opts{qw(REQ_START REQ_END)} = (1,1) if $opts{REQUEST};
	    $self->{system_log_events} = \%opts;
	} elsif (ref($value) eq 'HASH') {
	    $self->{system_log_events} = $value;
	} else {
	    die "system_log_events: argument must be a scalar or hash reference";
	}
    }
    return $self->{system_log_events};
}

#
# Determine if the specified event should be logged.
#
sub system_log_event_check
{
    my ($self,$flag) = @_;
    return ($self->{system_log_fh} && $self->system_log_events->{$flag});
}

#
# Allow scalar or code reference as argument to out_method.
#
sub out_method
{
    my ($self, $value) = @_;
    if (defined($value)) {
	if (ref($value) eq 'SCALAR') {
	    $self->{out_method} = sub { $$value .= $_[0] };
	} elsif (ref($value) eq 'CODE') {
	    $self->{out_method} = $value;
	} else {
	    die "out_method: argument must be a scalar or code reference";
	}
    }
    return $self->{out_method};
}

sub files_written
{
    my $self = shift;
    return @{$self->{files_written}};
}

#
# Push onto list of written files.
#
sub push_files_written
{
    my $self = shift;
    my $fref = $self->{'files_written'};
    push(@$fref,@_);
}

#
# Hook functions.
#
sub add_hook {
    my ($self, %args) = @_;
    my @validTypes = qw(start_comp start_primary end_primary end_comp start_file end_file);
    foreach (qw(name type code)) {
	die "add_hook: must specify $_\n" if !exists($args{$_});
    }
    die "add_hook: type must be one of ".join(",",@validTypes)."\n" if !grep($_ eq $args{type},@validTypes);
    die "add_hook: code must be a code reference\n" if ref($args{code}) ne 'CODE';
    $args{order} = 50 if !exists($args{order});
    push(@{$self->{hooks}},{%args});
    $self->index_hooks();
}

sub index_hooks {
    my ($self) = @_;
    my %hookindex;
    foreach my $hook (@{$self->{hooks}}) {
	my $name = $hook->{name};
	my $type = $hook->{type};
	push(@{$hookindex{"$name\cA$type"}},$hook);
	push(@{$hookindex{"*\cA$type"}},$hook);
	push(@{$hookindex{"$name\cA*"}},$hook);
    }
    my @keys = keys(%hookindex);
    foreach my $key (@keys) {
	my @hooklst = sort {$a->{order} <=> $b->{order}} @{$hookindex{$key}};
	$hookindex{$key} = [@hooklst];
    }
    $self->{hook_index} = {%hookindex};
}

sub find_hooks {
    my ($self, %args) = @_;
    my $name = $args{name} || '*';
    my $type = $args{type} || '*';
    my $lref = $self->{hook_index}->{"$name\cA$type"};
    return $lref ? @$lref : ();
}

sub remove_hooks {
    my ($self, %args) = @_;
    my @matchhooks = $self->find_hooks(%args);
    my %remhash;
    foreach (@matchhooks) { $remhash{$_}=1 }
    my @keephooks = grep(!$remhash{$_},@{$self->{hooks}});
    $self->{hooks} = [@keephooks];
    $self->index_hooks();
}

sub suppress_hooks {
    my ($self, %args) = @_;
    my @matchhooks = $self->find_hooks(%args);
    foreach my $hook (@matchhooks) {
	$self->{exec_state}->{suppressed_hooks}->{$hook} = 1;
    }
}

sub unsuppress_hooks {
    my ($self, %args) = @_;
    my @matchhooks = $self->find_hooks(%args);
    foreach my $hook (@matchhooks) {
	delete($self->{exec_state}->{suppressed_hooks}->{$hook});
    }
}

sub call_hooks {
    my ($self, %args) = @_;
    my @matchhooks = $self->find_hooks(%args);
    my @params;
    @params = @{$args{params}} if (defined($args{params}));

    foreach my $hook (@matchhooks) {
	if (!$self->{exec_state}->{suppressed_hooks}->{$hook}) {
	    $hook->{code}->($self,@params);
	}
    }
}

#
# Write a line to the Mason system log.
# Each line begins with the time, pid, and event name.
#
# We only print the line if the log file handle is defined AND the
# event name is in our system_log_events hash.
#
sub write_system_log {
    my $self = shift;

    if ($self->{system_log_fh} && $self->system_log_events->{$_[0]}) {
	my $time = ($HTML::Mason::Config{use_time_hires} ? scalar(Time::HiRes::gettimeofday()) : time);
	$self->{system_log_fh}->print(join ($self->system_log_separator,
					    $time,                  # current time
					    $_[0],                  # event name
					    $$,                     # pid
					    @_[1..$#_]              # event-specific fields
					    ),"\n");
    }
}

sub AUTOLOAD {
    my $self = shift;
    my $type = ref($self) or die "autoload error: bad function $AUTOLOAD";

    my $name = $AUTOLOAD;
    $name =~ s/.*://;   # strip fully-qualified portion
    return if $name eq 'DESTROY';

    die "No such function `$name' in class $type";
}
1;

__END__
