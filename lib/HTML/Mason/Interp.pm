# Copyright (c) 1998-99 by Jonathan Swartz. All rights reserved.
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
use HTML::Mason::Tools qw(read_file pkg_loaded is_absolute_path);
use HTML::Mason::Commands qw();
use HTML::Mason::Config;
require Time::HiRes if $HTML::Mason::Config{use_time_hires};

my %fields =
    (alternate_sources => undef,
     allow_recursive_autohandlers => 0,
     autohandler_name => 'autohandler',
     comp_root => undef,
     code_cache_mode => 'all',
     current_time => 'real',
     data_cache_dir => '',
     data_dir => undef,
     dhandler_name => 'dhandler',
     system_log_file => undef,
     system_log_separator => "\cA",
     max_recurse => 16,
     parser => undef,
     preloads => [],
     static_file_root => undef,
     use_data_cache => 1,
     use_object_files => 1,
     use_reload_file => 0,
     verbose_compile_error => 0
     );
# Create accessor routines
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
	hooks => {},
	last_reload_time => 0,
	last_reload_file_pos => 0,
	out_method => sub { print $_[0] },
	system_log_fh => undef,
	system_log_events_hash => undef
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
    die "HTML::Mason::Interp::new: must specify value for comp_root\n" if !$self->{comp_root};
    die "HTML::Mason::Interp::new: must specify value for data_dir\n" if !$self->{data_dir};
    $self->{data_cache_dir} ||= ($self->{data_dir} . "/cache");
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
    if (!$self->{parser}) {
	my $p = new HTML::Mason::Parser;
	$self->parser($p);
    }

    #
    # Remove unnecessary terminating slashes from directories, and check
    # that directories are absolute.
    #
    foreach my $field (qw(comp_root data_dir data_cache_dir)) {
	$self->{$field} =~ s/\/$//g unless $self->{$field} =~ /^([A-Za-z]:)?\/$/;
 	die "$field ('".$self->{$field}."') must be an absolute directory" if !is_absolute_path($self->{$field});
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
    if ($self->{system_log_events_hash}) {
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
		    if (-f $File::Find::name) {
			my $file = $File::Find::name;
			my $compPath = substr($file,$slen);
			$self->load($compPath);
		    }
		};
		find($sub,$fullPath);
	    } elsif (-f $fullPath) {
		$self->load($p);
	    }
	}
	$self->{code_cache_mode} = $savemode;	
    }

    #
    # Adjust to current size of reload file
    #
    if ($self->use_reload_file && -f $self->reload_file) {
	$self->{last_reload_file_pos} = (stat(_))[7];
	$self->{last_reload_time} = (stat(_))[9];
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

#
# exec is the initial entry point for executing a component
# in a new request.
#
sub exec {
    my ($self, $comp, %args) = @_;

    # Check if reload file has changed.
    $self->check_reload_file if ($self->{use_reload_file});
    
    # Create a new request, unless one has been passed in REQ option.
    my $req;
    if ($req = $args{REQ}) {
	delete($args{REQ});
    } else {
	$req = new HTML::Mason::Request (interp=>$self);
    }

    # $comp can be an absolute path or component object.  If a path,
    # load into object.
    if (!ref($comp) && substr($comp,0,1) eq '/') {
	my $path = $comp;
	if (!($comp = $self->load($path))) {
	    if (defined($self->{dhandler_name}) and $comp = $self->find_comp_upwards($path,$self->{dhandler_name})) {
		my $parent = $comp->dir_path;
		($req->{dhandler_arg} = $path) =~ s{^$parent/}{};
	    }
	}
	die "could not find component for path '$path'\n" if !$comp;
    } elsif (ref($comp) !~ /Component/) {
	die "exec: first argument ($comp) must be an absolute component path or a component object";
    }

    # Check for autohandler.
    if (defined($self->{autohandler_name})) {
	my $parent = $comp->dir_path;
	my $autocomp;
	if (!$self->{allow_recursive_autohandlers}) {
	    $autocomp = $self->load("$parent/".$self->{autohandler_name});
	} else {
	    $autocomp = $self->find_comp_upwards($parent,$self->{autohandler_name});
	}
	if (defined($autocomp)) {
	    $req->{autohandler_next} = [$comp,\%args];
	    $comp = $autocomp;
	}
    }

    # Call the first component.
    my ($result, @result);
    if (wantarray) {
	local $SIG{'__DIE__'} = sub { confess($_[0]) };
	@result = eval {$req->call($comp, %args)};
    } else {
	local $SIG{'__DIE__'} = sub { confess($_[0]) };
	$result = eval {$req->call($comp, %args)};
    }

    # If an error occurred...
    my $err = $@;
    if ($err) {
	return $req->{abort_retval} if ($req->{abort_flag});
	my $i = index($err,'HTML::Mason::Interp::exec');
	$err = substr($err,0,$i) if $i!=-1;
	$err =~ s/^\s*(HTML::Mason::Commands::__ANON__|HTML::Mason::Request::call).*\n//gm;
	if (@{$req->{stack}}) {
	    my $errmsg = "error while executing ".$req->comp->title.":\n";
	    $errmsg .= $err."\n";
	    if ($req->depth > 1) {
		$errmsg .= "backtrace: " . join(" <= ",map($_->{comp}->title,@{$req->{stack}}))."\n";
	    }
	    die ($errmsg);
	} else {
	    die ($err);
	}
    }

    return wantarray ? @result : $result;
}

#
# Check if reload file has changed. If so, read paths from last read
# position to end of file and delete those paths from the cache.
#
sub check_reload_file {
    my ($self) = @_;
    my $reloadFile = $self->reload_file;
    return if (!-f $reloadFile);
    my $lastmod = (stat(_))[9];
    if ($lastmod > $self->{last_reload_time}) {
	my ($block);
	my $length = (stat(_))[7];
	$self->{last_reload_file_pos} = 0 if ($length < $self->{last_reload_file_pos});
	my $fh = new IO::File $reloadFile;
	return if !$fh;
	my $pos = $self->{last_reload_file_pos};
	$fh->seek($pos,&SEEK_SET);
	read($fh,$block,$length-$pos);
	$self->{last_reload_time} = $lastmod;
	$self->{last_reload_file_pos} = $fh->tell;
	my @lines = split("\n",$block);
	foreach my $compPath (@lines) {
	    delete($self->{code_cache}->{$compPath});
	}
    }
}

#
# Load <$path> into a component, possibly parsing the source and/or
# caching the code. Returns a component object or undef if the
# component was not found.
#
sub load {
    my ($self,$path) = @_;
    my ($err,$maxfilemod,$srcfile,$objfile,$objfilemod,$srcfilemod);
    my (@srcstat, @objstat, $objisfile);
    my $codeCache = $self->{code_cache};
    my $compRoot = $self->{comp_root};
    $srcfile = $compRoot . $path;

    #
    # If using reload file, assume that we are using object files and
    # have a cached subroutine or object file.
    #
    if ($self->{use_reload_file}) {
	return $codeCache->{$path}->{comp} if exists($codeCache->{$path});

	$objfile = $self->object_dir . substr($srcfile,length($compRoot));
	return undef unless (-f $objfile);   # component not found
	
	$self->write_system_log('COMP_LOAD', $path);	# log the load event
	my $comp = $self->{parser}->eval_object_text(object_file=>$objfile, error=>\$err)
	    or die "Error while loading '$objfile' at runtime:\n$err\n";
	$comp->assign_file_properties($self->comp_root,$self->data_dir,$self->data_cache_dir,$path);
	
	if ($self->{code_cache_mode} eq 'all') {
	    $codeCache->{$path}->{comp} = $comp;
	}
	return $comp;
    }
    
    #
    # Determine source and (possibly) object filename.
    # If alternate sources are defined, check those first.
    #
    if (defined($self->{alternate_sources})) {
	my @alts = $self->{alternate_sources}->('comp',$path);
	foreach my $alt (@alts) {
	    @srcstat = stat ($compRoot . $alt);
	    if (-f _) {
		$srcfile = $compRoot . $alt;
		last;
	    }
	}
    }
    @srcstat = stat $srcfile unless @srcstat;
    return () unless (-f _);
    $srcfilemod = $srcstat[9];
    
    if ($self->{use_object_files}) {
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
	$codeCache->{$path}->{comp}->source_file eq $srcfile) {
	return $codeCache->{$path}->{comp};
    } else {
	$objfilemod = (defined($objfile) and $objisfile) ? $objstat[9] : 0;
	
	#
	# Load the component from source or object file.
	#
	$self->write_system_log('COMP_LOAD', $path);	# log the load event

	my $comp;
	my $parser = $self->{parser};
	if ($objfile) {
	    #
	    # We are using object files.  Update object file if necessary
	    # and load component from there.
	    #
	    update_object:
	    if ($objfilemod < $srcfilemod) {
		my @newfiles;
		my $objText = $parser->parse_component(script_file=>$srcfile,error=>\$err) or die "Error during compilation of $srcfile:\n$err\n";
		$parser->write_object_file(object_text=>$objText, object_file=>$objfile, files_written=>\@newfiles);
		$self->push_files_written(@newfiles);
	    }
	    $comp = $parser->eval_object_text(object_file=>$objfile, error=>\$err);
	    if (!$comp) {
		# If this is a pre-0.7 object file, replace it.
		if ($err =~ /object file was created by a pre-0\.7 parser/) {
		    $objfilemod = 0;
		    goto update_object;
		} else {
		    die "Error while loading '$objfile' at runtime:\n$err\n";
		}
	    }
	} else {
	    #
	    # No object files. Load component directly into memory.
	    #
	    $comp = $parser->make_component(script_file=>$srcfile,error=>\$err) or die "Error during compilation of $srcfile:\n$err\n";
	}
	$comp->assign_file_properties($self->comp_root,$self->data_dir,$self->data_cache_dir,$path);
	
	#
	# Cache code in memory
	#
	if ($self->{code_cache_mode} eq 'all') {
	    $codeCache->{$path}->{lastmod} = $srcfilemod;
	    $codeCache->{$path}->{comp} = $comp;
	}
	return $comp;
    }
}

#
# Set or fetch the current time value.
#
sub current_time {
    my $self = shift;
    if (@_) {
	my $newtime = shift;
	die "Interp::current_time: invalid value '$newtime' - must be 'real' or a numeric time value" if $newtime ne 'real' && $newtime !~ /^[0-9]+$/;
	return $self->{current_time} = $newtime;
    } else {
	return $self->{current_time};
    }
}

sub set_global
{
    my ($self, $decl, @values) = @_;
    die "Interp::set_global: expects a variable name and one or more values" if !@values;
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
	if (!ref($value)) {
	    $value =~ s/\s//g;
	    my %opts = map( ($_, 1), split /\|/, $value);
	    @opts{qw(REQUEST CACHE COMP_LOAD)} = (1,1,1) if $opts{ALL};
	    @opts{qw(CACHE_READ CACHE_WRITE)} = (1,1) if $opts{CACHE};
	    @opts{qw(REQ_START REQ_END)} = (1,1) if $opts{REQUEST};
	    $self->{system_log_events_hash} = \%opts;
	} elsif (ref($value) eq 'HASH') {
	    $self->{system_log_events_hash} = $value;
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
    return ($self->{system_log_fh} && $self->{system_log_events_hash}->{$flag});
}

#
# Allow scalar or code reference as argument to out_method.
#
sub out_method
{
    my ($self, $value) = @_;
    if (defined($value)) {
	if (ref($value) eq 'SCALAR') {
	    $self->{out_method} = sub { $$value .= $_[0] if defined($_[0]) };
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
# Look for component <$name> starting in <$startpath> and moving upwards
# to the root. Return component object or undef.
#
sub find_comp_upwards
{
    my ($self,$startpath,$name) = @_;

    my $comp;
    my $p = $startpath;
    while (!($comp = $self->load("$p/$name")) && $p) {
	my ($basename,$dirname) = fileparse($p);
	$dirname =~ s/^\.//;    # certain versions leave ./ in $dirname
	$p = substr($dirname,0,-1);
    }
    return $comp;
}

#
# Hook functions.
#
my @hookTypes = qw(start_comp start_primary end_primary end_comp start_file end_file);
my %hookTypeMap = map(($_,1),@hookTypes);

sub add_hook {
    my ($self, %args) = @_;
    foreach (qw(name type code)) {
	die "add_hook: must specify $_\n" if !exists($args{$_});
    }
    die "add_hook: type must be one of ".join(",",@hookTypes)."\n" if !$hookTypeMap{$args{type}};
    die "add_hook: code must be a code reference\n" if ref($args{code}) ne 'CODE';
    $self->{hooks}->{$args{type}}->{$args{name}} = $args{code};
}

sub remove_hook {
    my ($self, %args) = @_;
    foreach (qw(name type)) {
	die "remove_hook: must specify $_\n" if !exists($args{$_});
    }
    delete($self->{hooks}->{$args{type}}->{$args{name}});
}

#
# Write a line to the Mason system log.
# Each line begins with the time, pid, and event name.
#
# We only print the line if the log file handle is defined AND the
# event name is in system_log_events_hash.
#
sub write_system_log {
    my $self = shift;

    if ($self->{system_log_fh} && $self->{system_log_events_hash}->{$_[0]}) {
	my $time = ($HTML::Mason::Config{use_time_hires} ? scalar(Time::HiRes::gettimeofday()) : time);
	$self->{system_log_fh}->print(join ($self->system_log_separator,
					    $time,                  # current time
					    $_[0],                  # event name
					    $$,                     # pid
					    @_[1..$#_]              # event-specific fields
					    ),"\n");
    }
}

1;
