# Copyright (c) 1998-99 by Jonathan Swartz. All rights reserved.
# This program is free software; you can redistribute it and/or modify it
# under the same terms as Perl itself.

package HTML::Mason::Interp;
require 5.004;

use strict;
use Carp;
use Data::Dumper;
use File::Path;
use File::Basename;
use File::Find;
use IO::File;
use IO::Seekable;
use HTML::Mason::Parser;
use HTML::Mason::Tools qw(read_file pkg_loaded is_absolute_path);
use HTML::Mason::Commands qw();
use HTML::Mason::Config;
use HTML::Mason::Resolver::File;
require Time::HiRes if $HTML::Mason::Config{use_time_hires};

use HTML::Mason::MethodMaker
    ( read_only => [ qw( code_cache
			 comp_root
			 data_dir
			 hooks
			 system_log_file
			 system_log_separator
			 preloads ) ],

      read_write => [ qw( allow_recursive_autohandlers
			  autohandler_name
			  code_cache_max_size
			  data_cache_dir
			  dhandler_name
			  max_recurse
			  out_mode
			  parser
			  resolver
			  static_file_root
			  use_data_cache
			  use_object_files
			  use_reload_file
			  verbose_compile_error ) ],
      );

# Fields that can be set in new method, with defaults
my %fields =
    (
     allow_recursive_autohandlers => 1,
     autohandler_name => 'autohandler',
     code_cache_max_size => 10*1024*1024,
     comp_root => undef,
     current_time => 'real',
     data_cache_dir => '',
     data_dir => undef,
     dhandler_name => 'dhandler',
     system_log_file => undef,
     system_log_separator => "\cA",
     max_recurse => 32,
     out_mode => 'batch',
     parser => undef,
     preloads => [],
     resolver => undef,     
     static_file_root => undef,
     use_data_cache => 1,
     use_object_files => 1,
     use_reload_file => 0,
     verbose_compile_error => 0
     );

sub new
{
    my $class = shift;
    my $self = {
	%fields,
	data_cache_store => {},
        code_cache => {},
        code_cache_current_size => 0,
	files_written => [],
	hooks => {},
	last_reload_time => 0,
	last_reload_file_pos => 0,
	out_method => sub { print $_[0] if defined($_[0]) },
	system_log_fh => undef,
	system_log_events_hash => undef
    };
    my (%options) = @_;
    my ($rootDir,$outMethod,$systemLogEvents);
    while (my ($key,$value) = each(%options)) {
	next if $key =~ /out_method|system_log_events/;
	if (exists($fields{$key})) {
	    $self->{$key} = $value;
	} else {
	    die "HTML::Mason::Interp::new: invalid option '$key'\n";
	}
    }
    die "HTML::Mason::Interp::new: must specify value for data_dir\n" if !$self->{data_dir};
    $self->{data_cache_dir} ||= ($self->{data_dir} . "/cache");
    bless $self, $class;
    $self->out_method($options{out_method}) if (exists($options{out_method}));
    $self->system_log_events($options{system_log_events}) if (exists($options{system_log_events}));
    $self->_initialize;
    return $self;
}

sub _initialize
{
    my ($self) = shift;
    $self->{code_cache} = {};
    $self->{code_cache_current_size} = 0;
    $self->{data_cache_store} = {};

    #
    # Create parser if not provided
    #
    unless ($self->{parser}) {
	my $p = new HTML::Mason::Parser;
	$self->{parser} = $p;
    }

    #
    # Create resolver if not provided
    #
    unless ($self->{resolver}) {
	die "must specify value for comp_root\n" if !$self->{comp_root};
	my $r = new HTML::Mason::Resolver::File;
	$self->{resolver} = $r;
    }

    #
    # Remove unnecessary terminating slashes from directories, and check
    # that directories are absolute.
    #
    foreach my $field (qw(comp_root data_dir data_cache_dir)) {
	next if $field eq 'comp_root' and ref($self->{$field}) eq 'ARRAY';
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
    # If comp_root has multiple dirs, confirm format.
    #
    if (ref($self->comp_root) eq 'ARRAY') {
	foreach my $pair (@{$self->comp_root}) {
	    die "Multiple-path component root must consist of a list of two-element lists; see documentation" if ref($pair) ne 'ARRAY';
	    $pair->[1] =~ s/\/$//g unless $pair->[1] =~ /^([A-Za-z]:)?\/$/;
	    die "comp_root must contain only absolute directories" if !is_absolute_path($pair->[1]);
	}
    }
    
    #
    # Open system log file
    #
    if ($self->{system_log_events_hash}) {
	$self->{system_log_file} = $self->data_dir . "/etc/system.log" if !$self->system_log_file;
	my $fh = new IO::File ">>".$self->system_log_file
	    or die "Couldn't open system log file ".$self->{system_log_file}." for append";
	$fh->autoflush(1);
	$self->{system_log_fh} = $fh;
    }
    
    #
    # Preloads
    #
    if ($self->preloads) {
	foreach my $pattern (@{$self->preloads}) {
	    my @paths = $self->resolver->glob_path($pattern,$self);
	    foreach (@paths) { $self->load($_) }
	}
    }

    #
    # Adjust to current size of reload file
    #
    if ($self->use_reload_file && -f $self->reload_file) {
	$self->{last_reload_file_pos} = (stat(_))[7];
	$self->{last_reload_time} = (stat(_))[9];
    }
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
    my $self = shift;
    my $req = new HTML::Mason::Request (interp=>$self);
    $req->exec(@_);
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
	    if (exists($self->{code_cache}->{$compPath})) {
		$self->{code_cache_current_size} -= $self->{code_cache}->{$compPath}->{size};
		delete($self->{code_cache}->{$compPath});
	    }
	}
    }
}

#
# Return the absolute version of a component path. Handles . and ..
# Second argument is directory path to resolve relative paths against.
#
sub process_comp_path
{
    my ($self,$comp_path,$dir_path) = @_;
    if ($comp_path !~ m@^/@) {
	$comp_path = $dir_path . ($dir_path eq "/" ? "" : "/") . $comp_path;
    }
    while ($comp_path =~ s@/[^/]+/\.\.@@) {}
    while ($comp_path =~ s@/\./@/@) {}
    return $comp_path;    
}

#
# Look up <$path> as a component path. Return fully qualified path or
# or undef if it does not exist.
# 
sub lookup {
    my ($self,$path) = @_;
    my (@lookup_info) = $self->resolver->lookup_path($path,$self);
    return $lookup_info[0];
}

#
# Load <$path> into a component, possibly parsing the source and/or
# caching the code. Returns a component object or undef if the
# component was not found.
#
sub load {
    my ($self,$path) = @_;
    my ($err,$maxfilemod,$objfile,$objfilemod);
    my (@objstat, $objisfile);
    my $code_cache = $self->{code_cache};
    my $resolver = $self->{resolver};

    #
    # Use resolver to look up component and get fully-qualified path.
    # Return undef if component not found.
    #
    my (@lookup_info) = $resolver->lookup_path($path,$self);
    my $fq_path = $lookup_info[0] or return undef;

    #
    # If using reload file, assume that we are using object files and
    # have a cached subroutine or object file.
    #
    if ($self->{use_reload_file}) {
	return $code_cache->{$fq_path}->{comp} if exists($code_cache->{$fq_path});

	$objfile = $self->object_dir . $fq_path;
	return undef unless (-f $objfile);   # component not found
	
	$self->write_system_log('COMP_LOAD', $fq_path);	# log the load event
	my $comp = $self->{parser}->eval_object_text(object_file=>$objfile, error=>\$err)
	    or die "Error while loading '$objfile' at runtime:\n$err\n";
	$comp->assign_runtime_properties($self,$fq_path);
	
	$code_cache->{$fq_path}->{comp} = $comp;
	return $comp;
    }

    #
    # Get last modified time of source.
    #
    my $srcmod = $resolver->get_last_modified(@lookup_info);
    
    if ($self->{use_object_files}) {
	$objfile = $self->object_dir . $fq_path;
	@objstat = stat $objfile;
	$objisfile = -f _;
    }
    
    #
    # If code cache contains an up to date entry for this path,
    # use the cached sub.
    #
    if (exists($code_cache->{$fq_path}) and $code_cache->{$fq_path}->{lastmod} >= $srcmod) {
	return $code_cache->{$fq_path}->{comp};
    } else {
	$objfilemod = (defined($objfile) and $objisfile) ? $objstat[9] : 0;
	
	#
	# Load the component from source or object file.
	#
	$self->write_system_log('COMP_LOAD', $fq_path);	# log the load event

	my $comp;
	my $parser = $self->{parser};
	if ($objfile) {
	    #
	    # We are using object files.  Update object file if necessary
	    # and load component from there.
	    #
	    update_object:
	    if ($objfilemod < $srcmod) {
		my @newfiles;
		my @params = $resolver->get_source_params(@lookup_info);
		my $objText = $parser->parse_component(@params,error=>\$err)
		    or die sprintf("Error during compilation of %s:\n%s\n",$resolver->get_source_description(@lookup_info),$err);
		$parser->write_object_file(object_text=>$objText, object_file=>$objfile);
	    }
	    $comp = $parser->eval_object_text(object_file=>$objfile, error=>\$err);
	    if (!$comp) {
		# If this is an earlier version object file, replace it.
		if ($err =~ /object file was created by a pre-0\.7 parser/
		    or (($err =~ /object file was created by.*version ([\d\.]+) .* you are running parser version ([\d\.]+)\./) and $1 < $2)) {
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
	    my @params = $resolver->get_source_params(@lookup_info);
	    $comp = $self->parser->make_component(@params,error=>\$err)
		or die sprintf("Error during compilation of %s:\n%s\n",$resolver->get_source_description(@lookup_info),$err);
	}
	$comp->assign_runtime_properties($self,$fq_path);

	#
	# Cache code in memory, adjusting current cache size appropriately. Don't cache
	# elements that are too large.
	#
	$self->{code_cache_current_size} -= $code_cache->{$fq_path}->{comp}->object_size if (exists($code_cache->{$fq_path}));
	if ($comp->object_size <= $self->code_cache_max_elem) {
	    $code_cache->{$fq_path} = {lastmod=>$srcmod, comp=>$comp};
	    $self->{code_cache_current_size} += $comp->object_size;
	} else {
	    delete($code_cache->{$fq_path}) if (exists($code_cache->{$fq_path}));
	}
	return $comp;
    }
}

#
# If code cache has exceeded maximum, remove least frequently used
# elements from cache until size falls below minimum.
#
sub purge_code_cache {
    my ($self) = @_;

    if ($self->{code_cache_current_size} > $self->code_cache_max_size) {
	my $code_cache = $self->{code_cache};
	my $cur_size = $self->{code_cache_current_size};
	my $min_size = $self->code_cache_min_size;
	my $decay_factor = $self->code_cache_decay_factor;

	my @elems;
	while (my ($path,$href) = each(%{$code_cache})) {
	    push(@elems,[$path,$href->{comp}->mfu_count,$href->{comp}]);
	}
	@elems = sort { $a->[1] <=> $b->[1] } @elems;
	while (($cur_size > $min_size) and @elems) {
	    my $elem = shift(@elems);
	    $cur_size -= $elem->[2]->object_size;
	    delete($code_cache->{$elem->[0]});
	}
	$self->{code_cache_current_size} = $cur_size;

	#
	# Multiple each remaining cache item's count by a decay factor,
	# to gradually reduce impact of old information.
	#
	foreach my $elem (@elems) {
	    $elem->[2]->{mfu_count} *= $decay_factor;
	}
    }
}

#
# Make an anonymous component and assign it to this interpreter.
#
sub make_component {
    my $self = shift;
    my $comp = $self->parser->make_component(@_);
    $comp->assign_runtime_properties($self) if $comp;
    return $comp;
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
	    confess "out_method: argument must be a scalar or code reference";
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
    $p =~ s/\/$//;
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
my @hookTypes = qw(start_comp end_comp start_file end_file);
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

# Code cache parameter methods

sub code_cache_min_size { shift->code_cache_max_size * 0.75 }
sub code_cache_max_elem { shift->code_cache_max_size * 0.20 }
sub code_cache_decay_factor { 0.75 }

1;
