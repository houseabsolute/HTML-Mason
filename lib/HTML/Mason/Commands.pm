# Copyright (c) 1998 by Jonathan Swartz. All rights reserved.
# This program is free software; you can redistribute it and/or modify it
# under the same terms as Perl itself.

package HTML::Mason::Commands;

use strict;
use File::Basename;
use HTML::Mason::Utils;
use HTML::Mason::Tools qw(read_file chop_slash);
use HTML::Mason::Config;
use IO;
use Time::Local;

use vars qw($INTERP @ISA @EXPORT_OK @EXPORT);
 
require Exporter;
@ISA=qw(Exporter);
@EXPORT=qw(
	mc_abort
	mc_cache
	mc_cache_self
	mc_caller 
	mc_comp
	mc_comp_exists
	mc_comp_source
	mc_comp_stack
	mc_date 
	mc_file 
	mc_file_root 
	mc_out 
	mc_time
);

@EXPORT_OK=@EXPORT;

#
# Convert relative paths to absolute, handle . and ..
#
my $process_comp_path = sub {
    my ($compPath) = @_;
    if ($compPath !~ m@^/@) {
	$compPath = chop_slash($INTERP->locals->{parentPath}) . "/" . $compPath;
    }
    while ($compPath =~ s@/[^/]+/\.\.@@) {}
    while ($compPath =~ s@/\./@/@) {}
    return $compPath;
};

sub mc_abort
{
    $INTERP->abort(@_);
}

sub mc_cache
{
    my (%options) = @_;
    return undef if !$INTERP->use_data_cache;
    $options{cache_file} = $INTERP->data_cache_filename($INTERP->locals->{truePath});
    if ($options{keep_in_memory}) {
	$options{memory_cache} = $INTERP->{data_cache_store};
	delete($options{keep_in_memory});
    }
    
    $options{action} = $options{action} || 'retrieve';
    $options{key} = $options{key} || 'main';
    my $results = HTML::Mason::Utils::access_data_cache(%options);
    if ($options{action} eq 'retrieve') {
	$INTERP->write_system_log('CACHE_READ',$INTERP->locals->{truePath},$options{key},
				  defined $results ? 1 : 0);
    } elsif ($options{action} eq 'store') {
	$INTERP->write_system_log('CACHE_WRITE',$INTERP->locals->{truePath},$options{key});
    }
    return $results;
}

sub mc_cache_self
{
    my (%options) = @_;
    
    return 0 if !$INTERP->use_data_cache;
    return 0 if $INTERP->locals->{inCacheSelfFlag};
    my (%retrieveOptions,%storeOptions);
    foreach (qw(key expire_if keep_in_memory busy_lock)) {
	if (exists($options{$_})) {
	    $retrieveOptions{$_} = $options{$_};
	}
    }
    foreach (qw(key expire_at expire_next expire_in)) {
	if (exists($options{$_})) {
	    $storeOptions{$_} = $options{$_};
	}
    }
    my $result = mc_cache(action=>'retrieve',%retrieveOptions);
    if (!defined($result)) {
	#
	# Reinvoke the component with inCacheSelfFlag=1 and collect
	# output in $result.
	#
	my $lref = $INTERP->{stack}->[0];
	my %saveLocals = %$lref;
	$lref->{sink} = sub { $result .= $_[0] };
	$lref->{inCacheSelfFlag} = 1;
	my $sub = $lref->{callFunc};
	my %args = %{$lref->{callArgs}};
	&$sub(%args);
	$INTERP->{stack}->[0] = {%saveLocals};
	mc_cache(action=>'store',value=>$result,%storeOptions);
    } else {
	$INTERP->call_hooks('start_primary');
	$INTERP->call_hooks('end_primary');
    }
    mc_out($result);
    return 1;
}

sub mc_caller ()
{
    if ($INTERP->depth <= 1) {
	return undef;
    } else {
	return $INTERP->{stack}->[1]->{truePath};
    }
}

sub mc_call_stack ()
{
    return map($_->{truePath},@{$INTERP->{stack}});
}

sub mc_comp
{
    my ($compPath, %args) = @_;

    $compPath = &$process_comp_path($compPath);
    my ($result,@result);
    if (wantarray) {
	@result = $INTERP->exec_next($compPath, %args);
    } else {
	$result = $INTERP->exec_next($compPath, %args);
    }
    return wantarray ? @result : $result;
}

sub mc_comp_exists
{
    my ($compPath, %args) = @_;

    $compPath = &$process_comp_path($compPath);
    return 1 if ($INTERP->load($compPath));
    if ($args{ALLOW_HANDLERS}) {
	# This hack implements the ALLOW_HANDLERS flag for
	# backward compatibility with Scribe.  Looks for home and
	# dhandler files when component not found.  Hopefully can
	# remove someday soon.
	my $p = $compPath;
	return 1 if $INTERP->load("$p/home");
	while (!$INTERP->load("$p/dhandler") && $p =~ /\S/) {
	    my ($basename,$dirname) = fileparse($p);
	    $p = substr($dirname,0,-1);
	}
	return 1 if $p =~ /\S/;
    }
    return 0;
}

sub mc_comp_source
{
    my ($compPath) = @_;
    
    $compPath = &$process_comp_path($compPath);
    return $INTERP->comp_root.$compPath;
}

sub mc_comp_stack ()
{
    return map($_->{truePath},@{$INTERP->{stack}});
}

#
# Version of DateManip::UnixDate that uses interpreter's notion
# of current time and caches daily results.
#
sub mc_date ($)
{
    my ($format) = @_;

    my $time = $INTERP->current_time();
    if ($format =~ /%[^yYmfbhBUWjdevaAwEDxQF]/ || $time ne 'real' || !defined($INTERP->data_cache_dir)) {
	if ($time eq 'real') {
	    return Date::Manip::UnixDate('now',$format);
	} else {
	    return Date::Manip::UnixDate("epoch $time",$format);
	}
    } else {
	my %cacheOptions = (cache_file=>($INTERP->data_cache_filename('_global')),key=>'mc_date_formats',memory_cache=>($INTERP->{data_cache_store}));
	my $href = HTML::Mason::Utils::access_data_cache(%cacheOptions);
	if (!$href) {
	    my %dateFormats;
	    my @formatChars = qw(y Y m f b h B U W j d e v a A w E D x Q F);
	    my @formatVals = split("\cA",Date::Manip::UnixDate('now',join("\cA",map("%$_",@formatChars))));
	    my $i;
	    for ($i=0; $i<@formatChars; $i++) {
		$dateFormats{$formatChars[$i]} = $formatVals[$i];
	    }
	    $href = {%dateFormats};
	    HTML::Mason::Utils::access_data_cache(%cacheOptions,action=>'store',value=>$href,expire_next=>'day');
	}
	$format =~ s/%(.)/$href->{$1}/g;
	return $format;
    }
}

sub mc_file ($)
{
    my ($file) = @_;
    # filenames beginning with / or a drive letter (e.g. C:/) are absolute
    unless ($file =~ /^([A-Za-z]:)?\//) {
	$file = $INTERP->static_file_root . "/" . $file;
    }
    $INTERP->call_hooks('start_file',$file);
    my $content = read_file($file);
    $INTERP->call_hooks('end_file',$file);
    return $content;
}

sub mc_file_root ()
{
    return $INTERP->static_file_root;
}

sub mc_out ($)
{
    $INTERP->locals->{sink}->($_[0]);
}

sub mc_time
{
    my $time = $INTERP->current_time;
    $time = time() if $time eq 'real';
    return $time;
}

sub mc_var ($)
{
    my ($field) = @_;
    return $INTERP->{vars}->{$field};
}
1;

__END__
