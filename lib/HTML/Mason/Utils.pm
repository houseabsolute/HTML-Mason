# Copyright (c) 1998 by Jonathan Swartz. All rights reserved.
# This program is free software; you can redistribute it and/or modify it
# under the same terms as Perl itself.

package HTML::Mason::Utils;
require 5.004;
require Exporter;
@ISA = qw(Exporter);
@EXPORT = qw();
@EXPORT_OK = qw(access_data_cache);

use strict;
use IO::File qw(!/^SEEK/);
use POSIX;
use Fcntl;
use File::Basename;
use HTML::Mason::Config;
use HTML::Mason::Tools qw(date_delta_to_secs);

sub access_data_cache
{
    my (%options) = @_;

    #
    # Defaults
    #
    my $cacheFile = $options{cache_file} || die "cache: must specify cache file";
    my $physFile = $cacheFile . $HTML::Mason::DBM_FILE_EXT;
    my $tieClass = ($options{tie_class} || $HTML::Mason::DEFAULT_CACHE_TIE_CLASS || die "no tie class defined!");
    my $action = $options{action} || 'retrieve';
    my $key = $options{key} || 'main';
    my $memCache = $options{memory_cache};
    my $time = time();
    my $path = $cacheFile;

    #
    # Store / expire
    #
    if ($action eq 'store' || $action eq 'expire') {
	my ($expireTime,%out);
	die "no store value provided" if ($action eq 'store' && !exists($options{value}));

	#
	# Validate parameters
	#
	if (my @invalids = grep(!/^(expire_(at|next|in)|action|key|value|memory_cache|cache_file|tie_class)$/,keys(%options))) {
	    die "cache: invalid parameter '$invalids[0]' for action '$action'\n";
	}
	
	#
	# Determine expiration time if expire flag given
	#
	if (exists($options{expire_at})) {
	    $expireTime = $options{expire_at};
	    die "cache: invalid expire_at value '$options{expire_at}' - must be a numeric time value\n" if $expireTime !~ /^[0-9]+$/;
	} elsif (exists($options{expire_next})) {
	    my $term = $options{expire_next};
	    my ($sec,$min,$hour) = localtime(time);
	    my $deltaTime;
	    if ($term eq 'hour') {
		$deltaTime = 60*(60-$min)+(60-$sec);
	    } elsif ($term eq 'day') {
		$deltaTime = 3600*(24-$hour)+60*(60-$min)+(60-$sec);
	    } else {
		die "cache: invalid expire_next value '$term' - must be 'hour' or 'day'\n";
	    }
	    $expireTime = time() + $deltaTime;
	} elsif (exists($options{expire_in})) {
	    my $delta = $options{expire_in};
	    my $deltaTime = eval(date_delta_to_secs($delta));
	    die "cache: invalid expire_in value '$options{expire_in}' - must be of the form <num><unit>, where <unit> is one of second, minute, hour, date, week or an abbreviation thereof\n" if !$deltaTime;
	    $expireTime = time() + $deltaTime;
	}

	#
	# Try to get lock on cache lockfile. If not possible, return.
	#
	my ($base,$lockdir) = fileparse($physFile);
	$lockdir .= "locks";
	mkpath($lockdir,0,0755) if (!-d $lockdir);
	my $lockfile = "$lockdir/$base.lock";
	my $lockfh = new IO::File ">>$lockfile"
	   or die "cache: cannot open lockfile '$lockfile' for locking\n";
	return unless &get_lock($lockfh);
    
	# Tie to DB file
	tie (%out, $tieClass, $cacheFile, O_RDWR|O_CREAT, 0664)
		or die "cache: cannot create/open cache file '$cacheFile'\n";

	if ($action eq 'store') {
	    $out{"$key.contents"} = $options{value};
	    $out{"$key.expires"} = $expireTime;
	    $out{"$key.lastmod"} = $time;
	    if (defined($memCache)) {
		$memCache->{$path}->{$key} = {expires=>$expireTime,lastModified=>$time,lastUpdated=>$time,contents=>$options{value}};
	    }
	} elsif ($action eq 'expire') {
	    delete($out{"$key.contents"});
	    delete($out{"$key.expires"});
	    delete($out{"$key.lastmod"});
	    if (defined($memCache)) {
		delete($memCache->{$path}->{$key});
	    }
	}
	untie(%out);
	$lockfh->close();

    #
    # Retrieve
    #
    } elsif ($action eq 'retrieve') {
	return undef if (!(-e $physFile));
	my $fileLastModified = [stat($physFile)]->[9];
	my $mem;

	#
	# Validate parameters
	#
	if (my @invalids = grep(!/^(expire_if|action|key|memory_cache|cache_file|tie_class)$/,keys(%options))) {
	    die "cache: invalid parameter '$invalids[0]' for action '$action'\n";
	}
	
	if (defined($memCache)) {
	    if (!exists($memCache->{$path}->{$key})) {
		$memCache->{$path}->{$key} = {lastUpdated=>0};
	    }
	    $mem = $memCache->{$path}->{$key};
	} else {
	    $mem = {lastUpdated=>0};
	}

	#
	# If file has been modified since we last updated, then
	# our entry may be modified - check it.
	#
	if ($fileLastModified > $mem->{lastUpdated}) {
	    my %in;
	    tie (%in, $tieClass, $cacheFile, O_RDONLY, 0);

	    #
	    # If entry has been modified since we last updated, read
	    # it into memory.
	    #
	    my $entryLastModified = $in{"$key.lastmod"};
	    if (defined($entryLastModified) and
			$entryLastModified > $mem->{lastUpdated}) {
		$mem->{contents} = $in{"$key.contents"};
		$mem->{expires} = $in{"$key.expires"};
		$mem->{lastModified} = $in{"$key.lastmod"};
		$mem->{lastUpdated} = $time;
	    }
	    untie(%in);
	}

	#
	# If cache entry has expired, return undef. Otherwise return contents.
	#
	return undef if ($mem->{expires} && $time >= $mem->{expires});
	if (exists($options{expire_if})) {
	    my $sub = $options{expire_if};
	    return undef if (&$sub($mem->{lastModified}));
	}
	return $mem->{contents};
    } else {
	die "cache: bad action '$options{action}': must be one of 'store', 'retrieve', or 'expire'\n";
    }
}

#
# Returns 1 if the exclusive, non-blocking lock was obtained,
# undef otherwise.
#
sub get_lock {
	my $fh = shift;

	my $LOCK_EX = 2;
	my $LOCK_NB = 4;
	my $LOCK_UN = 8;
	return flock $fh, $LOCK_EX|$LOCK_NB;
}
