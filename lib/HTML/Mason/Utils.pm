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

    my $lockCacheFile = sub {
	my ($base,$lockdir) = fileparse($physFile);
	$lockdir .= "locks";
	mkpath($lockdir,0,0755) if (!-d $lockdir);
	my $lockfile = "$lockdir/$base.lock";
	my $lockfh = new IO::File ">>$lockfile"
	   or die "cache: cannot open lockfile '$lockfile' for locking\n";
	return (&get_lock($lockfh)) ? $lockfh : undef;
    };
    
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
	# Determine expiration time if expire flag given. For the "next"
	# options, we're jumping through hoops to find the *top* of the
	# next hour or day.
	#
	if (exists($options{expire_at})) {
	    $expireTime = $options{expire_at};
	    die "cache: invalid expire_at value '$options{expire_at}' - must be a numeric time value\n" if $expireTime !~ /^[0-9]+$/;
	} elsif (exists($options{expire_next})) {
            my $term = $options{expire_next};
	    my $lockTime = time();
            my ($sec,$min,$hour) = localtime($lockTime);
	    my $deltaTime;
            if ($term eq 'hour') {
		$deltaTime = 60*(59-$min)+(60-$sec);
            } elsif ($term eq 'day') {
		$deltaTime = 3600*(23-$hour)+60*(59-$min)+(60-$sec);
            } else {
                die "cache: invalid expire_next value '$term' - must be'hour' or 'day'\n";
            }
            $expireTime = $lockTime + $deltaTime;

	} elsif (exists($options{expire_in})) {
	    my $delta = $options{expire_in};
	    my $deltaTime = eval(date_delta_to_secs($delta));
	    die "cache: invalid expire_in value '$options{expire_in}' - must be of the form <num><unit>, where <unit> is one of seconds, minutes, hours, days, weeks, months or an abbreviation thereof\n" if !$deltaTime;
	    $expireTime = time() + $deltaTime;
	}

	#
	# Try to get lock on cache lockfile. If not possible, return.
	#
	my $lockfh = &$lockCacheFile();
	return unless $lockfh;
    
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
	    $out{"$key.expires"} = $time;
	    $out{"$key.lastmod"} = $time;
	    if (defined($memCache)) {
		$memCache->{$path}->{$key}->{expires} = $time;
		$memCache->{$path}->{$key}->{lastModified} = $time;
		$memCache->{$path}->{$key}->{lastUpdated} = $time;
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
	if (my @invalids = grep(!/^(expire_if|action|key|memory_cache|cache_file|busy_lock|tie_class)$/,keys(%options))) {
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
		$mem->{busyLock} = $in{"$key.busylock"} if $options{busy_lock};
		$mem->{lastUpdated} = $time;
	    }
	    untie(%in);
	}

	#
	# If cache entry has expired, return undef. Otherwise return contents.
	#
	my $expired = 0;
	$expired = 1 if ($mem->{expires} && $time >= $mem->{expires});
	if (exists($options{expire_if})) {
	    my $sub = $options{expire_if};
	    $expired = 1 if (&$sub($mem->{lastModified}));
	}
	return $mem->{contents} if !$expired;
	return undef if !$options{busy_lock};

	#
	# Here we implement the busy lock mechanism. This allows one
	# process to recompute a new cache value while the rest of the
	# processes temporarily return the old value.
	#
	my $delay = date_delta_to_secs($options{busy_lock});

	# there are two pieces of information stored about the busy
	# lock: a match number (which must match lastModified for the
	# lock to be valid) and the time at which the lock was
	# created.
	my ($lockMatch,$lastLocked) = defined($mem->{busyLock}) ? split("/",$mem->{busyLock}) : (0,0);
	my $locked = ($lockMatch && $lockMatch==$mem->{lastModified});
	if (!$locked) {
	    # busy lock has not been set yet.  Try to set it by
	    # locking and writing to the cache file.
	    my $lockfh = &$lockCacheFile();
	    if ($lockfh) {
		my %out;
		tie (%out, $tieClass, $cacheFile, O_RDWR|O_CREAT, 0664)
		    or die "cache: cannot create/open cache file '$cacheFile'\n";
		$out{"$key.busylock"} = $mem->{lastModified}."/".$time;
		untie(%out);
		$lockfh->close();

		# busy lock was set successfully.  Return undef so that
		# this process computes the new cache value.
		return undef;
	    }
	    # busy lock could not be set.  Return the old value if
	    # we're close enough to the expire time, otherwise return
	    # undef.
	    return ($time < $mem->{expires}+$delay) ? $mem->{contents} : undef;
	}
	# busy lock is already set.  Return the old value if we're
	# close enough to the locked time, otherwise return undef.
	return ($time < $lastLocked+$delay) ? $mem->{contents} : undef;

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
