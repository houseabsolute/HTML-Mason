#!/usr/local/bin/perl
#
# This is a Mason system log parser that outputs the average compute
# time of each unique URL, in order from slowest to quickest.
#
use strict;
open(LOG,"system.log");
my (%start,%urls);
while (<LOG>) {
    chomp;
    my (@fields) = split("\cA");
    my ($time,$event,$pid) = splice(@fields,0,3);
    if ($event eq 'REQ_START') {
	my ($reqnum,$url) = @fields;
	$start{"$pid,$reqnum"} = [$time,$url];
    } elsif ($event eq 'REQ_END') {
	my ($reqnum,$errflag) = @fields;
	my $s = $start{"$pid,$reqnum"};
	next if !$s;
	my ($starttime,$url) = @$s;
	my $elapsed = $time - $starttime;
	if (!exists($urls{$url})) {
	    $urls{$url} = {count=>1,t=>$elapsed};
	} else {
	    $urls{$url}->{count}++;
	    $urls{$url}->{t}+=$elapsed;
	}
    }
}
foreach my $val (values(%urls)) {
    $val->{avg} = $val->{t} / $val->{count};
}
my @keys = keys(%urls);
my @sorturls = sort {$urls{$b}->{avg} <=> $urls{$a}->{avg}} @keys;
foreach my $url (@sorturls) {
    printf("%5.3f %s\n",$urls{$url}->{avg},$url);
}

