#!/usr/local/bin/perl
#
# This is a code skeleton for parsing the various events in a Mason
# system log.
#
open(LOG,"system.log");
while (<LOG>) {
    chomp;
    my (@fields) = split("\cA");
    my ($time,$event,$pid) = splice(@fields,0,3);
    if ($event eq 'REQ_START') {
	my ($reqnum,$url) = @fields;
	...
    } elsif ($event eq 'REQ_END') {
	my ($reqnum,$errflag) = @fields;
	...
    } elsif ($event eq 'CACHE_READ') {	
	my ($comp,$key,$hitflag) = @fields;
	...
    } elsif ($event eq 'CACHE_STORE') {	
	my ($comp,$key) = @fields;
	...
    } elsif ($event eq 'COMP_LOAD') {
	my ($comp) = @fields;
	...
    } else {
	warn "unrecognized event type: $event\n";
    }
}
