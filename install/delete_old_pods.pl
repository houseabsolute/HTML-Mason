#!/usr/bin/perl -w

use strict;
use File::Spec;

foreach my $dir (@INC) {
    foreach my $pm ( qw( Interp ApacheHandler Request Component ) ) {
	my $pod_file = File::Spec->catfile( $dir, 'HTML', 'Mason', "$pm.pod" );
	
	if ( -e $pod_file ) {
	    warn "Removing obsolete documentation file $pod_file\n";
	    unlink $pod_file or warn "Cannot unlink $pod_file: $!";
	}
    }
}
