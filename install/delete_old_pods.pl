#!/usr/bin/perl -w

use strict;

use File::Find;
use File::Spec;

my $install_dir = shift
    or die "No install directory provided";

find( \&del_old_pods, File::Spec->catdir( $install_dir, 'HTML', 'Mason' ) );

sub del_old_pods
{
    return 1 unless $_ =~ /\.pm$/;

    (my $pod_file = $File::Find::name) =~ s/\.pm$/\.pod/;

    if ( -e $pod_file )
    {
	unlink $pod_file or die "Cannot remove $pod_file: $!"
    }
}

