#!/usr/bin/perl
package HTML::Mason;

#
# Sample Mason handler. At a minimum, set comp_root and data_dir.
#
use HTML::Mason;
use strict;

# List of modules that you want to use from components (see Admin
# manual for details)
#{  package HTML::Mason::Commands;
#   use CGI;
#}

# Create Mason objects
#
my $parser = new HTML::Mason::Parser;
my $interp = new HTML::Mason::Interp (parser=>$parser,
                                      comp_root=>'<component root>',
                                      data_dir=>'<data directory>');
my $ah = new HTML::Mason::ApacheHandler (interp=>$interp);

# Activate the following if running httpd as root (the normal case).
# Resets ownership of all files created by Mason at startup. Change
# these to match your server's 'User' and 'Group'.
#
#chown ( [getpwnam('nobody')]->[2], [getgrnam('nobody')]->[2],
#        $interp->files_written );

sub handler
{
    my ($r) = @_;

    # If you plan to intermix images in the same directory as
    # components, activate the following to prevent Mason from
    # evaluating image files as components.
    #
    #return -1 if $r->content_type !~ m|^text/|io;
    
    $ah->handle_request($r);
}

1;
