#!/usr/bin/perl
#
# This is a basic, fairly fuctional Mason handler.pl.
#
# For something a little more involved, check out session_handler.pl

package MyMason::MyApp;

# Bring in main Mason package.
use HTML::Mason;

# Bring in ApacheHandler, necessary for mod_perl integration.
# Uncomment the second line (and comment the first) to use
# Apache::Request instead of CGI.pm to parse arguments.
use HTML::Mason::ApacheHandler;

# Uncomment the next line if you plan to use the Mason previewer.
#use HTML::Mason::Preview;

use strict;

# List of modules that you want to use from components (see Admin
# manual for details)
#{  package HTML::Mason::Commands;
#   use CGI;
#}

# Create ApacheHandler object
#
my $ah = new HTML::Mason::ApacheHandler( comp_root => '<component root>',
                                         data_dir => '<data directory>' );

# Activate the following if running httpd as root (the normal case).
# Resets ownership of all files created by Mason at startup.
#
#chown (Apache->server->uid, Apache->server->gid, $ah->interp->files_written);

sub handler
{
    my ($r) = @_;

    # If you plan to intermix images in the same directory as
    # components, activate the following to prevent Mason from
    # evaluating image files as components.
    #
    #return -1 if $r->content_type && $r->content_type !~ m|^text/|i;
    
    my $status = $ah->handle_request($r);
    
    return $status;
}

1;
