#!/usr/bin/perl
#
# This is a basic, fairly fuctional Mason handler.pl.
#
# For something a little more involved, check out session_handler.pl

package HTML::Mason;

#
# Sample Mason handler.
#
use HTML::Mason;
use strict;

# Uncomment the next line if you plan to use the Mason previewer.
#use HTML::Mason::Preview;

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
#chown (scalar(getpwnam "nobody"), scalar(getgrnam "nobody"),
#       $interp->files_written);

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
