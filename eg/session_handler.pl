#!/usr/bin/perl
#
# This handler used demonstrate how to integrate Apache::Session with
# Mason the hard way.  Now it simply uses the
# MasonX::Request::WithApacheSession module available from CPAN.
#
# This module adds a session method to the Mason Request object ($m
# inside components).  It can also handle setting and reading cookies
# containing the session id.
#

package MyApp::Mason;

# Bring in main Mason package.
use HTML::Mason;
use MasonX::Request::WithApacheSession;

# Bring in ApacheHandler, necessary for mod_perl integration.
# Uncomment the second line (and comment the first) to use
# Apache::Request instead of CGI.pm to parse arguments.
use HTML::Mason::ApacheHandler;

use strict;

# List of modules that you want to use from components (see Admin
# manual for details)
{  package HTML::Mason::Commands;

   # use ...
}


# Create ApacheHandler object
#
my $ah =
    new HTML::Mason::ApacheHandler
        ( request_class => 'MasonX::Request::WithApacheSession',
          session_class => 'Apache::Session::File',
          # Let MasonX::Request::WithApacheSession automatically
          # set and read cookies containing the session id
          session_use_cookie => 1,
          session_directory => '/tmp/sessions',
          session_lock_directory => '/tmp/session-locks',
          comp_root => '<component root>',
          data_dir => '<data directory>' );

sub handler
{
    my ($r) = @_;

    # If you plan to intermix images in the same directory as
    # components, activate the following to prevent Mason from
    # evaluating image files as components.
    #
    #return -1 if $r->content_type && $r->content_type !~ m|^text/|io;

    my $status = $ah->handle_request($r);

    return $status;
}

1;


__END__

In your httpd.conf, add something like this:

 PerlRequire /path/to/handler.pl
 <LocationMatch "\.html$">
   SetHandler perl-script
   PerlHandler MyApp::Mason
 </LocationMatch>
