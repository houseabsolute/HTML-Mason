#!/usr/bin/perl
#
# This is a handler that supports a %session global, using cookies, that
# persists across HTTP requests.
#
# *NOTE* There is a bug in perl5.005 that can rear it's head when you die()
#        within an eval {}.  Unfortunately, Apache::Session expects you to
#        catch failures by using eval, so you may get bitten.  You can
#        either hack Apache::Session to change how it returns it's error
#        conditions, or upgrade to the latest perl development version (or
#        wait for the release of 5.6)


package HTML::Mason;

use HTML::Mason;
use strict;

# List of modules that you want to use from components (see Admin
# manual for details)
{  package HTML::Mason::Commands;
   use vars qw(%session);
   use CGI::Cookie;
   use Apache::Session::File;
}

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
    #return -1 if $r->content_type && $r->content_type !~ m|^text/|io;

    my %cookies = parse CGI::Cookie($r->header_in('Cookie'));

    eval { 
      tie %HTML::Mason::Commands::session, 'Apache::Session::File',
        ( $cookies{'AF_SID'} ? $cookies{'AF_SID'}->value() : undef );
    };

    if ( $@ ) {
      # If the session is invalid, create a new session.
      if ( $@ =~ m#^Object does not exist in the data store# ) {
        tie %HTML::Mason::Commands::session, 'Apache::Session::File', undef;
        undef $cookies{'AF_SID'};
      }
    }
       
    if ( !$cookies{'AF_SID'} ) {
      my $cookie = new CGI::Cookie(-name=>'AF_SID', -value=>$HTML::Mason::Commands::session{_session_id}, -path => '/',);
      $r->header_out('Set-Cookie', => $cookie);
    }

    my $status = $ah->handle_request($r);

    untie %HTML::Mason::Commands::session;
  
    return $status;
}

1;
