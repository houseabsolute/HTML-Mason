#!/usr/bin/perl
#
# This is a handler that supports a %session global, using cookies, that
# persists across HTTP requests.
#
# *NOTE* There is a bug in perl5.005 that can rear it's head when you
#        die() within an eval {}.  Unfortunately, Apache::Session
#        expects you to catch failures by using eval, so you may get
#        bitten.  You can either hack Apache::Session to change how it
#        returns its error conditions, or upgrade to Perl 5.6.0 or
#        greater, which has its own bugs.

#
# A much simpler alternative to this is to simply use the
# HTML::Mason::Request::WithApacheSession code available from CPAN.
#

package MyMason::MyApp;

# Bring in main Mason package.
use HTML::Mason;

# Bring in ApacheHandler, necessary for mod_perl integration.
# Uncomment the second line (and comment the first) to use
# Apache::Request instead of CGI.pm to parse arguments.
use HTML::Mason::ApacheHandler;

use strict;

# List of modules that you want to use from components (see Admin
# manual for details)
{  package HTML::Mason::Commands;
   use vars qw(%session);
   # You might want to replace this with Apache::Cookie if you have
   # libapreq installed.
   use CGI::Cookie;
   # Replace this if you want to use a different storage method.  Also
   # see Apache::Session::Flex in Apache::Session 1.50+ for a way to
   # specify this at run time.
   use Apache::Session::File 1.50;
}


# Create ApacheHandler object
#
my $ah = new HTML::Mason::ApacheHandler( comp_root => '<component root>',
                                         data_dir => '<data directory>' );

sub handler
{
    my ($r) = @_;

    # If you plan to intermix images in the same directory as
    # components, activate the following to prevent Mason from
    # evaluating image files as components.
    #
    #return -1 if $r->content_type && $r->content_type !~ m|^text/|io;

    my %cookies = parse CGI::Cookie($r->header_in('Cookie'));

    # Don't even bother trying to use badly formed session ids.
    my $sid;
    if ( defined $cookies{'AF_SID'} && $cookies{'AF_SID'}->value() =~ /^[a-z\d]+$/ ) {
	$sid = $cookies{'AF_SID'}->value();
    }

    # If $sid is not defined this will generate a new session.
    eval {
	tie %HTML::Mason::Commands::session, 'Apache::Session::File', $sid;
    };

    if ( $@ ) {
	if ( $@ =~ /Object does not exist in the data store/ && defined $sid ) {
	    # Attempt to create a new session if the previous one was
	    # not valid.  This attempt will die (leading to a 500
	    # error) if it fails.  Use eval {} to trap this if you so
	    # desire.
	    tie %HTML::Mason::Commands::session, 'Apache::Session::File', undef;
	} else {
	    # This means that we got a different error or we were
	    # attempting to create a new session from scratch.
	    die $@;
	}
    }

    # Always send the cookie out as there is no reason not to.
    my $cookie = new CGI::Cookie(-name=>'AF_SID', -value=>$HTML::Mason::Commands::session{_session_id}, -path => '/',);
    $r->header_out('Set-Cookie' => $cookie);

    my $status = $ah->handle_request($r);

    untie %HTML::Mason::Commands::session;

    return $status;
}

1;


__END__

In your httpd.conf, add something like this:

 PerlRequire /path/to/handler.pl
 <LocationMatch "\.html$">
   SetHandler perl-script
   PerlHandler MyMason::MyApp
 </LocationMatch>
