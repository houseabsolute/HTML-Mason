#!/usr/bin/perl
#
# A basic, fuctional Mason handler.pl.
#
package MyMason::MyApp;

# Bring in Mason with Apache support.
use HTML::Mason::ApacheHandler;
use strict;

# List of modules that you want to use within components.
{ package HTML::Mason::Commands;
  use Data::Dumper;
}

# Create ApacheHandler object at startup.
my $ah = new HTML::Mason::ApacheHandler();

sub handler
{
    my ($r) = @_;

    my $status = $ah->handle_request($r);
    return $status;
}

1;


__END__

In your httpd.conf, add something like this:

 PerlRequire /path/to/handler.pl
 <FilesMatch "\.html$">
   SetHandler perl-script
   PerlHandler MyMason::MyApp
 </FilesMatch>
