package HTML::Mason::Exceptions;

use strict;

use vars qw($VERSION);

$VERSION = sprintf '%2d.%02d', q$Revision$ =~ /(\d+)\.(\d+)/;

my %e;

BEGIN
{
    %e = ( 'HTML::Mason::Exception' =>
	   { description => 'generic base class for all Mason exceptions' },

	   'HTML::Mason::Exception::Aborted' =>
	   { isa => 'HTML::Mason::Exception',
	     description => 'a component called $m->abort' },

	   'HTML::Mason::Exception::Params' =>
	   { isa => 'HTML::Mason::Exception',
	     description => 'invalid parameters were given to a method/function' },

	   'HTML::Mason::Exception::Syntax' =>
	   { isa => 'HTML::Mason::Exception',
	     description => 'invalid syntax was found in a component' },

	 );
}

use Exception::Class (%e);

if ( HTML::Mason::DEBUG )
{
    Exception::Class::Base->do_trace(1);
    foreach my $class (keys %e)
    {
	$class->do_trace(1);
    }
}


1;
