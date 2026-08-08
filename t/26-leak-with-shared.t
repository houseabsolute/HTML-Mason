use strict;
use warnings;

use HTML::Mason::Tools qw(can_weaken);
BEGIN
{
    unless ( can_weaken )
    {
        print "Your installation does not include Scalar::Util::weaken\n";
        print "1..0\n";
        exit;
    }
}

use Test::More;

use HTML::Mason::Interp;

plan tests => 2;

our $Destroyed = 0;

SIMPLE_OBJECTS:
{
    $Destroyed = 0;
    my $interp = HTML::Mason::Interp->new( out_method => sub {} );
    my $comp = $interp->make_component( comp_source => 'Comp' );
    $interp->exec( $comp, Object->new() );
    is( $Destroyed, 1, 'object passed into request was destroyed' );
}

SIMPLE_OBJECTS_WITH_SHARED:
{
    $Destroyed = 0;
    my $interp = HTML::Mason::Interp->new( out_method => sub {} );
    my $comp = $interp->make_component( comp_source => 'Comp<%shared></%shared>' );
    $interp->exec( $comp, Object->new() );
    is( $Destroyed, 1, 'object passed into request was destroyed with shared' );
}

package Object;

sub new { return bless {}, $_[0] }

sub DESTROY { $Destroyed++ }

sub DestroyCount { $Destroyed }
