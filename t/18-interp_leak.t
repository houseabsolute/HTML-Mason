use strict;

use Test;

BEGIN
{
    if ($] < 5.006)
    {
        print "1..0\n";
        exit;
    }
    else
    {
        require Scalar::Util;
        unless ( defined &Scalar::Util::weaken )
        {
            print "Your installation of Scalar::Util does not include the weaken subroutine\n";
            print "1..0\n";
        }
    }

    plan tests => 2;
}

use HTML::Mason::Interp;

{
    package InterpWatcher;
    my $destroy_count = 0;
    
    use base qw(HTML::Mason::Interp);
    sub DESTROY { $destroy_count++ }
    sub count   { $destroy_count   }
}


my $comp;
{
    my $interp = InterpWatcher->new();
    $comp = $interp->make_component( comp_source => 'foo' );
}
ok( InterpWatcher->count, 1 );

{
    my $interp = InterpWatcher->new();
    $comp = $interp->make_component( comp_source => 'foo' );
}
ok( InterpWatcher->count, 2 );
