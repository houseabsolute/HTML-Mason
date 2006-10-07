#!/usr/bin/perl -w

use strict;

use File::Spec;
use File::Temp  qw( tempdir );
use Test::More;

use HTML::Mason::Interp;

BEGIN
{
    unless ( eval { require Test::Memory::Cycle;
                    Test::Memory::Cycle->import(); 1 } )
    {
        plan skip_all => 'These tests require Test::Memory::Cycle to run.';
    }
}

plan tests => 7;

{
    my $interp = HTML::Mason::Interp->new( out_method => sub {} );
    memory_cycle_ok( $interp, 'Interp before making a request' );

    my $comp = $interp->make_component( comp_source => 'Comp' );
    $interp->exec( $comp, foo => 1 );
    memory_cycle_ok( $interp, 'Interp after making a request with in-memory comp' );
}

our $Destroyed = 0;
{
    my $dir = tempdir( CLEANUP => 1 );

    make_comp( $dir, 'comp1', <<'EOF' );
This is component 1.

<&| comp2, object => $object &>
content
</&>

<%args>
$object
</%args>
EOF

    make_comp( $dir, 'comp2', <<'EOF' );
This is component 2.
EOF

    my $interp = HTML::Mason::Interp->new( out_method => sub {},
                                           comp_root  => $dir,
                                         );

    $interp->exec( '/comp1', object => Object->new() );
    memory_cycle_ok( $interp, 'Interp after making a request with on-disk comp' );
    is( $Destroyed, 1, 'object passed into request was destroyed' );

    my $req = $interp->make_request( comp => '/comp1', args => [ object => Object->new() ] );
    memory_cycle_ok( $req, 'Request object' );

    undef $req;
    is( $Destroyed, 2, 'object passed into make_request was destroyed' );
}

{
    my $dir = tempdir( CLEANUP => 1 );

    make_comp( $dir, 'comp1', <<'EOF' );
<& /comp2, object => Object->new() &>

Destroyed: <% Object->DestroyCount() %>
EOF

    make_comp( $dir, 'comp2', 'Comp 2' );

    my $output = '';
    my $interp = HTML::Mason::Interp->new( out_method => \$output,
                                           comp_root  => $dir,
                                         );

    $Destroyed = 0;
    $interp->exec('/comp1');

    like( $output, qr/Destroyed: 1/,
          'one object was destroyed in comp1' );
}

sub make_comp
{
    my $dir     = shift;
    my $file    = shift;
    my $content = shift;

    open my $fh, '>', File::Spec->catfile( $dir, $file )
        or die $!;
    print $fh $content
        or die $!;
    close $fh;
}


package Object;

sub new { return bless {}, $_[0] }

sub DESTROY { $Destroyed++ }

sub DestroyCount { $Destroyed }
