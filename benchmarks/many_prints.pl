#!/usr/bin/perl -w

use strict;

use lib '../lib';

use Benchmark;
use Cwd;
use HTML::Mason;

my $interp =
    HTML::Mason::Interp->new( comp_root => File::Spec->rel2abs(cwd),
                              data_dir  => '/tmp/mason-benchmarks',
                            );

Benchmark::timethis( $ARGV[0] || 1000, \&call_comp );

sub call_comp
{
    my $out;
    $interp->out_method(\$out);
    $interp->exec( '/comps/h2000.mas', title => 'foo', integer => int(rand(100)) );

}

__END__

=pod

=head1 many_prints.pl

This benchmarks a component that ends up making a B<lot> of calls to
C<< $m->print >>.

By default, it calls the same component 1000 times.  If you give it an
argument it will call the component this many times instead.

=cut
