#!/usr/bin/perl -w

use strict;

use lib '../lib';

use Benchmark;
use Cwd;
use HTML::Mason;

use Getopt::Long;

my %opts = ( reps => 1000 );
GetOptions( 'test:s'  => \@{ $opts{test} },
            'profile' => \$opts{profile},
            'reps:i'  => \$opts{reps},
          );

unless ( @{ $opts{test} } )
{
    usage();
    exit;
}

my %tests =
    ( print =>
      sub { call_comp( '/comps/print.mas', title => 'print', integer => 1000 ) },

      comp =>
      sub { call_comp( '/comps/comp.mas' ) },
    );

foreach my $test ( @{ $opts{test} } )
{
    unless ( exists $tests{$test} )
    {
        print "\n*** Invalid test: $test\n";
        usage();
        exit;
    }
}

my $interp =
    HTML::Mason::Interp->new( comp_root => File::Spec->rel2abs(cwd),
                              data_dir  => '/tmp/mason-benchmarks',
                            );

print "\n";
foreach my $name ( @{ $opts{test} } )
{
    Benchmark::timethis( $opts{reps}, $tests{$name}, $name );
}
print "\n";

sub call_comp
{
    my ($comp, @args) = @_;

    my $out;
    $interp->out_method(\$out);
    $interp->exec( $comp, @args );

}

sub usage
{
    print <<'EOF';

bench.pl

  --test  Specify one or more tests to perform.  Valid tests include:

            print  (focuses on $m->print)
            comp   (focuses on $m->comp)

  -reps   Number of times to repeat each test.  Defaults to 1000.

EOF
}

__END__

=pod

=head1 many_prints.pl

This benchmarks a component that ends up making a B<lot> of calls to
C<< $m->print >>.

By default, it calls the same component 1000 times.  If you give it an
argument it will call the component this many times instead.

=cut
