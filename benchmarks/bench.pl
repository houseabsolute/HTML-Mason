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

die "$0 must be run from inside the benchmarks/ directory\n"
  unless -e 'comps' and -d 'comps';

# Don't check this into CVS because it's big:
unless (-e 'comps/large.mas') {
  open my($fh), '> comps/large.mas' or die "Can't create comps/large.mas: $!";
  print $fh 'x' x 79, "\n" for 1..30_000; # 80 * 30_000 = 2.4 MB
}

my %tests =
    ( print =>
      sub { call_comp( '/comps/print.mas', title => 'print', integer => 1000 ) },

      comp =>
      sub { call_comp( '/comps/comp.mas' ) },

      large =>
      sub { call_comp( '/comps/large.mas' ) },
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
                              data_dir  =>
                              File::Spec->rel2abs( File::Spec->catdir( cwd, 'mason-data' ) ),
                            );

print "\n";
foreach my $name ( @{ $opts{test} } )
{
    Benchmark::timethis( $opts{reps}, $tests{$name}, $name );
    my ($rss, $vsz) = `ps -eo rss,vsz -p $$` =~ /(\d+)\s+(\d+)/;
    print "   Real mem: $rss\n";
    print "Virtual mem: $vsz\n";
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
            large  (a large component)

  --reps  Number of times to repeat each test.  Defaults to 1000.

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
