#!/usr/bin/perl -w

use strict;

use lib '../lib';

use Benchmark;
use Cwd;
use Fcntl qw( O_RDWR O_CREAT );
use HTML::Mason;
use Getopt::Long;
use MLDBM qw( DB_File Storable );
use File::Path;

my %tests =
    ( print =>
      { code =>
        sub { call_comp( '/comps/print.mas', title => 'print', integer => 1000 ) },
        description =>
        'Calls $m->print many times.',
      },

      one_comp =>
      { code =>
        sub { call_comp( '/comps/comp.mas' ) },
        description =>
        'Calls a single component',
      },

      large =>
      { code =>
        sub { call_comp( '/comps/large.mas' ) },
        description =>
        'Calls a very large text-only component',
      },
    );

my %opts = ( reps => 1000,
             tag  => $HTML::Mason::VERSION,
             save => 0,
           );

GetOptions( 'test:s'  => \@{ $opts{test} },
            'profile' => \$opts{profile},
            'reps:i'  => \$opts{reps},
            'help'    => \$opts{help},
            'tag:s'   => \$opts{tag},
            'save'    => \$opts{save},
	    'clear_cache' => \$opts{clear_cache},
          );

if ( $opts{help} || ! @{ $opts{test} } )
{
    usage();
    exit;
}

die "$0 must be run from inside the benchmarks/ directory\n"
  unless -e 'comps' and -d 'comps';

my $large_comp = File::Spec->catfile( 'comps', 'large.mas' );
# Don't check this into CVS because it's big:
unless ( -e $large_comp )
{
    open my $fh, ">$large_comp" or die "Can't create $large_comp: $!";
    print $fh 'x' x 79, "\n" for 1..30_000; # 80 * 30_000 = 2.4 MB
}

# Clear out the mason-data directory, otherwise we might include
# compilation in one run and not the next
my $data_dir = File::Spec->rel2abs( File::Spec->catdir( cwd, 'mason-data' ) );
rmtree($data_dir) if $opts{clear_cache};

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
                              data_dir  => $data_dir,
                            );

print "\n";
foreach my $name ( @{ $opts{test} } )
{
    my $results = Benchmark::timethis( $opts{reps}, $tests{$name}{code}, $name );

    my $per_sec = sprintf( '%.2f', $opts{reps} / ($results->[1] + $results->[2]) );

    my ($rss, $vsz) = `ps -eo rss,vsz -p $$` =~ /(\d+)\s+(\d+)/;
    print "   Real mem: $rss\n";
    print "Virtual mem: $vsz\n";

    if ( $opts{save} )
    {
        my %save;
        tie %save, 'MLDBM', 'result_history.db', O_CREAT | O_RDWR, 0644
            or die "Cannot tie to result_history.db: $!";

        my $tag = $opts{tag};
        my $old = $save{$tag};

        $old->{$name} ||= [];
        push @{ $old->{$name} }, $per_sec;

        $save{$tag} = $old;
    }
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
    my $comps;
    foreach my $name ( sort keys %tests )
    {
        $comps .= sprintf( '            %-10s   %s', $name, $tests{$name}{description} );
        $comps .= "\n";
    }

    print <<"EOF";

bench.pl

  --test  Specify one or more tests to perform.  Valid tests include:

$comps

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
