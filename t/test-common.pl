use HTML::Mason;
use DirHandle;
use strict;

use vars (qw($root $branch $comp_root $data_dir));

$comp_root = "$root/test/comps";
$data_dir = "$root/test/data";
my $comp_pattern = $ARGV[0];

sub try_exec_all
{
    my $interp = new HTML::Mason::Interp(comp_root => $comp_root, data_dir => $data_dir);
    clear_cache_dir();
    
    my $listfile = "$root/test/comps/$branch/comps.lst";
    my @comps;
    if (-f $listfile) {
	my $listfh = new IO::File $listfile or die "cannot read $listfile: $!";
	chomp(@comps = <$listfh>);
    } else {
	my $dir = "$root/test/comps/$branch";
	my $dh = new DirHandle $dir or die "cannot read $dir: $!";
	@comps = grep(!/^\./ && -f ("$dir/$_"),$dh->read);
    }

    print "1..".scalar(@comps)."\n";
    foreach my $component ( @comps ) {
	try_exec($interp, $component);
    }
}

sub try_exec {
    my ($interp, $component) = @_;
    next if (defined($comp_pattern) and $component !~ /$comp_pattern/);
    print "Running $component\n";
    
    my $results_dir = "$root/test/results/$branch";
    my $tmp_dir = "$root/test/tmp/$branch";
    my $buf = "";
    $interp->out_method(\$buf);
    my $result;
    eval { $interp->exec("/$branch/$component"); };
    if (my $err = $@) {
	print "ERROR:\n$err\nnot ok\n";
	next;
    }
    $component =~ s/\//::/g;
    my $outfile = "$tmp_dir/$component";
    my $fh = new IO::File ">$outfile" or die "cannot write $outfile: $!";
    $fh->print($buf) if defined($buf);
    $fh->close;
    my $diff = compare_files("$results_dir/$component", "$tmp_dir/$component");
    print ( $diff ? "$diff\nnot ok\n" : "ok\n" );
}

# Clear cache directory
sub clear_cache_dir
{
    my $cache_dir = "$root/test/data/cache";
    if (-d $cache_dir) {
	my $d = new DirHandle ($cache_dir) or die "cannot read directory $cache_dir: $!";
	while (defined (my $file = $d->read)) {
	    unlink("$cache_dir/$file");
	}
    }
}

sub compare_files
{
    my ($file1,$file2) = @_;
    my $fh1 = new IO::File $file1 or die "cannot read $file1: $!";
    my $fh2 = new IO::File $file2 or die "cannot read $file2: $!";
    my $linenum = 0;
    my $err = sub {
	my ($linenum,$line1,$line2) = @_;
	return "\nError in line $linenum\nExpected: $line1\nGot: $line2\n";
    };
    
    while (defined (my $line1 = <$fh1>)) {
	$linenum++;
	if (defined (my $line2 = <$fh2>)) {
	    for ($line1,$line2) { chomp; s/\cM//g }
	    return $err->($linenum,$line1,$line2) unless ($line1 eq $line2);
	} else {
	    return $err->($linenum,$line1,"<EOF>");
	}
    }
    if (defined (my $line2 = <$fh2>)) {
	return $err->($linenum,"<EOF>",$line2);
    }
    return undef;
}

1;
