BEGIN { $HTML::Mason::IN_DEBUG_FILE = 1 if !$HTML::Mason::IN_DEBUG_FILE }
use Data::Dumper;
use DirHandle;
use HTML::Mason;
use HTML::Mason::ApacheHandler;
use Getopt::Std;
use strict;

use vars (qw($root $branch $comp_root $data_dir $comp_pattern $create_mode));

sub init
{
    my %opts;
    getopts('ct:',\%opts);

    $comp_root = "$root/test/comps";
    $data_dir = "$root/test/data";

    $comp_pattern = $opts{t};
    $create_mode = $opts{c};
}

sub try_exec_all
{
    my $interp = new HTML::Mason::Interp(comp_root => $comp_root, data_dir => $data_dir);
    clear_cache_dir();
    
    my $listfile = "$root/test/comps/$branch/comps.lst";
    my @comps;
    if (-f $listfile) {
	my $listfh = new IO::File $listfile or die "cannot read $listfile: $!";
	chomp(@comps = grep(/\S/,<$listfh>));
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
    my ($interp, $component, $iteration) = @_;
    next if (defined($comp_pattern) and $component !~ /$comp_pattern/);
    my $itermark = ($iteration) ? "-$iteration" : "";
    my $test_name = $component . $itermark;
    print "Running $test_name\n";
    
    my $buf = "";
    $interp->out_method(\$buf);
    eval { $interp->exec("/$branch/$component"); };
    if (my $err = $@) {
	print "ERROR:\n$err\nnot ok\n";
	return;
    }

    compare_results($test_name, $buf);
}

sub compare_results {
    my ($test_name, $buf) = @_;
    my $results_dir = "$root/test/results/$branch";
    my $tmp_dir = "$root/test/tmp/$branch";
    unless (-d $tmp_dir) {
	mkdir($tmp_dir,0775) or die "could not make directory $tmp_dir: $!";
    }

    $test_name =~ s/\//::/g;
    
    my $tmpfile = "$tmp_dir/$test_name";
    my $fh = new IO::File ">$tmpfile" or die "cannot write $tmpfile: $!";
    $fh->print($buf) if defined($buf);
    $fh->close;
    my $resultsfile = "$results_dir/$test_name";
    if (!-f $resultsfile and $create_mode) {
	print "$resultsfile does not exist.\n$tmpfile contains:\n$buf\nCopy to results? ";
	my $ans = <>;
	system("cp $tmpfile $resultsfile") if ($ans =~ /^[Yy]/);
    }
    my $diff = compare_files($resultsfile, $tmpfile);
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
