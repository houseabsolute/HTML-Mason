#!/usr/bin/perl -w
use strict;
use Cwd;

my $pwd = cwd();
my $root = (-f "test-common.pl") ? "$pwd/.." : (-f "t/test-common.pl") ? "$pwd" : die "ERROR: cannot find test-common.pl\n";

unshift(@INC,"$root/lib");

require "$root/t/test-common.pl";

my $comp_root = "$root/test/comps";
my $data_dir = "$root/test/data";
my $cache_dir = "$root/test/data/cache";
my $results_dir = "$root/test/results/";
my $tmp_dir = "$root/test/tmp/";

my $buf;

# Optional argument gives pattern of tests to run
my $pattern = $ARGV[0];

# Read list of test components
my $listfh = new IO::File "$root/test/comps.lst" or die "cannot read component list";
my @comps = <$listfh>;
chomp(@comps);
$listfh->close;

# Clear cache directory
if (-d $cache_dir) {
    my $d = new DirHandle ($cache_dir) or die "cannot read directory $cache_dir";
    while (defined (my $file = $d->read)) {
	unlink("$cache_dir/$file");
    }
}

my $parser = new HTML::Mason::Parser;
my $interp = new HTML::Mason::Interp( parser=>$parser,
                                      comp_root => $comp_root,
                                      data_dir => $data_dir,
                                      out_method => \$buf, );

print "1..".scalar(@comps)."\n";

foreach my $component ( @comps ) {
    next if (defined($pattern) and $component !~ /$pattern/);
    print "Running $component\n";
    $buf = "";
    my $result;
    eval { $interp->exec("/$component"); };
    if (my $err = $@) {
	print "ERROR:\n$err\nnot ok\n";
	next;
    }
    $component =~ s/\//::/g;
    open(F, ">$tmp_dir/$component");
    print F $buf if defined($buf);
    close F;
    my $diff = compareFiles("$tmp_dir/$component", "$results_dir/$component");
    print ( $diff == 0 ? "ok\n" : "not ok\n" );
}


# The file comparison subroutines below were taken from Gerald Richter's
# HTML::Embperl package.

sub chompcr {
    chomp ($_[0]) ;
    if ($_[0] =~ /(.*?)\s*\r$/) {
	$_[0] = $1;
    } elsif ($_[0] =~ /(.*?)\s*$/) {
	$_[0] = $1;
    }
}

sub compareFiles {
    my ($f1, $f2, $errin) = @_;
    my $line = 1;
    my $err  = 0;
    my($l1, $l2, $eq);

    open F1, $f1 or do { print "Cannot open $f1\n"; return 1 };
    if (!$errin) {
	open F2, $f2 or do { print "Cannot open $f2\n"; return 1 };
    }

    while (defined ($l1 = <F1>)) {
	chompcr ($l1);
	if (!$errin) {
	    $l2 = <F2>;
	    chompcr ($l2);
	}
	if (!defined ($l2)) {
	    print "\nError in Line $line\nIs:\t$l1\nShould:\t<EOF>\n";
	    return $line;
	}

	$eq = 0;
	while (((($l2 =~ /^\^\^(.*?)$/)) || ($l2 =~ /^\^\-(.*?)$/)) && !$eq) {
	    $l2 = $1;
	    if (($l1 =~ /^\s*$/) && ($l2 =~ /^\s*$/)) { 
		$eq = 1;
	    } else {
		$eq = $l1 =~ /$l2/;
	    }
	    $l2 = <F2> if (!$eq);
	    chompcr ($l2);
	}

	if (!$eq) {
	    if ($l2 =~ /^\^(.*?)$/) {
		$l2 = $1;
		$eq = $l1 =~ /$l2/;
	    } else {
		$eq = $l1 eq $l2;
	    }
	}

	if (!$eq) {
	    print "\nError in Line $line\nIs:\t>$l1<\nShould:\t>$l2<\n";
	    return $line;
	}
	$line++;
    }

    if (!$errin) {
	while (defined ($l2 = <F2>)) {
	    chompcr ($l2);
	    if (!($l2 =~ /^\s*$/)) {
		print "\nError in Line $line\nIs:\t\nShould:\t$l2\n";
		return $line;
	    }
	    $line++;
	}
    }

    close F1;
    close F2;

    return $err; 
}

1;
