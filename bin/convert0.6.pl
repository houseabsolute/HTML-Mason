#!/usr/bin/perl -w

use Data::Dumper;
use File::Find;	   
use Getopt::Std;
use IO::File;
use strict;

my ($LOWER, $QUIET, $TEST, $UPPER);

sub convert
{
    my ($file) = @_;
    my $buf;
    my $infh = new IO::File $file;
    if (!$infh) { warn "cannot read $file: $!"; return }
    { local $/ = undef; $buf = <$infh> }

    my $c = 0;
    my @changes;
    my $report = sub { push(@changes,"$_[0]  -->  $_[1]") };

    #
    # Convert section names to short versions
    #
    my $pat = "<(/?%)perl_(args|cleanup|doc|init|once|text)>";
    if (!$TEST) {
	if ($UPPER) {
	    $c += ($buf =~ s{$pat}{"<$1".uc($2).">"}geio);
	} elsif ($LOWER) {
	    $c += ($buf =~ s{$pat}{"<$1".lc($2).">"}geio);
	} else {
	    $c += ($buf =~ s{$pat}{<$1$2>}gio);
	}
    } else {
	while ($buf =~ m{($pat)}gio) {
	    $report->($1,"<$2".($UPPER ? uc($3) : $LOWER ? lc($3) : $3).">");
	}
    }

    #
    # Convert <% mc_comp ... %> to <& ... &>
    #
    if (!$TEST) {
	$c += ($buf =~ s{<%\s*mc_comp\s*\(\s*\'([^\']+)\'\s*(.*?)\s*\)%>} {<& $1$2 &>}g);
	$c += ($buf =~ s{<%\s*mc_comp\s*\(\s*\"([^\"]+)\"\s*(.*?)\s*\)%>} {<& $1$2 &>}g);
    } else {
	while ($buf =~ m{(<%\s*mc_comp\s*\(\s*\'([^\']+)\'\s*(.*?)\s*\)%>)}g) {
	    $report->($1,"<& $2$3 &>");
	}
	while ($buf =~ m{(<%\s*mc_comp\s*\(\s*\"([^\"]+)\"\s*(.*?)\s*\)%>)}g) {
	    $report->($1,"<& $2$3 &>");
	}
    }

    if (@changes) {
	print scalar(@changes)." substitutions in $file:\n";
	print join("\n",@changes)."\n\n";
    }
    
    if ($c) {
	print "$c substitutions in $file\n" if !$QUIET;
	my $outfh = new IO::File ">$file";
	if (!$outfh) { warn "cannot write $file: $!"; return }
	$outfh->print($buf);
    }
}

my $usage = <<EOF;
Usage: $0 -lqtu dir...
-l: Write all section names as lowercase (<%init>, etc.)
-q: Quiet mode, do not report normal processing of files
-t: Do not actually change files, just summarize what changes would be made
-u: Write all section names as uppercase (<%INIT>, etc.)
EOF

sub usage
{
    print $usage;
    exit;
}

sub main
{
    my (%opts);
    getopts('lqtu',\%opts);
    ($LOWER, $QUIET, $TEST, $UPPER) = @opts{qw(l q t u)};
    my @dirs = @ARGV;
    my $sub = sub {
	if (-f $_) { convert("$File::Find::dir/$_") }
    };
    find($sub,@dirs);
}

main();
