use ExtUtils::MakeMaker;
require 5.004;

#--- Installation check

sub chk_version
{
 my($pkg,$wanted,$msg) = @_;

 local($|) = 1;
 print "Checking for $pkg...";

 eval { my $p; ($p = $pkg . ".pm") =~ s#::#/#g; require $p; };

 my $vstr = ${"${pkg}::VERSION"} ? "found v" . ${"${pkg}::VERSION"}
				 : "not found";
 my $vnum = ${"${pkg}::VERSION"} || 0;

 print $vnum >= $wanted ? "ok\n" : " " . $vstr . "\n";

 $vnum >= $wanted;
}

# These are required modules.

chk_version(File::Tools => '1.0') or
    warn  "\n"
	. "*** Mason requires version 1.0, or later, of File::Tools\n"
	. "    from CPAN/modules/by-module/File/Tools-x.x.tar.gz\n\n";

chk_version(MLDBM => '2.0') or
    warn  "\n"
	. "*** Mason requires version 2.0, or later, of MLDBM\n"
	. "    from CPAN/modules/by-module/MLDBM-x.x.tar.gz\n\n";

chk_version(Data::Dumper => '2.08') or
    warn  "\n"
	. "*** Mason requires version 2.08, or later, of Data::Dumper\n"
	. "    from CPAN/modules/by-module/Data/Dumper-x.x.tar.gz\n\n";

# These are optional modules.

chk_version(Time::HiRes => '1.19') or do {
    warn  "\n"
	. "*** Mason needs version 1.19, or later, of Time::HiRes\n"
        . "    in order to record microsecond time values in the system\n"
	. "    log. Since you do not seem to have this module,\n"
	. "    system log times will be recorded in seconds\n"
	. "    only.  If you do decide to obtain Time::HiRes, run\n"
	. "    Makefile.PL again or edit Config.pm.\n\n";
    $noTimeHiRes = 1;
};

my $conf;
my $read = open(F,"lib/HTML/Mason/Config.pm");
if ($read) {
    local $/ = undef;
    $conf = <F>;
    close (F);
    if ($noTimeHiRes) {
        $conf =~ s/^use Time::HiRes;$/#use Time::HiRes;/m;
    } else {
        $conf =~ s/^#\s*use Time::HiRes;$/use Time::HiRes;/m;
    }
    my $write = open(F,">lib/HTML/Mason/Config.pm");
    if ($write) {
        print F $conf;
        print "Writing lib/HTML/Mason/Config.pm\n";
        close(F);
    } else {
        warn "cannot write lib/HTML/Mason/Config.pm\n";
    }
} else {
    warn "cannot read lib/HTML/Mason/Config.pm\n";
}

# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.
WriteMakefile(
    'NAME'    => 'HTML::Mason',
    'VERSION_FROM' => 'lib/HTML/Mason.pm', # finds $VERSION
    'PREREQ_PM' => { MLDBM => 2.0, Data::Dumper => 2.08, File::Tools => 1.0 }
);
