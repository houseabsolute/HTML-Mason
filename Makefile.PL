use ExtUtils::MakeMaker;
require 5.004;
require './makeconfig.pl';

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

chk_version(MLDBM => '2.0') or
    warn  "\n"
	. "*** Mason requires version 2.0, or later, of MLDBM\n"
	. "    from CPAN/modules/by-module/MLDBM-x.x.tar.gz\n\n";

chk_version(Data::Dumper => '2.08') or
    warn  "\n"
	. "*** Mason requires version 2.08, or later, of Data::Dumper\n"
	. "    from CPAN/modules/by-module/Data/Dumper-x.x.tar.gz\n\n";

make_config();

# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.
WriteMakefile(
    'NAME'    => 'HTML::Mason',
    'VERSION_FROM' => 'lib/HTML/Mason.pm', # finds $VERSION
    'PREREQ_PM' => { MLDBM => 2.0, Data::Dumper => 2.08 }
);
