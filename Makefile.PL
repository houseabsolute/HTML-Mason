use ExtUtils::MakeMaker;
require 5.004;
require './makeconfig.pl';

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
setup_mod_perl_tests();

# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.
WriteMakefile(
    'NAME'    => 'HTML::Mason',
    'VERSION_FROM' => 'lib/HTML/Mason.pm', # finds $VERSION
    'PREREQ_PM' => { MLDBM => 2.0, Data::Dumper => 2.08, 'File::Spec' => 0.8 },
    'DIR' => 'faq',

    # $COMP_ROOT & $DATA_DIR are globals created when
    # setup_mod_perl_tests() is called.  The code that does this
    # creation is in makeconfig.pl.
    clean => {
	'FILES' => "lib/HTML/Mason/Config.pm t/httpd.conf t/error_log t/httpd $COMP_ROOT $DATA_DIR t/mason_handler_CGI.pl t/mason_handler_mod_perl.pl t/error_log t/access_log",
    }
);


package MY;

sub test
{
    my $self = shift;

    my $test = $self->SUPER::test(@_);

    my $port = $APACHE_PARAMS{port} || 8228;
    $test =~ s/(runtests \@ARGV;)/\$\$ENV{MASON_VERBOSE}=\$(TEST_VERBOSE); \$\$ENV{PORT}=$port; $1/;

    return $test;
}
