use ExtUtils::MakeMaker;

use 5.005;
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

# Get these into the MY namespace for the MY::test method
%MY::APACHE_PARAMS = %APACHE_PARAMS;
$MY::APACHE_DIR = $APACHE_DIR;

# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.
WriteMakefile(
    'NAME'    => 'HTML::Mason',
    'VERSION_FROM' => 'lib/HTML/Mason.pm', # finds $VERSION
    'PREREQ_PM' => { MLDBM => 2.0, Data::Dumper => 2.08, 'File::Spec' => 0.8 },

    # $COMP_ROOT & $DATA_DIR are globals created when
    # setup_mod_perl_tests() is called.  The code that does this
    # creation is in makeconfig.pl.
    clean => {
	'FILES' => "lib/HTML/Mason/Config.pm mason_tests $APACHE_DIR/httpd.conf $APACHE_DIR/error_log $APACHE_DIR/httpd $COMP_ROOT $DATA_DIR $APACHE_DIR/mason_handler_CGI.pl $APACHE_DIR/mason_handler_mod_perl.pl",
    }
);

package MY;

sub test
{
    my $self = shift;

    my $test = $self->SUPER::test(@_);

    $test =~ s/(runtests \@ARGV;)/\$\$ENV{MASON_VERBOSE}=\$(TEST_VERBOSE); $1/;

    # %APACHE_PARAMS & $APACHE_DIR are globals set in makeconfig.pl.
    # If we are not going to test with Apache there is no harm in
    # setting this anyway.
    my $port = $APACHE_PARAMS{port} || 8228;
    $APACHE_DIR ||= '';
    $test =~ s/(runtests \@ARGV;)/\$\$ENV{MASON_VERBOSE}=\$(TEST_VERBOSE); \$\$ENV{PORT}=$port; \$\$ENV{APACHE_DIR}=q^$APACHE_DIR^; $1/;

    return $test;
}

sub libscan
{
    my $self = shift;
    my $file = shift;

    return $file =~ /makeconfig\.pl/ ? 0 : $self->SUPER::libscan($file);
}
