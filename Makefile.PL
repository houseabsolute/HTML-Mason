use strict;
use 5.005;

use ExtUtils::MakeMaker;
use Getopt::Long;

use vars qw(%APACHE);

my %opts;
GetOptions( \%opts, 'no-prompts' );

require './makeconfig.pl';
require './apache_tests_helper.pl';
require './assisted_install_helper.pl';

# These are required modules.

chk_version('Data::Dumper' => '2.08') or
    warn  "\n"
	. "*** Mason requires version 2.08, or later, of Data::Dumper\n"
	. "    from CPAN/modules/by-module/Data/Data-Dumper-x.x.tar.gz\n\n";

chk_version('File::Spec' => '0.8') or
    warn  "\n"
	. "*** Mason requires version 0.8, or later, of File::Spec\n"
	. "    from CPAN/modules/by-module/File/File-Spec-x.x.tar.gz\n\n";

chk_version('Params::Validate' => '0.07') or
    warn  "\n"
	. "*** Mason requires version 0.07, or later, of Params::Validate\n"
	. "    from CPAN/modules/by-authors/id/D/DR/DROLSKY/Params-Validate-x.x.tar.gz\n\n";

chk_version('Apache::Request' => '0.31') or
    warn  "\n"
	. "*** If you plan to use Mason with mod_perl,\n"
        . "    Mason requires version 0.31, or later, of Apache::Request\n"
	. "    from CPAN/modules/by-module/Apache/Apache-Request-x.x.tar.gz\n\n";

chk_version('Exception::Class' => '0.97') or
    warn  "\n"
	. "*** If you plan to use Mason with mod_perl,\n"
        . "    Mason requires version 0.97, or later, of Exception::Class\n"
	. "    from CPAN/modules/by-module/Exception/Exception-Class-x.x.tar.gz\n\n";

chk_version('Cache::Cache' => '0.99') or
    warn  "\n"
	. "*** If you plan to use Mason with mod_perl,\n"
        . "    Mason requires version 0.99, or later, of Cache::Cache\n"
	. "    from CPAN/modules/by-authors/id/D/DC/DCLINTON/Cache-Cache-x.x.tar.gz\n\n";

make_config();
setup_mod_perl_tests() unless $opts{'no-prompts'};
assisted_install_config() unless $opts{'no-prompts'};

# Get these into the MY namespace for the MY::test method
%MY::APACHE = %APACHE;

# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.
WriteMakefile(
    'NAME'    => 'HTML::Mason',
    'VERSION_FROM' => 'lib/HTML/Mason.pm', # finds $VERSION
    'PREREQ_PM' => { 'Data::Dumper' => 2.08, 'File::Spec' => 0.8, 'Params::Validate' => 0.07,
		     'Apache::Request' => 0.31,
		     'Exception::Class' => 0.97,
		     'Cache::Cache' => 0.99,
		     },

    # %APACHE is created when setup_mod_perl_tests() is called.  The
    # code that does this creation is in makeconfig.pl.
    clean => {
	'FILES' => "apache_install.txt lib/HTML/Mason/Config.pm mason_tests $APACHE{apache_dir}/httpd.conf $APACHE{apache_dir}/error_log $APACHE{apache_dir}/httpd $APACHE{apache_dir}/httpd.pid $APACHE{comp_root} $APACHE{data_dir} $APACHE{apache_dir}/mason_handler_CGI.pl $APACHE{apache_dir}/mason_handler_mod_perl.pl",
    }
);

package MY;

sub install
{
    my $self = shift;

    my $install = $self->SUPER::install(@_);

    $install =~ s/install :: all pure_install doc_install/install :: all pure_install doc_install configure_apache/;

    $install .= "\nconfigure_apache :\n\t\$(PERL) configure_apache.pl\n";

    return $install;
}

sub test
{
    my $self = shift;

    my $test = $self->SUPER::test(@_);

    # %MY::APACHE is set in makeconfig.pl.
    # If we are not going to test with Apache there is no harm in
    # setting this anyway.

    # The PORT env var is used by Apache::test.  Don't delete it!
    my $port = $MY::APACHE{port} || 8228;
    $MY::APACHE{apache_dir} ||= '';
    $test =~ s/(runtests \@ARGV;)/\$\$ENV{MASON_VERBOSE} = \$(TEST_VERBOSE) ? \$(TEST_VERBOSE) : \$\$ENV{MASON_VERBOSE}; \$\$ENV{PORT}=$port; \$\$ENV{APACHE_DIR}=q^$MY::APACHE{apache_dir}^; $1/;

    return $test;
}

sub libscan
{
    my $self = shift;
    my $file = shift;

    return $file =~ /makeconfig\.pl/ ? 0 : $self->SUPER::libscan($file);
}

