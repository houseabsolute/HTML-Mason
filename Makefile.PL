use ExtUtils::MakeMaker;

use 5.005;
require './makeconfig.pl';

# These are required modules.

chk_version(Data::Dumper => '2.08') or
    warn  "\n"
	. "*** Mason requires version 2.08, or later, of Data::Dumper\n"
	. "    from CPAN/modules/by-module/Data/Data-Dumper-x.x.tar.gz\n\n";

chk_version(File::Spec => '0.8') or
    warn  "\n"
	. "*** Mason requires version 0.8, or later, of File::Spec\n"
	. "    from CPAN/modules/by-module/File/File-Spec-x.x.tar.gz\n\n";

chk_version(Params::Validate => '0.04') or
    warn  "\n"
	. "*** Mason requires version 0.04, or later, of Params::Validate\n"
	. "    from CPAN/modules/by-authors/id/D/DR/DROLSKY/Params-Validate-x.x.tar.gz\n\n";

chk_version(Apache::Request => '0.31') or
    warn  "\n"
	. "*** If you plan to use Mason with mod_perl,\n"
        . "    Mason requires version 0.31, or later, of Apache::Request\n"
	. "    from CPAN/modules/by-module/Apache/Apache-Request-x.x.tar.gz\n\n";

make_config();
setup_mod_perl_tests();

# Get these into the MY namespace for the MY::test method
%MY::APACHE = %APACHE;

# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.
WriteMakefile(
    'NAME'    => 'HTML::Mason',
    'VERSION_FROM' => 'lib/HTML/Mason.pm', # finds $VERSION
    'PREREQ_PM' => { Data::Dumper => 2.08, 'File::Spec' => 0.8, 'Params::Validate' => 0.04 },

    # %APACHE is created when setup_mod_perl_tests() is called.  The
    # code that does this creation is in makeconfig.pl.
    clean => {
	'FILES' => "lib/HTML/Mason/Config.pm mason_tests $APACHE{apache_dir}/httpd.conf $APACHE{apache_dir}/error_log $APACHE{apache_dir}/httpd $APACHE{comp_root} $APACHE{data_dir} $APACHE{apache_dir}/mason_handler_CGI.pl $APACHE{apache_dir}/mason_handler_mod_perl.pl",
    }
);

package MY;

sub test
{
    my $self = shift;

    my $test = $self->SUPER::test(@_);

    $test =~ s/(runtests \@ARGV;)/\$\$ENV{MASON_VERBOSE}=\$(TEST_VERBOSE); $1/;

    # %APACHE is set in makeconfig.pl.
    # If we are not going to test with Apache there is no harm in
    # setting this anyway.

    # The PORT env var is used by Apache::test.  Don't delete it!
    my $port = $APACHE{port} || 8228;
    $APACHE{apache_dir} ||= '';
    $test =~ s/(runtests \@ARGV;)/\$\$ENV{MASON_VERBOSE}=\$(TEST_VERBOSE); \$\$ENV{PORT}=$port; \$\$ENV{APACHE_DIR}=q^$APACHE{apache_dir}^; $1/;

    return $test;
}

sub libscan
{
    my $self = shift;
    my $file = shift;

    return $file =~ /makeconfig\.pl/ ? 0 : $self->SUPER::libscan($file);
}

