use strict;
use 5.005;

use ExtUtils::MakeMaker;
use Getopt::Long;

use vars qw(%APACHE);

my %opts;
GetOptions( \%opts, 'no-prompts' );

require 'install/apache_tests_helper.pl';
require 'install/assisted_install_helper.pl';

eval { require HTML::Mason };

unless ($@)
{
    if ( $HTML::Mason::VERSION < 1.09 )
    {
	print <<"EOF";

It looks like you have an older version of Mason already installed on
your machine (version $HTML::Mason::VERSION).  This version is not backwards
compatible with versions of Mason before version 1.10.
Please read the UPGRADE document before continuing with this
installation.

EOF

	unless ( $opts{'no-prompts'} )
	{
	    my $yn = prompt('Continue with installation?', 'N');

	    exit unless $yn =~ /y(?:es)?/i;
	}
    }
}

# These are required modules.
my %prereq = ( 'File::Spec' => {version => 0.8,
				where => "CPAN/modules/by-module/File/File-Spec-x.x.tar.gz"},
	       'Params::Validate' => {version => 0.24,
				      where => "CPAN/modules/by-authors/id/D/DR/DROLSKY/Params-Validate-x.x.tar.gz"},
	       'Exception::Class' => {version => 1.01,
				      where => "CPAN/modules/by-module/Exception/Exception-Class-x.x.tar.gz"},
	       'Cache::Cache' => {version => 1.00,
				  where => "CPAN/modules/by-authors/id/D/DC/DCLINTON/Cache-Cache-x.x.tar.gz"},
               'Class::Container' => {version => 0.07,
				      where => "CPAN/modules/by-authors/id/K/KW/KWILLIAMS/Class-Container-x.x.tar.gz"},
	     );

foreach (keys %prereq) {
    chk_version($_ => $prereq{$_}{version}) or
	warn  "\n"
	    . "*** Mason requires version $prereq{$_}{version} or later of $_\n"
	    . "    from $prereq{$_}{where}\n\n";
    $prereq{$_} = $prereq{$_}{version};
}

eval { require mod_perl };
unless ( $@ || $^O =~ /mac|darwin/ )
{
    chk_version('Apache::Request' => '0.31') or
	warn  "\n"
	    . "*** If you plan to use Mason with mod_perl,\n"
	    . "    Mason requires version 0.31, or later, of Apache::Request\n"
	    . "    from CPAN/modules/by-module/Apache/Apache-Request-x.x.tar.gz\n\n";

    $prereq{'Apache::Request'} = 0.31;
}

setup_mod_perl_tests() if is_maintainer() && ! $opts{'no-prompts'};
assisted_install_config() unless $opts{'no-prompts'};

# Get these into the MY namespace for the MY::test method
%MY::APACHE = %APACHE;

# Be very careful about removing these files - otherwise, if
# apache_dir isn't set, we're removing things like /httpd and /mason !
my $more_clean_files = ($APACHE{apache_dir} 
			? join(' ', map "$APACHE{apache_dir}/$_", 
			       qw(httpd.conf error_log httpd httpd.pid
                                  mason_handler_CGI.pl mason_handler_mod_perl.pl 
                                  CGIHandler.cgi mason)
			      )
			: '');
  

WriteMakefile(
    'NAME'    => 'HTML::Mason',
    'VERSION_FROM' => 'lib/HTML/Mason.pm',
    'PREREQ_PM' => \%prereq,

    # %APACHE is created when setup_mod_perl_tests() is called.  The
    # code that does this creation is in apache_tests_helper.pl.
    clean => {
	'FILES' => "apache_install.txt lib/HTML/Mason/Config.pm mason_tests $APACHE{comp_root} $APACHE{data_dir} $more_clean_files",
    }
);

sub chk_version
{
    my ($pkg, $wanted, $msg) = @_;

    local $| = 1;
    print "Checking for $pkg...";

    eval { eval "require $pkg" };

    my $vstr;
    my $vnum;
    {
	no strict 'refs';
	$vstr = ${"${pkg}::VERSION"} ? "found v" . ${"${pkg}::VERSION"}	: "not found";
	$vnum = ${"${pkg}::VERSION"} || 0;
    }

    print $vnum >= $wanted ? "ok\n" : " " . $vstr . "\n";

    $vnum >= $wanted;
}

sub is_maintainer
{
    return $ENV{MASON_MAINTAINER} if exists $ENV{MASON_MAINTAINER};

    return -d 'CVS' ? 1 : 0;
}

package MY;

sub install
{
    my $self = shift;

    my $install = $self->SUPER::install(@_);

    $install =~ s/install :: all pure_install doc_install/install :: all pure_install doc_install delete_old_docs configure_apache/;

    $install .= "\nconfigure_apache :\n\t\$(PERL) install/configure_apache.pl\n";

    $install .= "\ndelete_old_docs :\n\t\$(PERL) install/delete_old_pods.pl\n";

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

    my $is_maintainer = main::is_maintainer();

    # This works for older MakeMakers
    $test =~ s/(runtests \@ARGV;)/\$\$ENV{MASON_VERBOSE} == \$(TEST_VERBOSE) ? \$(TEST_VERBOSE) : \$\$ENV{MASON_VERBOSE}; \$\$ENV{PORT}=$port; \$\$ENV{APACHE_DIR}=q^$MY::APACHE{apache_dir}^; \$\$ENV{MASON_MAINTAINER}=$is_maintainer; $1/;

    # This works for newer MakeMakers (5.48_01 +)
    $test =~ s/("-MExtUtils::Command::MM" "-e" ")(test_harness\(\$\(TEST_VERBOSE\).*?\)" \$\(TEST_FILES\))/$1 \\\$\$ENV{MASON_VERBOSE} == \$(TEST_VERBOSE) ? \$(TEST_VERBOSE) : \\\$\$ENV{MASON_VERBOSE}; \\\$\$ENV{PORT}=$port; \\\$\$ENV{APACHE_DIR}=q^$MY::APACHE{apache_dir}^; \\\$\$ENV{MASON_MAINTAINER}=$is_maintainer; $2/;

    return $test;
}

# prevents the Makefile from trying to process the Build.PL file into
# Build
sub processPL { '' }

sub libscan
{
    my $self = shift;
    my $file = shift;

    return $file =~ /\.pl$/ ? 0 : $self->SUPER::libscan($file);
}

