use strict;

use 5.005;

use lib 'inc';

use Mason::Build;

use File::Spec;

use Getopt::Long;
my %opts;
GetOptions( \%opts, 'no-prompts' );

require 'install/apache_tests_helper.pl';
require 'install/assisted_install_helper.pl';

eval { require HTML::Mason };

unless ($@)
{
    if ( $HTML::Mason::VERSION < 1.10 )
    {
        print <<"EOF";

It looks like you have an older version of Mason already installed on
your machine (version $HTML::Mason::VERSION).  This version is not backwards
compatible with versions of Mason before version 1.09_01.
Please read the UPGRADE document before continuing with this
installation.

EOF

        unless ( $opts{'no-prompts'} )
        {
            my $yn = Module::Build->prompt('Continue with installation?', 'N');

            exit unless $yn =~ /y(?:es)?/i;
        }
    }
}

setup_mod_perl_tests() unless $opts{'no-prompts'};
assisted_install_config() unless $opts{'no-prompts'};

use vars qw(%APACHE);

eval { require mod_perl };
my $has_only_mp2 = $@ && eval { require Apache2; 1; };

my %prereq = ( 'Apache::Request'  => 0.31,
               'Cache::Cache' => 1.00,
               'Class::Container' => 0.07,
               'CGI'              => 2.46,
               'Exception::Class' => 1.14,
               'File::Spec'       => 0.8,
               'Params::Validate' => 0.59,
               'Scalar::Util'     => 1.01,
             );

unless ( $@ || $^O =~ /mac|darwin/ || $has_only_mp2 )
{
    $prereq{'Apache::Request'} = 0.32;
}

my $build = Mason::Build->new
  (
   module_name => 'HTML::Mason',
   requires    => \%prereq,
   build_requires => {
		      'Module::Build' => '0.20',
		      'Test'          => 0,
		     },
   args => { apache => \%APACHE,
           },
   license     => 'perl',
  );

$build->add_to_cleanup
    ( 'apache_install.txt',
      'mason_tests',
      $APACHE{comp_root},
      $APACHE{data_dir},
      ( map { File::Spec->catfile( $APACHE{apache_dir}, $_ ) }
        qw( httpd.conf error_log httpd httpd.pid mason_handler_CGI.pl
            mason_handler_mod_perl.pl CGIHandler.cgi mason )
      )
    );
$build->create_build_script;
