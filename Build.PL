use strict;
use 5.005;
use Module::Build 0.05_01;

require 'install/apache_tests_helper.pl';
require 'install/assisted_install_helper.pl';

use Getopt::Long;
my %opts;
GetOptions( \%opts, 'no-prompts' );

setup_mod_perl_tests() unless $opts{'no-prompts'};
#assisted_install_config() unless $opts{'no-prompts'};

use vars qw(%APACHE);

my $class = Module::Build->subclass( code => <<'EOF' );

  sub ACTION_install
  {
    my $self = shift;
    
    $self->SUPER::ACTION_install;
    $self->run_script('install/delete_old_pods.pl');  # These could probably be separate actions.
    $self->run_script('install/configure_apache.pl');
  }

  sub ACTION_test
  {
    my $self = shift;
    
    $ENV{APACHE_DIR}    = $self->{args}{apache_dir};
    $ENV{PORT}          = $self->{args}{port};
    $ENV{MASON_VERBOSE} = $self->{args}{mason_verbose};
    
    $self->SUPER::ACTION_test;
  }

EOF

my $build = $class->new
  (
   module_name => 'HTML::Mason',
   prereq => {
	      'File::Spec' => 0.8,
	      'Params::Validate' => 0.14,
	      'Apache::Request' => 0.31,
	      'Exception::Class' => 1.00,
	      'Cache::Cache' => 0.99,
	     },
   recommended => {
		   
		  },

   args => {
	    %APACHE,
	    mason_verbose => 0,
	   },
  );

$build->create_build_script;


__END__

    clean => {
	'FILES' => "apache_install.txt lib/HTML/Mason/Config.pm mason_tests $APACHE{apache_dir}/httpd.conf $APACHE{apache_dir}/error_log $APACHE{apache_dir}/httpd $APACHE{apache_dir}/httpd.pid $APACHE{comp_root} $APACHE{data_dir} $APACHE{apache_dir}/mason_handler_CGI.pl $APACHE{apache_dir}/mason_handler_mod_perl.pl $APACHE{apache_dir}/CGIHandler.cgi $APACHE{apache_dir}/mason",
    }
