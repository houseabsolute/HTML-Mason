use strict;

use 5.005;

use lib 'inc';

use Mason::Build;

use File::Spec;

eval { require mod_perl };
my $has_only_mp2 = $@ && eval { require Apache2; 1; };

my %prereq = ( 'Apache::Request'  => 0.31,
               'Cache::Cache'     => 1.00,
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
		      'Test'          => 0,
		     },
   license     => 'perl',
  );

$build->create_build_script;
