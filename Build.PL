use strict;

use 5.005;

use lib 'inc';

use Mason::Build;

use File::Spec;

my %prereq = ( 'Cache::Cache'     => 1.00,
               'Class::Container' => 0.07,
               'CGI'              => 2.46,
               'Exception::Class' => 1.14,
               'File::Spec'       => 0.8,
               'Params::Validate' => 0.59,
               'Scalar::Util'     => 1.01,
             );

eval { require mod_perl };
my $has_mod_perl_1 = $@ ? 0 : 1;

if ($has_mod_perl_1)
{
    $prereq{mod_perl} = 1.24;
}

my $has_only_mp2 = ! $has_mod_perl_1 && eval { require Apache2; 1; };

unless ( $@ || $^O =~ /mac|darwin/ || $has_only_mp2 )
{
    $prereq{'Apache::Request'} = 0.32;
}

eval { require Apache::Filter };

unless ( $@ || $Apache::Filter::VERSION >= 1.021 )
{
    $prereq{'Apache::Filter'} = 1.021;
}

my $build = Mason::Build->new
  (
   module_name => 'HTML::Mason',
   requires    => \%prereq,
   build_requires => {
		      'Test'          => 0,
		      'Test::Builder' => 0,
		     },
   license     => 'perl',
  );

$build->create_build_script;
