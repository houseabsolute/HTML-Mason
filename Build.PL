use strict;

use 5.005;

use lib 'inc';

use Mason::Build;

use File::Spec;

my %prereq = ( 'Cache::Cache'     => 1.00,
               'Class::Container' => 0.07,
               'CGI'              => 2.46,
               'Exception::Class' => 1.15,
               'File::Spec'       => 0.8,
               'Params::Validate' => 0.70,
               'Scalar::Util'     => 1.01,
             );

eval { require mod_perl };
my $has_mod_perl_1 = $@ ? 0 : 1;

if ($has_mod_perl_1)
{
    $prereq{mod_perl} = 1.24;
}

eval { require mod_perl2 };
my $has_mod_perl_2 = $@ ? 0 : 1;
my $has_only_mp2 = ! $has_mod_perl_1 && $has_mod_perl_2;

# XXX - this really isn't right since we really want to know which
# version of mod_perl they _intend_ to use with Mason, rather than
# just blindly adding all these prereqs.
if ($has_mod_perl_1)
{
    $prereq{'Apache::Request'} = 1.05; # minimum for OSX
}

if ($has_only_mp2)
{
    $prereq{CGI} = 3.08;
    $prereq{mod_perl} = 1.999022;
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
   create_makefile_pl => 'passthrough',
  );

$build->create_build_script;
