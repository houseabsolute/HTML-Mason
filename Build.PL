use strict;
use 5.005;
use Module::Build; # needs 0.19_05;

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

my $class = Module::Build->subclass( code => <<'EOC' );

use File::Basename;
use File::Spec;

sub ACTION_test
{
    my $self = shift;

    $self->notes( test_data => { apache_dir => $self->{args}{apache}{apache_dir},
                                 port       => $self->{args}{apache}{port},
                                 is_maintainer => -d 'CVS' ? 1 : 0,
                               } );

    $self->SUPER::ACTION_test;
}

sub ACTION_install
{
    my $self = shift;

    $self->SUPER::ACTION_install;
    $self->depends_on('delete_old_pods');
    $self->depends_on('configure_apache');

    $self->run_perl_script('install/delete_old_pods.pl');  # These could probably be separate actions.
    $self->run_perl_script('install/configure_apache.pl');
}

sub ACTION_delete_old_pods
{
    my $self = shift;

    foreach my $dir (@INC) {
        foreach my $pm ( qw( Interp ApacheHandler Request Component ) ) {
            my $pod_file = File::Spec->catfile( $dir, 'HTML', 'Mason', "$pm.pod" );

	    if ( -e $pod_file ) {
		warn "Removing obsolete documentation file $pod_file\n";
		unlink $pod_file or warn "Cannot unlink $pod_file: $!";
	    }
	}
    }
}

sub ACTION_configure_apache
{
    my $self = shift;

    my $params_file = File::Spec->catfile( $self->build_dir, 'apache_install.txt' );

    return unless -e $params_file;

    my %params = $self->_read_apache_config_params();
    my $mason_conf = $self->_write_mason_conf(%params);
    $self->_alter_httpd_conf( mason_config_file => $mason_conf, %params );
}

sub _read_apache_config_params
{
    my $self = shift;

    local *INST;
    open INST, "<$params_file";
    my $install;
    eval join '', <INST>;
    die $@ if $@;

    return %$install;
}

sub _write_mason_conf
{
    my $self = shift;
    my %params = @_;

    my $conf = <<"EOF";
PerlSetVar MasonCompRoot "$params{comp_root}"
PerlSetVar MasonDataDir  "$params{data_dir}"
PerlModule HTML::Mason::ApacheHandler

<Directory "$params{comp_root}">
EOF

    if ( $params{extensions} )
    {
	my $ext_re = '(';
	$ext_re .= join '|', map { "\\.$_" } @{ $params{extensions} };
	$ext_re .= ')$';

	$conf .= qq|  <LocationMatch "$ext_re">\n|;
    }

    $conf .= <<"EOF";
    SetHandler perl-script
    PerlHandler HTML::Mason::ApacheHandler
EOF
    $conf .= "  </LocationMatch>\n" if $params{extensions};

    $conf .= "</Directory>\n";

    my $conf_dir = dirname( $params{apache_config_file} );
    my $conf_file = File::Spec->catfile( $conf_dir, 'mason.conf' );

    local *CONF;
    open CONF, ">$conf_file" or die "Cannot write $conf_file: $!";
    print CONF $conf;
    close CONF or die "Can't close $conf_file: $!";

    return $conf_file;
}

sub _alter_httpd_conf
{
    my $self = shift;
    my %params = @_;

    local *CONF;
    open CONF, "<$params{apache_config_file}"
	or die "Can't read $params{apache_config_file}: $!";

    my $new = '';
    while (<CONF>)
    {
	if ( /^# Mason config/ )
	{
	    my $skip = <CONF>; # just eat another line
	    next;
	}
	$new .= $_;
    }
    # clear off last two newlines otherwise file will add one extra
    # blank line every time this script runs
    chomp $new;
    chomp $new;
    $new .= "\n\n# Mason config\nInclude $params{mason_config_file}\n";

    close CONF or die "Can't close $params{apache_config_file}: $!";

    open CONF, ">$params{apache_config_file}"
	or die "Can't write to $params{apache_config_file}: $!";
    print CONF $new
	or die "Can't write to $params{apache_config_file}: $!";
    close CONF or die "Can't close $params{apache_config_file}: $!";
}

EOC

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

my $build = $class->new
  (
   module_name => 'HTML::Mason',
   requires    => \%prereq,
   build_requires => {
		      'Module::Build' => '0.19_05',
		      'Test'          => 0,
		     },
   args => { apache => \%APACHE,
           },
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
