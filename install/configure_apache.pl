use strict;

use File::Basename;
use File::Spec;

my $params_file = './apache_install.txt';
# main
{
    exit unless -e $params_file;

    my %params = read_params();
    my $mason_conf = write_mason_conf(%params);
    alter_httpd_conf( mason_config_file => $mason_conf, %params );
}

sub read_params
{
    local *INST;
    open INST, "<$params_file";
    my $install;
    eval join '', <INST>;
    die $@ if $@;

    return %$install;
}

sub write_mason_conf
{
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

	$conf .= qq|  <FilesMatch "$ext_re">\n|;
    }

    $conf .= <<"EOF";
    SetHandler perl-script
    PerlHandler HTML::Mason::ApacheHandler
EOF
    $conf .= "  </FilesMatch>\n" if $params{extensions};

    $conf .= "</Directory>\n";

    my $conf_dir = dirname( $params{apache_config_file} );
    my $conf_file = File::Spec->catfile( $conf_dir, 'mason.conf' );

    local *CONF;
    open CONF, ">$conf_file" or die "Cannot write $conf_file: $!";
    print CONF $conf;
    close CONF or die "Can't close $conf_file: $!";

    return $conf_file;
}

sub alter_httpd_conf
{
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
