use strict;

use lib 't/lib';

use Apache::test;
use Data::Dumper;
use Module::Build;
use File::Basename;
use File::Spec;

sub assisted_install_config
{
    return unless $APACHE{httpd};

    my %httpd_params = Apache::test->get_compilation_params( $APACHE{httpd} );

    my $conf_file = $APACHE{config_file} ? $APACHE{config_file} : $httpd_params{SERVER_CONFIG_FILE};
    my %config_params = eval { _get_config_file_params($conf_file) };
    warn " * Can't investigate current installation status:\n $@" and return if $@;

    foreach my $k ( qw( document_root user group ) )
    {
	# strip quotes if they're there.
	for ( $config_params{$k}) { s/^"//; s/"$//; }
    }

    my $default = $config_params{has_mason} ? 'no' : 'yes';

    my $conf_dir = dirname( $conf_file );
    $default = 'no' if -e File::Spec->catfile( $conf_dir, 'mason.conf' );

    print <<"EOF";

It is possible to have this program automatically set up a simple
Mason configuration.  This would involve altering the configuration
file at $conf_file.

EOF

    print "It appears that this configuration file does " . ($config_params{has_mason} ? '' : 'not ' ) . "have previous Mason configuration directives.\n\n";

    my $yn = Module::Build->prompt( 'Would you like help configuring Apache/mod_perl to use Mason?', $default );

    return unless $yn =~ /^y/i;

    my %install = ( user => $config_params{user},
		    group => $config_params{group},
		    apache_config_file => $conf_file,
		  );

    print <<'EOF';

Mason needs to know what your component root should be.  This is a
directory in which Mason can expect to find components.  Generally,
when starting out with Mason this will either be your server's
document root or a subdirectory below it.

If this directory does not exist it will be created.

EOF

    do
    {
	$install{comp_root} = Module::Build->prompt( 'Component root?', $config_params{document_root} );
    } until $install{comp_root};

    print <<'EOF';

Mason needs to know where it should store data files that it
generated.  This includes compiled components, cache files, and other
miscellania that Mason generates.  This directory will be made
readable and writable by the user the web server runs as.

EOF

    do
    {
	$install{data_dir} = Module::Build->prompt( 'Data directory?',
				     File::Spec->catdir( $httpd_params{HTTPD_ROOT}, 'mason' ) );
	if ($install{data_dir} && -e $install{data_dir})
	{
	    my $yn = Module::Build->prompt( "This directory ('$install{data_dir}') already exists, is that ok?", 'yes' );
	    delete $install{data_dir} unless $yn =~ /y/;
	    print "\n";
	}
    } until $install{data_dir};

    print <<'EOF';

It is often desirable to tell the web server to only recognize certain
extensions as Mason components.  This allows you to easily put HTML,
images, etc. and Mason components all together under the document root
without worrying that Mason will try to serve static content.

Enter a list of extensions separated by spaces.  Periods are not
needed.

If you want all files under the document root to be treated as Mason
components simply enter '!' here.

EOF

    my @ext;
    do
    {
	my $ext = Module::Build->prompt( 'What extensions should the web server recognize as Mason components', 'html' );
	@ext = map { s/^\.//; $_ } split /\s+/, $ext;

	unless (@ext == 1 && $ext[0] eq '!')
	{
	    $install{extensions} = \@ext;
	}
    } until @ext;

    local *INST;
    open INST, ">./apache_install.txt" or die "Can't write to ./apache_install.txt: $!";
    print INST Data::Dumper->Dump([\%install], ['install']);
    close INST or die "Can't close ./apache_install.txt: $!";
}

sub _get_config_file_params
{
    my $file = shift;

    local *CONF;

    open CONF, "<$file"	or die "Can't read $file: $!\n";

    my %conf;
    while (<CONF>)
    {
	next if /^\s*\#/; # skip comments

	# all regexes below attempt to make sure that they're not in a
	# comment

	if ( /[^\#]*HTML::Mason/ )
	{
	    $conf{has_mason} = 1;
	}

	if ( /[^\#]*DocumentRoot\s+(.*)/ )
	{
	    $conf{document_root} = $1;
	}

	if ( /[^\#]*User\s+(.*)/ )
	{
	    $conf{user} = $1;
	}

	if ( /[^\#]*Group\s+(.*)/ )
	{
	    $conf{group} = $1;
	}
    }

    close CONF or die "Can't close $file: $!";

    return %conf;
}


1;
