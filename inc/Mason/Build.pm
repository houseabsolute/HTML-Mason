# sneakily introduce new namespace ;)
package Mason::Build;

use strict;

use Module::Build 0.20;

use base 'Module::Build';

use lib 't/lib';

use Apache::test;
use ExtUtils::Manifest ();
use File::Basename ();
use File::Path ();
use File::Spec;

sub create_build_script
{
    my $self = shift;

    $self->_check_for_old_mason;

    unless ( exists $self->{args}{noprompts} )
    {
	$self->_apache_test_config;

	$self->_assisted_install_config;
    }

    $self->add_to_cleanup('mason_tests');

    $self->SUPER::create_build_script(@_);

    return $self;
}

sub _check_for_old_mason
{
    my $self = shift;

    eval { require HTML::Mason };

    # no Mason installed
    return if $@;

    if ( $HTML::Mason::VERSION < 1.09 )
    {
	print <<"EOF";

It looks like you have an older version of Mason already installed on
your machine (version $HTML::Mason::VERSION).  This version is not backwards
compatible with versions of Mason before version 1.09_01.
Please read the UPGRADE document before continuing with this
installation.

EOF

	unless ( exists $self->{args}{noprompts} )
	{
	    my $yn = Module::Build->prompt('Continue with installation?', 'N');

	    exit unless $yn =~ /y(?:es)?/i;
	}
    }
}

sub _apache_test_config
{
    my $self = shift;

    return unless $self->_is_maintainer;

    return if $^O =~ /win32/i;

    eval { require mod_perl; };
    return if $@;

    $self->_cleanup_apache_test_files();

    $self->_write_apache_test_conf()
	or return;

    $self->_setup_handler('mod_perl');
    $self->_setup_handler('CGI');
    $self->_write_CGIHandler();
}

sub _is_maintainer
{
    return $ENV{MASON_MAINTAINER} if exists $ENV{MASON_MAINTAINER};

    return -d 'CVS' ? 1 : 0;
}

sub _cleanup_apache_test_files
{
    my $self = shift;

    foreach ( qw( httpd httpd.conf mason_handler_CGI.pl mason_handler_mod_perl.pl ) )
    {
	my $file = File::Spec->catdir( 't', $_ );
	if ( -e $file )
	{
	    unlink $file
		or die "Can't unlink '$file': $!";
	}
    }

    foreach ( qw( comps data ) )
    {
	my $dir = File::Spec->catdir( 't', $_ );
	if ( -d $dir )
	{
	    $self->delete_filetree($dir);
	}
    }
}

sub _write_apache_test_conf
{
    my $self = shift;

    my %conf = Apache::test->get_test_params();

    return unless keys %conf;

    my $conf_file = File::Spec->catfile( $self->base_dir, 't', 'httpd.conf' );
    $conf{apache_dir} = File::Basename::dirname($conf_file);
    $conf{apache_dir} =~ s,/$,,;

    $conf{comp_root} = File::Spec->catdir( $conf{apache_dir}, 'comps' );
    $conf{data_dir} = File::Spec->catdir( $conf{apache_dir}, 'data' );

    mkdir $conf{comp_root}, 0755
	or die "Can't make dir '$conf{comp_root}': $!";
    mkdir $conf{data_dir}, 0755
	or die "Can't make dir '$conf{data_dir}': $!";

    $self->add_to_cleanup( @conf{'comp_root', 'data_dir'} );

    my $libs = $self->_apache_test_conf_libs();

    my $cgi_handler =
        File::Spec->catfile( $conf{apache_dir}, 'mason_handler_CGI.pl' );
    my $mod_perl_handler =
        File::Spec->catfile( $conf{apache_dir}, 'mason_handler_mod_perl.pl' );

    my %multiconf;
    $multiconf{1}{comp_root} = File::Spec->catfile( $conf{comp_root}, 'multiconf1' );
    $multiconf{1}{data_dir}  = File::Spec->catfile( $conf{data_dir}, 'multiconf1' );
    $multiconf{2}{comp_root} = File::Spec->catfile( $conf{comp_root}, 'multiconf2' );
    $multiconf{2}{data_dir}  = File::Spec->catfile( $conf{data_dir}, 'multiconf2' );

    my $include .= <<"EOF";
ServerRoot $conf{apache_dir}

# tainting has to be turned on before any Perl code is loaded
<IfDefine taint>
  PerlSetEnv PATH /bin:/usr/bin
  PerlTaintCheck On
</IfDefine>

<Perl>
 $libs
</Perl>

<IfDefine CGI>
  PerlModule  CGI
  PerlRequire $cgi_handler
  SetHandler  perl-script
  PerlHandler HTML::Mason
</IfDefine>

<IfDefine CGI_no_handler>
  PerlModule  CGI
  PerlSetVar  MasonCompRoot "$conf{comp_root}"
  PerlSetVar  MasonDataDir  "$conf{data_dir}"

  PerlAddVar  MasonAllowGlobals \$foo
  PerlAddVar  MasonAllowGlobals \@bar

  PerlSetVar  MasonArgsMethod CGI
  SetHandler  perl-script
  PerlModule  HTML::Mason::ApacheHandler
  PerlHandler HTML::Mason::ApacheHandler
</IfDefine>

<IfDefine mod_perl>
  PerlRequire $mod_perl_handler
  SetHandler  perl-script
  PerlHandler HTML::Mason
</IfDefine>

<IfDefine mod_perl_no_handler>
  PerlSetVar  MasonArgsMethod mod_perl
  PerlSetVar  MasonCompRoot "root => $conf{comp_root}"
  PerlAddVar  MasonCompRoot "root2 => $conf{data_dir}"
  PerlSetVar  MasonDataDir  "$conf{data_dir}"
  PerlSetVar  MasonDeclineDirs 0
  # We need to test setting a "code" type parameter
  PerlSetVar  MasonPreprocess "sub { \${\$_[0]} =~ s/fooquux/FOOQUUX/ }"

  PerlSetVar  MasonEscapeFlags "old_h  => \\&HTML::Mason::Escapes::basic_html_escape"
  PerlAddVar  MasonEscapeFlags "old_h2 => basic_html_escape"
  PerlAddVar  MasonEscapeFlags "uc => sub { \${\$_[0]} = uc \${\$_[0]}; }"

  PerlSetVar  MasonDataCacheDefaults "cache_class => MemoryCache"
  PerlAddVar  MasonDataCacheDefaults "namespace => foo"

  SetHandler  perl-script
  PerlModule  HTML::Mason::ApacheHandler
  PerlHandler HTML::Mason::ApacheHandler
</IfDefine>

<IfDefine multi_config>
  PerlSetVar MasonArgsMethod CGI

  <Location /comps/multiconf1>
    PerlSetVar  MasonCompRoot "$multiconf{1}{comp_root}"
    PerlSetVar  MasonDataDir  "$multiconf{1}{data_dir}"
    PerlSetVar  MasonAutohandlerName no_such_file
    SetHandler  perl-script
    PerlModule  HTML::Mason::ApacheHandler
    PerlHandler HTML::Mason::ApacheHandler
  </Location>

  <Location /comps/multiconf2>
    PerlSetVar  MasonCompRoot "$multiconf{2}{comp_root}"
    PerlSetVar  MasonDataDir  "$multiconf{2}{data_dir}"
    PerlSetVar  MasonDhandlerName no_such_file
    SetHandler  perl-script
    PerlModule  HTML::Mason::ApacheHandler
    PerlHandler HTML::Mason::ApacheHandler
  </Location>

</IfDefine>

<IfDefine no_config>
  SetHandler  perl-script
  PerlHandler HTML::Mason::ApacheHandler
</IfDefine>

<IfDefine single_level_serverroot>
  ServerRoot /tmp
  SetHandler perl-script
  PerlSetVar MasonDataDir /tmp/one/two
  PerlHandler HTML::Mason::ApacheHandler
</IfDefine>

<IfDefine taint>
  SetHandler  perl-script
  PerlHandler HTML::Mason::ApacheHandler
</IfDefine>

<IfDefine CGIHandler>
  AddHandler cgi-script .cgi
  Action html-mason /CGIHandler.cgi
  <Location /comps>
    Options +ExecCGI
    SetHandler html-mason
  </Location>
</IfDefine>
EOF

    if ( load_pkg('Apache::Filter') )
    {
        my $filter_handler = <<'EOF';
  sub FilterTest::handler
  {
      my $r = shift;

      $r = $r->filter_register;

      my ($fh, $status) = $r->filter_input;

      return $status unless $status == Apache::Constants::OK();

      print uc while <$fh>;

      return $status;
  }
EOF

        $include .= <<"EOF";
<IfDefine filter_tests>
  PerlModule  Apache::Constants
  <Perl>
$filter_handler
  </Perl>

  PerlSetVar  MasonArgsMethod mod_perl
  PerlSetVar  MasonCompRoot "root => $conf{comp_root}"
  PerlSetVar  MasonDataDir  "$conf{data_dir}"
  PerlModule  Apache::Filter;
  PerlSetVar  Filter  On

  SetHandler  perl-script
  PerlModule  HTML::Mason::ApacheHandler
  PerlHandler HTML::Mason::ApacheHandler FilterTest
</IfDefine>
EOF
    } # matches 'if ( load_pkg('Apache::Filter') )'

    {
	local $^W;
	Apache::test->write_httpd_conf
	    ( %conf,
	      include => $include
	    );
    }

    $self->add_to_cleanup
	( map { File::Spec->catfile( $conf{apache_dir}, $_ ) }
	  qw( httpd.conf error_log httpd httpd.pid mason )
	);

    $self->notes( apache_test_conf => \%conf );

    return 1;
}

sub _setup_handler
{
    my $self = shift;
    my $args_method = shift;

    my $conf = $self->notes('apache_test_conf');

    my $handler = "mason_handler_$args_method.pl";
    my $handler_file = File::Spec->catfile( $conf->{apache_dir}, $handler );
    open F, ">$handler_file"
	or die "Can't write to '$handler_file': $!";

    my $libs = $self->_apache_test_conf_libs();

    # The code below tries to create its configurations using
    # different combinations of parameters.  The goal is to have
    # different combinations of providing contained objects and
    # providing the contained object class and its parameters.
    print F <<"EOF";
package My::Resolver;
\$My::Resolver::VERSION = '0.01';
\@My::Resolver::ISA = 'HTML::Mason::Resolver::File::ApacheHandler';


package My::Interp;
\$My::Interp::VERSION = '0.01';
\@My::Interp::ISA = 'HTML::Mason::Interp';

package HTML::Mason;

$libs

use Apache::Constants qw(REDIRECT);

use HTML::Mason::ApacheHandler;
use HTML::Mason;

my \@ah_params = ( {},
                   {},
                   { decline_dirs => 0 },
                   {}
                 );

my \@interp_params = ( {},
                       { autoflush => 1 },
                       {},
                       { error_mode => 'fatal', error_format => 'line' },
                     );

my \@ah;
for (my \$x = 0; \$x <= \$#ah_params; \$x++)
{
    my \%res_params;

    if ( \$x < 2 )
    {
        \%res_params = ( resolver_class => 'My::Resolver',
                        comp_root => '$conf->{comp_root}',
                      );
    }
    else
    {
        \%res_params =
            ( resolver =>
              My::Resolver->new( comp_root => '$conf->{comp_root}' )
            );
    }

    my \%interp_params;
    if ( \$x % 2 )
    {

        \%interp_params = ( interp_class => 'My::Interp',
                           data_dir => '$conf->{data_dir}',
                           error_mode => 'output',
                           error_format => 'html',
                           \%{\$interp_params[\$x]},
                         );
    }
    else
    {
        \%interp_params =
            ( interp =>
              My::Interp->new( request_class => 'HTML::Mason::Request::ApacheHandler',
                               data_dir => '$conf->{data_dir}',
                               error_mode => 'output',
                               error_format => 'html',
                               %res_params,
                              \%{\$interp_params[\$x]},
                             )
            );

        \%res_params = ();
    }

    my \$ah =
        HTML::Mason::ApacheHandler->new
            ( args_method => '$args_method',
              \%{\$ah_params[\$x]},
              \%interp_params,
	      \%res_params,
            );

    chown Apache->server->uid, Apache->server->gid, \$ah->interp->files_written;

    push \@ah, \$ah;
}

sub handler
{
    my \$r = shift;
    \$r->header_out('X-Mason-Test' => 'Initial value');

    my (\$ah_index) = \$r->uri =~ /ah=(\\d+)/;

    unless (\$ah[\$ah_index])
    {
        \$r->print( "No ApacheHandler object at index #\$ah_index" );
        warn "No ApacheHandler object at index #\$ah_index\n";
        return;
    }

    # strip off stuff just used to figure out what handler to use.
    my \$filename = \$r->filename;
    \$filename =~ s,/ah=\\d+,,;
    \$filename .= \$r->path_info;
    \$filename =~ s,//+,/,g;
    \$r->filename(\$filename);

    my \$status = \$ah[\$ah_index]->handle_request(\$r);
    return \$status if \$status == REDIRECT;
    \$r->print( "Status code: \$status\\n" );
}

1;
EOF
    close F;

    $self->add_to_cleanup($handler_file);
}

sub _write_CGIHandler
{
    my $self = shift;

    my $conf = $self->notes('apache_test_conf');

    my $handler_file = File::Spec->catfile( $conf->{apache_dir}, 'CGIHandler.cgi' );
    open F, ">$handler_file"
	or die "Can't write to '$handler_file': $!";

    my $libs = $self->_apache_test_conf_libs();

    my $data_dir = File::Spec->catdir( $conf->{apache_dir}, 'data' );

    use Config;

    print F <<"EOF";
$Config{startperl}

$libs

use HTML::Mason::CGIHandler;

my \%p;
if ( \$ENV{PATH_INFO} =~ s,/autoflush\$,, )
{
    \%p = ( autoflush => 1 );
}

my \$h = HTML::Mason::CGIHandler->new( data_dir  => '$data_dir', \%p );

if ( \$ENV{PATH_INFO} =~ s,/handle_comp\$,, )
{
    \$h->handle_comp( \$ENV{PATH_INFO} );
}
elsif ( \$ENV{PATH_INFO} =~ s,/handle_cgi_object\$,, )
{
    my \$cgi = CGI->new;
    \$cgi->param( 'foo' => 'bar' );
    \$h->handle_cgi_object( \$cgi );
}
else
{
    \$h->handle_request;
}
EOF

    close F;

    chmod 0755, $handler_file
	or die "cannot chmod $handler_file to 0755: $!";

    $self->add_to_cleanup($handler_file);
}

sub _apache_test_conf_libs
{
    my $self = shift;

    my $libs = 'use lib qw( ';
    $libs .= join ' ', ( File::Spec->catdir( $self->base_dir, 'blib', 'lib' ),
                         File::Spec->catdir( $self->base_dir, 't', 'lib' ) );

    if ($ENV{PERL5LIB})
    {
	$libs .= ' ';
	$libs .= join ' ', (split /:|;/, $ENV{PERL5LIB});
    }
    $libs .= ' );';

    return $libs;
}

sub _assisted_install_config
{
    my $self = shift;

    return if $self->_is_maintainer;

    my $conf = $self->notes('apache_test_conf');

    unless ( $conf->{httpd} )
    {
	my %conf = Apache::test->get_test_params();
	$conf = \%conf;
    }

    return unless $conf->{httpd};

    my %httpd_params = Apache::test->get_compilation_params( $conf->{httpd} );

    my $conf_file =
	( $conf->{config_file} ?
	  $conf->{config_file} :
	  $httpd_params{SERVER_CONFIG_FILE}
	);

    my %config_params = eval { $self->_get_config_file_params($conf_file) };
    warn " * Can't investigate current installation status:\n $@" and return if $@;

    foreach my $k ( qw( document_root user group ) )
    {
	# strip quotes if they're there.
	for ( $config_params{$k}) { s/^"//; s/"$//; }
    }

    my $conf_dir = File::Basename::dirname( $conf_file );

    my $has_mason_string = $config_params{has_mason} ? 'does' : 'does not';
    print <<"EOF";

It is possible to have this program automatically set up a simple
Mason configuration.  This would involve altering the configuration
file at $conf_file.

It appears that this configuration file $has_mason_string have
previous Mason configuration directives.

EOF

    my $default = $config_params{has_mason} ? 'no' : 'yes';
    $default = 'no' if -e File::Spec->catfile( $conf_dir, 'mason.conf' );

    my $yn =
	Module::Build->prompt
	    ( 'Would you like help configuring Apache/mod_perl to use Mason?',
	      $default );

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
	$install{comp_root} =
	    Module::Build->prompt( 'Component root?', $config_params{document_root} );
    } until $install{comp_root};

    print <<'EOF';

Mason needs to know where it should store data files that it
generated.  This includes compiled components, cache files, and other
miscellania that Mason generates.  This directory will be made
readable and writable by the user the web server runs as.

EOF

    do
    {
	$install{data_dir} =
	    Module::Build->prompt( 'Data directory?',
				   File::Spec->catdir( $httpd_params{HTTPD_ROOT}, 'mason' ) );

	if ($install{data_dir} && -e $install{data_dir})
	{
	    my $yn =
		Module::Build->prompt
		    ( "This directory ('$install{data_dir}') already exists," .
		      " is that ok?", 'yes' );

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
	my $ext =
	    Module::Build->prompt
		( 'What extensions should the web server' .
		  ' recognize as Mason components', 'html' );

	@ext = map { s/^\.//; $_ } split /\s+/, $ext;

	unless (@ext == 1 && $ext[0] eq '!')
	{
	    $install{extensions} = \@ext;
	}
    } until @ext;

    $self->notes( apache_install => \%install );
}

sub _get_config_file_params
{
    my $self = shift;
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

sub ACTION_build
{
    my $self = shift;

    $self->depends_on('params_pod');

    $self->SUPER::ACTION_build(@_);

    # This has to be done to the blib files or else if we run this
    # from our local repositories we end up modifying those files.
    $self->_convert_custom_pod('blib');
}

# trick ApacheHandler into not dying
sub Apache::perl_hook { 1 }
sub Apache::server { 0 }

use strict;

sub ACTION_params_pod
{
    my $self = shift;

    my $params_pod =
	File::Spec->catfile( $self->_lib_dir, 'HTML', 'Mason', 'Params.pod' );

    my $comp = File::Spec->catfile( $self->base_dir, 'inc', 'params.mtxt' );

    return if $self->up_to_date( [ $comp,
				   $self->_files_with_pod('lib') ],
				 $params_pod
			       );

    # make sure we get distro's modules
    local @INC = ( $self->_lib_dir, @INC );

    require Data::Dumper;

    eval
    {
	require HTML::Mason;
	require HTML::Mason::Compiler::ToObject;
	require HTML::Mason::ApacheHandler;
	require HTML::Mason::Tools;
    };

    if ($@)
    {
	warn "Cannot load Mason modules: $@\n";
	warn "Skipping generation of HTML::Mason::Params document\n";
	return;
    }

    my @params = $self->_find_params;
    my $pod = $self->_run_params_comp(@params);

    my $fh = HTML::Mason::Tools::make_fh();
    open $fh, ">$params_pod"
	or die "Cannot write to $params_pod";
    print $fh $pod
	or die "Cannot write to $params_pod";
    close $fh;

    $self->add_to_cleanup($params_pod);
}

sub _find_params
{
    my $self = shift;

    my %specs = Class::Container->all_specs;
    my %params;
    my %pod;

    foreach my $class_file ( $self->_files_with_params )
    {
	my $pod_text = HTML::Mason::Tools::read_file($class_file);

	while ($pod_text =~ /=item ([a-z_]+)\n\n(.*?)\n(?==item [a-z_]+\n|=back\s+=head)/sg)
	{
	    my ($name, $desc) = ($1, $2);
	    next if exists($pod{$name});
	    chomp($pod{$name} = $desc);
	}
    }

    foreach my $class ( sort keys %specs )
    {
	foreach my $name ( sort keys %{ $specs{$class}{valid_params} } )
	{
	    my $param = $specs{$class}{valid_params}{$name};
	    next unless $param->{public};
	    next if $param->{type} eq 'object';

	    $param->{name} = $name;
	    $param->{class} = $class;
	    $param->{default} = 'Varies' if $name =~ /^(?:comp_root|error_format|error_mode)$/;
	    $param->{default} = 'Print to STDOUT' if $name eq 'out_method';
	    $param->{pod} = $pod{$name} or die "could not find pod entry for $name\n";
	    $params{$name} = $param;
	}

	foreach my $obj ( sort keys %{ $specs{$class}{contained_objects} } )
	{
	    my $name = $obj . '_class';
	    my $default = $specs{$class}{contained_objects}{$obj}{class};
	    my $is_delayed = $specs{$class}{contained_objects}{$obj}{delayed};
	    $params{$name} = {
		name => $name,
		type => 'string',
		class => $class,
		default => $default,
		is_delayed => $is_delayed,
		pod => $pod{$name},
		public => 1,
	    };
	}
    }

    foreach my $spec (sort values %params)
    {
	(my $studly = $spec->{name}) =~ s/(?:^|_)(\w)/\U$1/g;
	$spec->{apache_name} =
	    HTML::Mason::ApacheHandler->studly_form( $spec->{name} );
    }

    return map { $params{$_} } sort keys %params;
}

sub _lib_dir
{
    my $self = shift;

    my $lib_dir = File::Spec->catdir( $self->base_dir, 'lib' );
}

sub _files_with_params
{
    my $self = shift;

    my @files;
    foreach my $class ( qw( ApacheHandler Compiler Compiler/ToObject
                            Interp Request Resolver/File ) )
    {
	my @class_pieces = split /\//, $class;
	$class_pieces[-1] .= '.pm';

	push @files,
	    File::Spec->catfile( $self->_lib_dir, 'HTML', 'Mason', @class_pieces );
    }

    return @files;
}

sub _run_params_comp
{
    my $self = shift;
    my @params = @_;

    my $buf;
    my $interp = HTML::Mason::Interp->new( out_method => \$buf );

    $interp->exec( '/inc/params.mtxt',
		   params => \@params,
		   pods => [ $self->_files_with_pod('lib') ],
		 );

    $buf =~ s/\s+$//s;

    return $buf;
}

sub _files_with_pod
{
    my $self = shift;
    my $dir = shift;

    return
	( grep { $self->contains_pod($_) }
	  @{ $self->rscan_dir( $dir, qr{\.pm$} ) },
	  @{ $self->rscan_dir( $dir, qr{\.pod$} ) }
	);
}

sub _convert_custom_pod
{
    my $self = shift;
    my $dir = shift;

    print "Converting custom POD tags in files under $dir\n";

    foreach my $file ( $self->_files_with_pod($dir) )
    {
	$self->_convert_pod_in_file($file);
    }
}

sub _convert_pod_in_file
{
    my $self = shift;
    my $file = shift;

    my $fh = do { local *FH; *FH; };

    open $fh, "<$file" or die "Cannot read $file: $!";
    local $_ = join '', <$fh>;
    close $fh;

    # WARNING - these regexes are run over entire .pm and .pod files -
    # be careful!

    # Convert custom P<> tags to appropriate params link
    s{P<([a-z_]+)>}
     {L<$1|HTML::Mason::Params/$1>}g;

    # Convert custom DEVEL<> tags to appropriate developer's manual link
    s{DEVEL<([\w \n]+)>}
     {the L<$1|HTML::Mason::Devel/$1> section of the developer\'s manual}mg;

    # Convert custom ADMIN<> tags to appropriate administrator's manual link
    s{ADMIN<([\w \n]+)>}
     {the L<$1|HTML::Mason::Admin/$1> section of the administrator\'s manual}mg;

    $self->_make_writeable($file);

    open $fh, ">$file" or die "Cannot write to $file: $!";
    print $fh $_;
    close $fh;
}

sub _make_writeable
{
    my $self = shift;
    my $file = shift;

    unless ( -w $file )
    {
	my $mode = (stat $file)[2];

	# let user & group write the darn thing
	$mode |= 0220;

	chmod $mode, $file
	    or die "Can't make $file writeable: $!";
    }
}

sub ACTION_dist
{
    my $self = shift;

    unless ( defined &ExtUtils::Manifest::maniadd )
    {
	warn <<'EOF';

The dist action requires a recent version of ExtUtils::Manifest.
Please upgrade your installed version of the ExtUtils::MakeMaker
distribution.

EOF
	exit;
    }

    $self->depends_on('params_pod');
    $self->depends_on('manifest');
    $self->depends_on('distdir');

    $self->_convert_custom_pod( File::Spec->catdir( $self->dist_dir, 'lib' ) );

    my @files = $self->_generate_html_docs( $self->dist_dir );

    my $dist_manifest =	File::Spec->catfile( $self->dist_dir, 'MANIFEST' );

    # Nice use of undocumented globals.  I hate the EU::* code!
    local $ExtUtils::Manifest::MANIFEST = $dist_manifest;

    $self->_make_writeable($dist_manifest);

    ExtUtils::Manifest::maniadd( { map { $_ => '' } @files } );

    $self->_cleanup_changes_file;

    my $dist_dir = $self->dist_dir;

    $self->make_tarball($dist_dir);
    $self->delete_filetree($dist_dir);
}

sub ACTION_manifest
{
    my $self = shift;

    $self->SUPER::ACTION_manifest(@_);

    # We always generate MANIFEST when making dist
    $self->add_to_cleanup('MANIFEST');
}

sub ACTION_html_docs
{
    my $self = shift;

    $self->depends_on('build');

    $self->_generate_html_docs( File::Spec->catdir( $self->base_dir, 'blib' ),
				File::Spec->catdir( $self->base_dir ),
			      );
}

sub _generate_html_docs
{
    my $self = shift;
    my $dir = shift;
    my $target_dir = shift || $dir;

    require File::Temp;

    # should use something less sucky
    require Pod::Html;

    my $html_dir = File::Spec->catdir( $target_dir, 'htdocs' );

    my @files;
    foreach my $file ( $self->_files_with_pod( File::Spec->catdir( $dir, 'lib' ) ) )
    {
	my $html_file = $self->_pod_from_html( $file, $html_dir );

	my $rel_path = File::Spec->abs2rel( $html_file, $target_dir );

	push @files, $rel_path;
    }

    $self->_check_html_doc_links($html_dir);

    $self->add_to_cleanup( map { File::Spec->catfile( $self->base_dir, $_ ) }
			   'pod2htmd.x~~', 'pod2htmi.x~~' );

    return @files;
}

sub _pod_from_html
{
    my $self = shift;
    my ( $pod_file, $out_dir ) = @_;

    die "could not find $pod_file" unless -f $pod_file;

    # Determine html filename - will break if run on non-Unix
    my ($base) = ($pod_file =~ m/\/HTML\/(.*)\.(?:pm|pod)/);
    return unless $base;

    $base =~ s{Mason/}{};
    my $html_file = "$out_dir/$base.html";

    # Convert to html with pod2html
    print "$pod_file => $html_file\n";
    my ($rawfh, $raw_html_file) = File::Temp::tempfile();
    Pod::Html::pod2html("--infile=$pod_file", "--outfile=$raw_html_file");

    # Fix braindead pod links
    File::Path::mkpath( File::Basename::dirname($html_file) );

    my $htmlfh = do { local *FH; *FH };
    open $htmlfh, ">$html_file" or die "cannot write to $html_file: $!";

    while (<$rawfh>) {
	my $base_dir = File::Basename::dirname($base);
	if ($base_dir eq '.') {
	    s|HREF="/HTML/Mason/([^\"]+)"|HREF="$1"|gi;
	} else {
	    s|HREF="/HTML/Mason/([^\"]+)"|HREF="../$1"|gi;
	    s|HREF="/HTML/Mason.html"|HREF="../Mason.html"|gi;
	    s|HREF="$base_dir/([^\"]+)"|HREF="$1"|gi;
	}
	s|HREF="/HTML/Mason.html"|HREF="Mason.html"|gi;
	s/A HREF="([^\"\#]*)\#([^\"]+)"/"A HREF=\"$1\#" . $self->_escape_link($2) . "\""/gie;
	s/A NAME="([^\"]+)"/"A NAME=\"" . $self->_escape_link($1) . "\""/gie;
	print $htmlfh $_  or die "cannot write to $html_file: $!";
    }

    unlink $raw_html_file
	or die "Cannot unlink $raw_html_file: $!";

    $self->add_to_cleanup($html_file);

    return $html_file;
}

# This is how pod2html used to escape its links. It may not be
# necessary anymore, but then again some browsers may choke on the
# full unfettered link, and our swish_index depends on links looking
# like this.
sub _escape_link
{
    my $self = shift;

    my ($link) = @_;
    for ($link) { s/^\s+//; s/\s+$// }
    $link = substr($link, 0, 32);
    $link =~ s/\W+/_/g;
    return $link;
}

sub _check_html_doc_links
{
    my $self = shift;
    my $html_dir = shift;

    print "running linklint\n";
    my $lfh = do { local *FH; *FH };

    # magic incantation
    open $lfh, "linklint -limit 100000 -error -root $html_dir -xref /@ 2>&1 |"
        or die "Cannot open pipe to linklint: $!";

    my $output = do { local $/; <$lfh> };

    if (my ($error_log) = ($output =~ /(\#-+\n\# ERROR.*)/s))
    {
	print $error_log;
	die "linklint had errors";
    }
}

sub _cleanup_changes_file
{
    my $self = shift;

    # Read contents of Changes
    my $changes = File::Spec->catfile( $self->dist_dir, 'Changes' );

    my $fh = do { local *FH; *FH };
    open $fh, $changes or die "could not read $changes: $!";
    my $buf = do { local $/ = undef; <$fh> };
    close $fh;

    # Look for release numbers with no date, as we forget this often
    if (my ($relnum) = ($buf =~ /^(\d\.[\.\d]+)\s*\n/m))
    {
        warn "found release number with no date: $relnum\n";
    }

    # Find first release number
    my $i = index($buf, "1.");

    # Remove L{} documentation links; these are only for web site
    $buf =~ s/\s*L\{([^\{\}]+)\}\s*/\n/g;

    # Indent lines beginning with "- " and "[ [A-Z]" with four spaces
    $buf =~ s/^(\- |\[ [A-Z])/    $1/mg;

    # Indent everything else with six spaces
    substr($buf,$i) =~ s/^(\S)/      $1/mg;

    # Don't indent release titles at all
    $buf =~ s/\n\n\s+(\d\.[\.\d]+\s*)/\n\n$1/mg;

    $self->_make_writeable($changes);

    # Write out changed Changes
    open $fh, ">$changes" or die "could not write $changes: $!";
    print $fh $buf or die "could not write $changes: $!";
    close $fh;
}

sub ACTION_test
{
    my $self = shift;

    my $conf = $self->notes('apache_test_conf');

    $self->notes( test_data => { apache_dir => $conf->{apache_dir},
                                 port       => $conf->{port},
                                 is_maintainer => $self->_is_maintainer,
                               } );

    $self->SUPER::ACTION_test;
}

sub ACTION_install
{
    my $self = shift;

    $self->SUPER::ACTION_install;
    $self->depends_on('delete_old_pods');
    $self->depends_on('configure_apache');
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

    my $install = $self->notes('apache_install');

    return unless $install;

    my $mason_conf = $self->_write_mason_conf($install);
    $self->_alter_httpd_conf( { mason_config_file => $mason_conf, %$install } );
}

sub _write_mason_conf
{
    my $self = shift;
    my $params = shift;

    my $conf = <<"EOF";
PerlSetVar MasonCompRoot "$params->{comp_root}"
PerlSetVar MasonDataDir  "$params->{data_dir}"
PerlModule HTML::Mason::ApacheHandler

<Directory "$params->{comp_root}">
EOF

    if ( $params->{extensions} )
    {
	my $ext_re = '(';
	$ext_re .= join '|', map { "\\.$_" } @{ $params->{extensions} };
	$ext_re .= ')$';

	$conf .= qq|  <LocationMatch "$ext_re">\n|;
    }

    $conf .= <<"EOF";
    SetHandler perl-script
    PerlHandler HTML::Mason::ApacheHandler
EOF
    $conf .= "  </LocationMatch>\n" if $params->{extensions};

    $conf .= "</Directory>\n";

    my $conf_dir = File::Basename::dirname( $params->{apache_config_file} );
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
    my $params = shift;

    local *CONF;
    open CONF, "<$params->{apache_config_file}"
	or die "Can't read $params->{apache_config_file}: $!";

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
    $new .= "\n\n# Mason config\nInclude $params->{mason_config_file}\n";

    close CONF or die "Can't close $params->{apache_config_file}: $!";

    open CONF, ">$params->{apache_config_file}"
	or die "Can't write to $params->{apache_config_file}: $!";
    print CONF $new
	or die "Can't write to $params->{apache_config_file}: $!";
    close CONF or die "Can't close $params->{apache_config_file}: $!";
}

# Copied from HTML::Mason::Tools
sub load_pkg {
    my ($pkg, $nf_error) = @_;

    my $file = File::Spec->catfile( split /::/, $pkg );
    $file .= '.pm';
    return 1 if exists $INC{$file};

    eval "use $pkg";

    if ($@) {
	if ($@ =~ /^Can\'t locate .* in \@INC/) {
	    if (defined($nf_error)) {
		die sprintf("Can't locate %s in \@INC. %s\n(\@INC contains: %s)",
			    $pkg, $nf_error, "@INC");
	    } else {
		undef $@;
		return 0;
	    }
	} else {
	    die $@;
	}
    }
    return 1;
}

1;

__END__
