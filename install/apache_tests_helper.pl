use strict;

use lib 'lib', 't/lib';

use Cwd;
use File::Path;
use File::Basename;
use File::Spec;

# Don't load HTML::Mason::* modules here, because in Makefile.PL we
# might not yet have the proper prerequisites installed.

use vars qw(%APACHE);

sub setup_mod_perl_tests
{
    return if $^O =~ /win32/i;

    # Skip if no mod_perl
    eval { require mod_perl; };
    return if $@;

    require Apache::test;

    cleanup_files();

    write_apache_conf();
    setup_handler('mod_perl');
    setup_handler('CGI');
    write_CGIHandler();
}

sub cleanup_files
{
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
	    rmtree( $dir, $ENV{MASON_DEBUG} );
	}
    }
}

sub write_apache_conf
{
    my %p = Apache::test->get_test_params();
    while (my ($k, $v) = each %p)
    {
	$APACHE{$k} = $v;
    }

    my $cwd = cwd();
    my $conf_file = File::Spec->catfile( $cwd, 't', 'httpd.conf' );
    $APACHE{apache_dir} = dirname($conf_file);
    $APACHE{apache_dir} =~ s,/$,,;

    $APACHE{comp_root} = File::Spec->catdir( $APACHE{apache_dir}, 'comps' );
    $APACHE{data_dir} = File::Spec->catdir( $APACHE{apache_dir}, 'data' );

    mkdir $APACHE{comp_root}, 0755
	or die "Can't make dir '$APACHE{comp_root}': $!";
    mkdir $APACHE{data_dir}, 0755
	or die "Can't make dir '$APACHE{data_dir}': $!";

    my $libs = _libs();

    my $cgi_handler =
        File::Spec->catfile( $APACHE{apache_dir}, 'mason_handler_CGI.pl' );
    my $mod_perl_handler =
        File::Spec->catfile( $APACHE{apache_dir}, 'mason_handler_mod_perl.pl' );

    my %multiconf;
    $multiconf{1}{comp_root} = File::Spec->catfile( $APACHE{comp_root}, 'multiconf1' );
    $multiconf{1}{data_dir}  = File::Spec->catfile( $APACHE{data_dir}, 'multiconf1' );
    $multiconf{2}{comp_root} = File::Spec->catfile( $APACHE{comp_root}, 'multiconf2' );
    $multiconf{2}{data_dir}  = File::Spec->catfile( $APACHE{data_dir}, 'multiconf2' );

    my $include .= <<"EOF";
ServerRoot $APACHE{apache_dir}

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
  PerlSetVar  MasonCompRoot "$APACHE{comp_root}"
  PerlSetVar  MasonDataDir  "$APACHE{data_dir}"
EOF

    if ($mod_perl::VERSION >= 1.24) {
	$include .= <<'EOF';
  PerlAddVar  MasonAllowGlobals $foo
  PerlAddVar  MasonAllowGlobals @bar
EOF
    }

    $include .= <<"EOF";
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
  PerlSetVar  MasonCompRoot "root => $APACHE{comp_root}"
  PerlAddVar  MasonCompRoot "root2 => $APACHE{data_dir}"
  PerlSetVar  MasonDataDir  "$APACHE{data_dir}"
  PerlSetVar  MasonDeclineDirs 0

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
  PerlSetVar  MasonCompRoot "root => $APACHE{comp_root}"
  PerlSetVar  MasonDataDir  "$APACHE{data_dir}"
  PerlModule  Apache::Filter;
  PerlSetVar  Filter  On

  SetHandler  perl-script
  PerlModule  HTML::Mason::ApacheHandler
  PerlHandler HTML::Mason::ApacheHandler FilterTest
</IfDefine>
EOF
    } # matches 'if ( load_pkg('Apache::Filter') )'

    local $^W;
    Apache::test->write_httpd_conf
	    ( %APACHE,
	      include => $include
	    );
}

sub setup_handler
{
    my $args_method = shift;

    my $handler = "mason_handler_$args_method.pl";
    my $handler_file = File::Spec->catfile( $APACHE{apache_dir}, $handler );
    open F, ">$handler_file"
	or die "Can't write to '$handler_file': $!";

    my $libs = _libs();

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
    my \$res = My::Resolver->new( comp_root => '$APACHE{comp_root}' );

    my \$ah =
        HTML::Mason::ApacheHandler->new
            ( args_method => '$args_method',
              interp_class => 'My::Interp',
	      request_class => 'HTML::Mason::Request::ApacheHandler',
	      resolver => \$res,
	      error_mode => 'output',
	      error_format => 'html',
	      data_dir => '$APACHE{data_dir}',
              \%{\$ah_params[\$x]},
	      \%{\$interp_params[\$x]},
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
}

sub write_CGIHandler
{
    my $handler = "CGIHandler.cgi";
    my $handler_file = File::Spec->catfile( $APACHE{apache_dir}, $handler );
    open F, ">$handler_file"
	or die "Can't write to '$handler_file': $!";

    my $libs = _libs();

    my $data_dir = File::Spec->catdir( $APACHE{apache_dir}, 'data' );

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
}

sub _libs
{
    my $cwd = cwd();
    my $libs = 'use lib qw( ';
    $libs .= join ' ', ( File::Spec->catdir( $cwd, 'blib', 'lib' ),
                         File::Spec->catdir( $cwd, 't', 'lib' ) );
    if ($ENV{PERL5LIB})
    {
	$libs .= ' ';
	$libs .= join ' ', (split /:|;/, $ENV{PERL5LIB});
    }
    $libs .= ' );';

    return $libs;
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
