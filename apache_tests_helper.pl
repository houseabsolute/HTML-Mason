use strict;

use lib 'lib', 't/lib';

use Cwd;
use File::Path;
use File::Basename;

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
}

sub cleanup_files
{
    foreach ( qw( httpd httpd.conf mason_handler_CGI.pl mason_handler_mod_perl.pl ) )
    {
	my $file = "t/$_";
	if ( -e $file )
	{
	    unlink $file
		or die "Can't unlink '$file': $!";
	}
    }

    foreach ( qw( comps data ) )
    {
	my $dir = "t/$_";
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
    my $conf_file = "$cwd/t/httpd.conf";
    $APACHE{apache_dir} = dirname($conf_file);
    $APACHE{apache_dir} =~ s,/$,,;

    $APACHE{comp_root} = "$APACHE{apache_dir}/comps";
    $APACHE{data_dir} = "$APACHE{apache_dir}/data";

    mkdir $APACHE{comp_root}, 0755
	or die "Can't make dir '$APACHE{comp_root}': $!";
    mkdir $APACHE{data_dir}, 0755
	or die "Can't make dir '$APACHE{comp_root}': $!";

    my $libs = _libs();

    my $include .= <<"EOF";

<Perl>
 $libs
</Perl>

<IfDefine CGI>
  PerlModule  CGI
  PerlRequire $APACHE{apache_dir}/mason_handler_CGI.pl
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
  PerlRequire $APACHE{apache_dir}/mason_handler_mod_perl.pl
  SetHandler  perl-script
  PerlHandler HTML::Mason
</IfDefine>

<IfDefine mod_perl_no_handler>
  PerlSetVar  MasonArgsMethod mod_perl
  PerlSetVar  MasonCompRoot "root => $APACHE{comp_root}"
  PerlSetVar  MasonDataDir  "$APACHE{data_dir}"
  PerlSetVar  MasonTopLevelPredicate "sub { \$_[0] !~ m(/__[^/]+\$) }"
  PerlSetVar  MasonDeclineDirs 0
  SetHandler  perl-script
  PerlModule  HTML::Mason::ApacheHandler
  PerlHandler HTML::Mason::ApacheHandler
</IfDefine>

<IfDefine multi_config>
  PerlSetVar MasonArgsMethod CGI

  <Location /comps/multiconf1>
    PerlSetVar  MasonCompRoot "$APACHE{comp_root}/multiconf1"
    PerlSetVar  MasonDataDir  "$APACHE{data_dir}/multiconf1"
    PerlSetVar  MasonUseAutohandlers 0
    SetHandler  perl-script
    PerlModule  HTML::Mason::ApacheHandler
    PerlHandler HTML::Mason::ApacheHandler
  </Location>

  <Location /comps/multiconf2>
    PerlSetVar  MasonCompRoot "$APACHE{comp_root}/multiconf2"
    PerlSetVar  MasonDataDir  "$APACHE{data_dir}/multiconf2"
    PerlSetVar  MasonUseDhandlers 0
    SetHandler  perl-script
    PerlModule  HTML::Mason::ApacheHandler
    PerlHandler HTML::Mason::ApacheHandler
  </Location>

</IfDefine>

EOF

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
    my $handler_file = "$APACHE{apache_dir}/$handler";
    open F, ">$handler_file"
	or die "Can't write to '$handler_file': $!";

    my $libs = _libs();

    print F <<"EOF";
package HTML::Mason;

$libs

use HTML::Mason::ApacheHandler;
use HTML::Mason;

my \$interp = HTML::Mason::Interp->new( comp_root => '$APACHE{comp_root}',
				       data_dir => '$APACHE{data_dir}' );
chown Apache->server->uid, Apache->server->gid, \$interp->files_written;

my \@ah = ( HTML::Mason::ApacheHandler->new( interp => \$interp,
                                            args_method => '$args_method',
                                            output_mode => 'batch' ),
           HTML::Mason::ApacheHandler->new( interp => \$interp,
                                            args_method => '$args_method',
                                            output_mode => 'stream' ),
	   HTML::Mason::ApacheHandler->new( interp => \$interp,
                                            args_method => '$args_method',
					    top_level_predicate => sub { \$_[0] =~ m,/_.*, ? 0 : 1 },
                                            output_mode => 'batch' ),
	   HTML::Mason::ApacheHandler->new( interp => \$interp,
                                            args_method => '$args_method',
                                            decline_dirs => 0,
                                            output_mode => 'batch' ),
	   HTML::Mason::ApacheHandler->new( interp => \$interp,
                                            args_method => '$args_method',
                                            error_mode => 'fatal',
                                            output_mode => 'batch' ),
	 );

sub handler
{
    my \$r = shift;
    \$r->header_out('X-Mason-Test' => 'Initial value');

    my (\$ah_index) = \$r->uri =~ /ah=(\\d+)/;

    unless (\$ah[\$ah_index])
    {
        \$r->print( "No ApacheHandler object at index #\$ah_index" );
        return;
    }

    # strip off stuff just used to figure out what handler to use.
    my \$filename = \$r->filename;
    \$filename =~ s,/ah=\\d+,,;
    \$filename .= \$r->path_info;
    \$filename =~ s,//+,/,g;
    \$r->filename(\$filename);

    my \$status = \$ah[\$ah_index]->handle_request(\$r);
    \$r->print( "Status code: \$status\\n" );
}
EOF
    close F;
}

sub _libs
{
    my $cwd = cwd();
    my $libs = 'use lib qw( ';
    $libs .= join ' ', "$cwd/blib/lib", "$cwd/t/lib";
    if ($ENV{PERL5LIB})
    {
	$libs .= ' ';
	$libs .= join ' ', (split /:|;/, $ENV{PERL5LIB});
    }
    $libs .= ' );';

    return $libs;
}

1;
