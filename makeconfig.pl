use strict;

my $hiresWarnMsg = <<EOF;
==> Mason needs version 1.19, or later, of Time::HiRes in order to record
microsecond time values in the system log. Since you do not seem to
have this module, system log times will be recorded in seconds only.
If you do decide to obtain Time::HiRes, run Makefile.PL again or edit
Config.pm.
EOF

my $successMsg = <<EOF;
Edit lib/HTML/Mason/Config.pm to read about these settings and change
them if desired.  When you run "make install" this file will be
installed alongside the other Mason libraries.
EOF
    
my $confFile = <<EOF;
# Copyright (c) 1998,1999 by Jonathan Swartz. All rights reserved.
# This program is free software; you can redistribute it and/or modify it
# under the same terms as Perl itself.

# This is the global configuration file for HTML::Mason.

\%HTML::Mason::Config = (
    # Do we have the XS version of Data::Dumper?
    #
    'use_data_dumper_xs'      => %d,

    # Determines whether to use Time::HiRes to record microsecond time
    # values in the system log. If this is 0, times will be recorded
    # in seconds only.  Typically this should be 1 if and only if
    # Time::HiRes is available.
    #
    'use_time_hires'          => %d,
);
EOF

sub have_pkg
{
    my ($pkg) = @_;
    eval { my $p; ($p = $pkg . ".pm") =~ s|::|/|g; require $p; };
    no strict 'refs';
    return ${"${pkg}::VERSION"} ? 1 : 0;
}

sub chk_version
{
    my($pkg,$wanted,$msg) = @_;

    local($|) = 1;
    print "Checking for $pkg...";

    eval { my $p; ($p = $pkg . ".pm") =~ s#::#/#g; require $p; };

    my $vstr;
    my $vnum;
    {
	no strict 'refs';
	$vstr = ${"${pkg}::VERSION"} ? "found v" . ${"${pkg}::VERSION"}	: "not found";
	$vnum = ${"${pkg}::VERSION"} || 0;
    }

    print $vnum >= $wanted ? "ok\n" : " " . $vstr . "\n";

    $vnum >= $wanted;
}

sub make_config
{
    print "-"x20 . "\nCreating Mason configuration file.\n";
    print "Checking for existing configuration...";
    eval {require 'HTML/Mason/Config.pm'; };
    my $err = $@;
    print (($err) ? "not found." : (!defined(%HTML::Mason::Config)) ? "old-style Config.pm found." : "found.");
    print "\n";
    my %c = %HTML::Mason::Config;

    if (!defined($c{use_data_dumper_xs})) {
	print "Checking for Data::Dumper->Dumpxs...";
	eval {
	    require Data::Dumper;
	    my $d = new Data::Dumper([[1,2,3]]);
	    $d->Dumpxs;
	};
	if ($@) {
	    print "not found.\n";
	    $c{use_data_dumper_xs} = 0;
	} else {
	    print "found.\n";
	    $c{use_data_dumper_xs} = 1;
	}
    }

    if (!defined($c{use_time_hires})) {
	print "\n";
	my $h = chk_version('Time::HiRes' => '1.19');
	print $hiresWarnMsg if !$h;
	$c{use_time_hires} = $h;
    }

    open(F,">lib/HTML/Mason/Config.pm") or die "\nERROR: Cannot write lib/HTML/Mason/Config.pm. Check directory permissions and rerun.\n";
    my $conf = sprintf($confFile,@c{qw(default_cache_tie_class mldbm_file_ext mldbm_use_db mldbm_serializer use_data_dumper_xs use_time_hires)});
    print F $conf;
    print "\nWriting lib/HTML/Mason/Config.pm.\n";
    close(F);

    print "\nYour settings are:\n";
    print join("\n",grep(/=>/,split("\n",$conf)))."\n\n";
    print $successMsg,"-"x20,"\n";
}

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

    eval { require Apache::Request; };
    $APACHE{has_apache_request} = $@ ? 0 : 1;

    cleanup_files();

    write_apache_conf();
    setup_handler('CGI');
    setup_handler('mod_perl') if $APACHE{has_apache_request};
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
    my $conf_file = $APACHE{conf_file} ? "$cwd/$APACHE{conf_file}" : "$cwd/t/httpd.conf";
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
  PerlRequire $APACHE{apache_dir}/mason_handler_CGI.pl
  SetHandler  perl-script
  PerlHandler HTML::Mason
</IfDefine>

<IfDefine CGI_no_handler>
  PerlSetVar  MasonCompRoot "$APACHE{comp_root}"
  PerlSetVar  MasonDataDir  "$APACHE{data_dir}"
EOF

    if ($mod_perl::VERSION >= 1.24) {
	$include .= <<'EOF';
  PerlAddVar  MasonAllowedGlobals $foo
  PerlAddVar  MasonAllowedGlobals @bar
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
  PerlSetVar MasonMultipleConfig 1

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

use HTML::Mason::ApacheHandler ( args_method => '$args_method' );
use HTML::Mason;

my \$interp = HTML::Mason::Interp->new( comp_root => '$APACHE{comp_root}',
				       data_dir => '$APACHE{data_dir}' );
chown Apache->server->uid, Apache->server->gid, \$interp->files_written;

my \@ah = ( HTML::Mason::ApacheHandler->new( interp => \$interp,
                                            output_mode => 'batch' ),
           HTML::Mason::ApacheHandler->new( interp => \$interp,
                                            output_mode => 'stream' ),
	   HTML::Mason::ApacheHandler->new( interp => \$interp,
					    top_level_predicate => sub { \$_[0] =~ m,/_.*, ? 0 : 1 },
                                            output_mode => 'batch' ),
	   HTML::Mason::ApacheHandler->new( interp => \$interp,
                                            decline_dirs => 0,
                                            output_mode => 'batch' ),
	   HTML::Mason::ApacheHandler->new( interp => \$interp,
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
