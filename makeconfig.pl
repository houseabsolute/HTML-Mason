use strict;

my $dbmWarnMsg = <<EOF;
==> WARNING: the DBM file format chosen for your system, %s, is
inadequate for data caching due to size limitations. If you intend to
use data caching we strongly recommend installing Berkley DB (DB_File)
or GNU DBM (GDBM_File), which have no such limitations.
EOF

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
    # Default cached tie class. Change this to tie cache files to
    # something other than MLDBM.  Normally this should be left alone.
    #
    'default_cache_tie_class' => '%s',

    # Automatic file extension used for the DBM format specified
    # below.  For example, this is '' for DB_File and GDBM, '.db'
    # for NDBM, and '.pag' for SDBM. Mason needs to know this so it
    # can stat arbitrary DBM files.
    #
    'mldbm_file_ext'          => '%s',
    
    # The DBM format used by MLDBM. Ideally this will be one of
    # DB_File or GDBM_File. The other formats (SDBM, ODBM, NDBM) are
    # inadequate for data caching purposes due to size limitations.
    #
    'mldbm_use_db'            => '%s',
	
    # The serializer used by MLDBM. Currently can be set to one of
    # Data::Dumper, Storable, or FreezeThaw.
    #
    'mldbm_serializer'        => '%s',

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
    
    $c{default_cache_tie_class} ||= 'MLDBM';

    my $val;
    if (!defined($c{mldbm_use_db})) {
	if (defined($MLDBM::UseDB) && $MLDBM::UseDB !~ /^SDBM|ODBM|NDBM/) {
	    $val = $MLDBM::UseDB;
	} else {
	    print "\nSearching for DBM packages...";
	    foreach (qw(GDBM_File DB_File NDBM_File SDBM_File ODBM_File)) {
		if (have_pkg($_)) {
		    print "found $_.\n";
		    $val = $_;
		    last;
		}
	    }
	    if (!$val) {
		print "failed! Assuming SDBM.\n";
		$val = 'SDBM_File';
	    }
	}
	if ($val =~ /^(SDBM|ODBM|NDBM)_File$/) {
	    printf ("\n$dbmWarnMsg",$val);
	}
	$c{mldbm_use_db} = $val;
    }
    $c{mldbm_file_ext} = '';
    $c{mldbm_file_ext} ||= {NDBM_File=>'.db', SDBM_File=>'.pag'}->{$c{mldbm_use_db}};
    
    if (!defined($c{mldbm_serializer})) {
	$c{mldbm_serializer} = $MLDBM::Serializer || 'Data::Dumper';
    }

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
	my $h = chk_version( 'Time::HiRes' => '1.19');
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

use Apache::test;
use Cwd;
use File::Path;
use File::Spec;

use vars qw($HAS_APACHE_REQUEST $CONF_DIR $COMP_ROOT $DATA_DIR %APACHE_PARAMS);
$CONF_DIR = '';
$DATA_DIR = '';

sub setup_mod_perl_tests
{
    # Skip if no mod_perl
    eval { require mod_perl };
    # need to use it twice to avoid annoying warning
    return unless $mod_perl::VERSION || $mod_perl::VERSION;

    $HAS_APACHE_REQUEST = 1;
    eval { require Apache::Request; };
    $HAS_APACHE_REQUEST = 0 if $@;

    cleanup_files();

    write_apache_conf();
    setup_handler('CGI');
    write_test_comps();

    setup_handler('mod_perl') if $HAS_APACHE_REQUEST;
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
    %APACHE_PARAMS = Apache::test->get_test_params();

    my $conf_file = $APACHE_PARAMS{conf_file} || 't/httpd.conf';
    $CONF_DIR = ( File::Spec->splitpath($conf_file) )[1];
    $CONF_DIR =~ s,/$,,;

    my $cwd = cwd();
    $COMP_ROOT = "$cwd/$CONF_DIR/comps";
    $DATA_DIR = "$cwd/$CONF_DIR/data";

    mkdir $COMP_ROOT, 0755
	or die "Can't make dir '$COMP_ROOT': $!";
    mkdir $DATA_DIR, 0755
	or die "Can't make dir '$COMP_ROOT': $!";

    my $include = <<"EOF";

<IfDefine CGI>
  PerlRequire $CONF_DIR/mason_handler_CGI.pl

  <Location /mason>
    SetHandler perl-script
    PerlHandler HTML::Mason
  </Location>

  <Location /mason_stream>
    SetHandler perl-script
    PerlHandler HTML::Mason
  </Location>
</IfDefine>
EOF

    $include .= <<"EOF"

<IfDefine mod_perl>
  PerlRequire $CONF_DIR/mason_handler_mod_perl.pl

  <Location /mason>
    SetHandler perl-script
    PerlHandler HTML::Mason
  </Location>

  <Location /mason_stream>
    SetHandler perl-script
    PerlHandler HTML::Mason
  </Location>
</IfDefine>
EOF
	if $HAS_APACHE_REQUEST;

    local $^W;
    Apache::test->write_httpd_conf
	    ( %APACHE_PARAMS,
	      include => $include
	    );
}

sub setup_handler
{
    my $args_method = shift;

    my $handler = "mason_handler_$args_method.pl";
    my $handler_file = "$CONF_DIR/$handler";
    open F, ">$handler_file"
	or die "Can't write to '$handler_file': $!";
    print F <<"EOF";
package HTML::Mason;

use HTML::Mason::ApacheHandler ( args_method => '$args_method' );
use HTML::Mason;

my \$parser = HTML::Mason::Parser->new;
my \$interp = HTML::Mason::Interp->new( parser => \$parser,
					comp_root => '$COMP_ROOT',
					data_dir => '$DATA_DIR' );

my \$stream_ah = HTML::Mason::ApacheHandler->new( interp => \$interp,
                                                  output_mode => 'stream' );
my \$batch_ah = HTML::Mason::ApacheHandler->new( interp => \$interp,
                                                 output_mode => 'batch' );

sub handler
{
    my \$r = shift;
    \$r->header_out('X-Mason-Test' => 'Initial value');

    my \$ah = \$r->uri =~ /mason_stream/ ? \$stream_ah : \$batch_ah;

    # strip off stuff just used to figure out what handler to use.
    my \$filename = \$r->filename;
    \$filename =~ s,/mason(?:_stream)?\$,,;

    \$filename .= \$r->path_info;
    \$filename =~ s,//+,/,g;

    \$r->filename(\$filename);

    my \$status = \$ah->handle_request(\$r);
    \$r->print( "Status code: \$status\\n" );
}
EOF
    close F;
}

sub write_test_comps
{
    write_comp( 'basic', <<'EOF',
Basic test.
2 + 2 = <% 2 + 2 %>.
uri = <% $r->uri =~ /basic$/ ? '/basic' : $r->uri %>.
method = <% $r->method %>.


EOF
	      );

    write_comp( 'headers', <<'EOF',


% $r->header_out('X-Mason-Test' => 'New value 2');
Blah blah
blah
% $r->header_out('X-Mason-Test' => 'New value 3');
<%init>
$r->header_out('X-Mason-Test' => 'New value 1');
$m->abort if $blank;
</%init>
<%args>
$blank=>0
</%args>
EOF
	      );

    write_comp( 'cgi_object', <<'EOF',
<% $m->cgi_object->isa('CGI') ? 'CGI' : 'NO CGI' %>
EOF
	      );
}

sub write_comp
{
    my $name = shift;
    my $comp = shift;

    my $file = "$COMP_ROOT/$name";
    open F, ">$file"
	or die "Can't write to '$file': $!";

    print F $comp;

    close F;
}



1;
