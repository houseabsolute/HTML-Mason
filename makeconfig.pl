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

 my $vstr = ${"${pkg}::VERSION"} ? "found v" . ${"${pkg}::VERSION"}
				 : "not found";
 my $vnum = ${"${pkg}::VERSION"} || 0;

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

    if (!defined($c{use_time_hires})) {
	print "\n";
	my $h = chk_version(Time::HiRes => '1.19');
	print $hiresWarnMsg if !$h;
	$c{use_time_hires} = $h;
    }

    open(F,">lib/HTML/Mason/Config.pm") or die "\nERROR: Cannot write lib/HTML/Mason/Config.pm. Check directory permissions and rerun.\n";
    my $conf = sprintf($confFile,@c{qw(default_cache_tie_class mldbm_file_ext mldbm_use_db mldbm_serializer use_time_hires)});
    print F $conf;
    print "\nWriting lib/HTML/Mason/Config.pm.\n";
    close(F);

    print "\nYour settings are:\n";
    print join("\n",grep(/=>/,split("\n",$conf)))."\n\n";
    print $successMsg,"-"x20,"\n";
}
    
1;
