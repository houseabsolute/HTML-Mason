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
# Copyright (c) 1998-2001 by Jonathan Swartz. All rights reserved.
# This program is free software; you can redistribute it and/or modify
# it under the same terms as Perl itself.

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

1;
