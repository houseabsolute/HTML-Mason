# Copyright (c) 1998 by Jonathan Swartz. All rights reserved.
# This program is free software; you can redistribute it and/or modify it
# under the same terms as Perl itself.

#
# Miscellaneous tools used by the other Mason modules.  Some of these
# admittedly exist in better versions on CPAN but we rewrite them so
# as to minimize external package requirements.
#

package HTML::Mason::Tools;
require 5.004;
require Exporter;
@ISA = qw(Exporter);
@EXPORT = qw();
@EXPORT_OK = qw(read_file chop_slash html_escape url_escape url_unescape date_delta_to_secs make_absolute_path pkg_loaded pkg_installed);

use strict;
use IO::File qw(!/^SEEK/);
use Cwd;

#
# Return contents of file.
#
sub read_file
{
    my ($file) = @_;
    die "read_file: '$file' does not exist" if (!-e $file);
    die "read_file: '$file' is a directory" if (-d _);
    my $fh = new IO::File $file;
    die "read_file: could not open file '$file' for reading\n" if !$fh;
    binmode $fh;
    local $/ = undef;
    my $text = <$fh>;
    return $text;
}

#
# Remove final slash from string, if any; return resulting string.
#
sub chop_slash
{
    my ($str) = (@_);
    $str =~ s@/$@@;
    return $str;
}

#
# Escape HTML &, >, <, and " characters. Borrowed from CGI::Base.
#
sub html_escape
{
    my ($text) = @_;
    my %html_escape = ('&' => '&amp;', '>'=>'&gt;', '<'=>'&lt;', '"'=>'&quot;');
    my $html_escape = join('', keys %html_escape);
    $text =~ s/([$html_escape])/$html_escape{$1}/mgoe;
    return $text;
}

#
# Unescape URL-encoded data. Borrowed from CGI.
#
sub url_unescape {
    my $todecode = shift;
    return undef unless defined($todecode);
    $todecode =~ tr/+/ /;       # pluses become spaces
    $todecode =~ s/%([0-9a-fA-F]{2})/pack("c",hex($1))/ge;
    return $todecode;
}

#
# URL-encode data. Borrowed from CGI.
#
sub url_escape {
    my $toencode = shift;
    return undef unless defined($toencode);
    $toencode=~s/([^a-zA-Z0-9_.-])/uc sprintf("%%%02x",ord($1))/eg;
    return $toencode;
}

#
# Convert a "date delta string" (e.g. 1sec, 3min, 2h) to a number of
# seconds. Based on Date::Manip date delta concept.
#
my %dateDeltaHash = ('y'=>31557600, yr=>31557600, year=>31557600, years=>31557600,
		     'm'=>2592000, mon=>2592000, month=>2592000, months=>2592000,
		     'w'=>604800, wk=>604800, ws=>604800, wks=>604800, week=>604800, weeks=>604800,
		     'd'=>86400, day=>86400, days=>86400,
		     'h'=>3600, hr=>3600, hour=>3600, hours=>3600,
		     mn=>60, min=>60, minute=>60, minutes=>60,
		     's'=>1, sec=>1, second=>1, seconds=>1
		     );
sub date_delta_to_secs
{
    my ($delta) = @_;
    my $usage = "date_delta_to_secs: invalid argument '$delta'";
    my ($num,$unit,$sign);
    if ($delta =~ /^([-+]?)\s*([0-9]+)\s*([a-zA-Z]*)\s*$/) {
	($sign,$num,$unit) = ($1,$2,lc($3));
    } else {
	die $usage;
    }
    $unit = "s" if !$unit;
    my $mult = $dateDeltaHash{$unit};
    die $usage if !$mult;
    return $num * $mult * ($sign eq '-' ? -1 : 1);
}

#
# Return an absolute version of a pathname.  No change if already absolute.
#
sub make_absolute_path
{
    my ($path) = @_;
    # filenames beginning with / or a drive letter (e.g. C:/) are absolute
    unless ($path =~ /^([A-Za-z]:)?\//) {
	$path = cwd() . $path;
    }
    return $path;
}


no strict 'refs';

#
# Determine if package is installed without loading it, by checking
# the INC path.
#
sub pkg_installed
{
    my ($pkg) = @_;

    (my $pkgfile = "$pkg.pm") =~ s{::}{/}g;
    return grep(-f "$_/$pkgfile",@INC);
}

#
# Determined if package is loaded by checking for its version.
#
sub pkg_loaded
{
    my ($pkg) = @_;

    my $varname = "${pkg}::VERSION";
    return $$varname ? 1 : 0;
}

