# Copyright (c) 1998-2000 by Jonathan Swartz. All rights reserved.
# This program is free software; you can redistribute it and/or modify it
# under the same terms as Perl itself.

#
# Miscellaneous tools used by the other Mason modules.  Some of these
# admittedly exist in better versions on CPAN but we rewrite them so
# as to minimize external package requirements.
#

package HTML::Mason::Tools;

use strict;

use Cwd;

require Exporter;

use vars qw(@ISA @EXPORT_OK);

@ISA = qw(Exporter);
@EXPORT_OK = qw(read_file html_escape date_delta_to_secs dumper_method paths_eq compress_path pkg_loaded pkg_installed is_taint_on);

#
# Return contents of file. If $binmode is 1, read in binary mode.
#
sub read_file
{
    my ($file,$binmode) = @_;
    die "read_file: '$file' does not exist" if (!-e $file);
    die "read_file: '$file' is a directory" if (-d _);
    my $fh = do { local *FH; *FH; };
    open $fh, $file
	or die "read_file: could not open file '$file' for reading: $!";
    binmode $fh if $binmode;
    local $/ = undef;
    my $text = <$fh>;
    return $text;
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
# Call the XS or normal version of Data::Dumper::Dump depending on what's installed.
#
sub dumper_method {
    my ($d) = @_;
    return ($HTML::Mason::Config{use_data_dumper_xs} ? $d->Dumpxs : $d->Dump);
}

#
# Determines whether two paths are equal, taking into account
# case-insensitivity in Windows O/S.
#
sub paths_eq {
    return File::Spec->case_tolerant ? (lc($_[0]) eq lc($_[1])) : $_[0] eq $_[1];
}

sub compress_path
{
    my ($path) = @_;
    for ($path) {
	s@^/@@;
	s/([^\w\.\-\~])/sprintf('+%02x', ord $1)/eg;
    }
    return $path;
}

sub mason_canonpath {
    # Just like File::Spec::canonpath, but we're having trouble
    # getting a patch through to them.
    shift;
    my $path = shift;
    $path =~ s|/+|/|g unless($^O eq 'cygwin');       # xx////yy  -> xx/yy
    $path =~ s|(/\.)+/|/|g;                          # xx/././yy -> xx/yy
    {
	$path =~ s|^(\./)+||s unless $path eq "./";  # ./xx      -> xx
	$path =~ s|^/(\.\./)+|/|s;                   # /../../xx -> xx
	$path =~ s|/\Z(?!\n)|| unless $path eq "/";  # xx/       -> xx
	$path =~ s|[^/]+/\.\./|| && redo;            # /xx/../yy -> /yy
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

#
# Determine if taint mode is on.
#
sub is_taint_on
{
    not eval { "$0$^X" && kill 0; 1 };
}

1;
