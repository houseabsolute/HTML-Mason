# Copyright (c) 1998-2001 by Jonathan Swartz. All rights reserved.
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
use File::Spec;

require Exporter;

use vars qw(@ISA @EXPORT_OK);

@ISA = qw(Exporter);
@EXPORT_OK = qw(read_file html_escape url_escape paths_eq compress_path mason_canonpath make_fh taint_is_on);

#
# Return contents of file. If $binmode is 1, read in binary mode.
#
sub read_file
{
    my ($file,$binmode) = @_;
    HTML::Mason::Exception->throw( error => "read_file: '$file' does not exist" )
	unless -e $file;
    HTML::Mason::Exception->throw( error => "read_file: '$file' is a directory" ) if (-d _);
    my $fh = make_fh();
    open $fh, $file
	or HTML::Mason::Exception::System->throw( error => "read_file: could not open file '$file' for reading: $!" );
    binmode $fh if $binmode;
    local $/ = undef;
    my $text = <$fh>;
    close $fh;
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
# Determines whether two paths are equal, taking into account
# case-insensitivity in Windows O/S.
#
sub paths_eq {
    return File::Spec->case_tolerant ? (lc($_[0]) eq lc($_[1])) : $_[0] eq $_[1];
}

#
# Compress a component path into a single, filesystem-friendly
# string. Uses URL-like escaping with + instead of %.
#
sub compress_path
{
    my ($path) = @_;
    for ($path) {
	s@^/@@;
	s/([^\w\.\-\~])/sprintf('+%02x', ord $1)/eg;
    }
    return $path;
}

#
# Makes a few fixes to File::Spec::canonpath. Will go away if/when they
# accept our patch.
#
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
	$path =~ s|/[^/]+/\.\.$|| && redo;           # /xx/..    -> /
	$path =~ s|[^/]+/\.\./|| && redo;            # /xx/../yy -> /yy
    }
    return $path;
}

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
    no strict 'refs';
    return $$varname ? 1 : 0;
}

#
# Determine if taint mode is on.
#
sub taint_is_on
{
    not eval { "$0$^X" && kill 0; 1 };
}

sub make_fh
{
    return undef if $] >= 5.6;  # Let filehandles autovivify
    return do { local *FH; *FH; };  # double *FH avoids a warning
}

#
# Process escape flags in <% %> tags
#   h - html escape
#   u - url escape
#
my %html_escape = ('&' => '&amp;', '>'=>'&gt;', '<'=>'&lt;', '"'=>'&quot;');
sub escape_perl_expression
{
    my ($expr,@flags) = @_;

    return $expr if grep { $_ eq 'n' } @flags;

    if (defined($expr)) {
	foreach my $flag (@flags) {
	    if ($flag eq 'h') {
		$expr =~ s/([<>&\"])/$html_escape{$1}/mgoe;
	    } elsif ($flag eq 'u') {
		$expr =~ s/([^a-zA-Z0-9_.-])/uc sprintf("%%%02x",ord($1))/eg;
	    }
	}
    }
    return $expr;
}


1;

