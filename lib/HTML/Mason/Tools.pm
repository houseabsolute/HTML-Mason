# Copyright (c) 1998-2002 by Jonathan Swartz. All rights reserved.
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
use HTML::Mason::Exceptions( abbr => [qw(system_error param_error error)] );

require Exporter;

use vars qw(@ISA @EXPORT_OK);

@ISA = qw(Exporter);
@EXPORT_OK = qw(read_file url_escape paths_eq compress_path mason_canonpath make_fh taint_is_on load_pkg pkg_loaded absolute_comp_path);

#
# Return contents of file. If $binmode is 1, read in binary mode.
#
sub read_file
{
    my ($file,$binmode) = @_;
    error "read_file: '$file' does not exist" unless -e $file;
    error "read_file: '$file' is a directory" if (-d _);
    my $fh = make_fh();
    open $fh, "< $file"
	or system_error "read_file: could not open file '$file' for reading: $!";
    binmode $fh if $binmode;
    return do { local $/; scalar <$fh> };
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
# Return the absolute version of a component path. Handles . and ..
# Second argument is directory path to resolve relative paths against.
#
sub absolute_comp_path
{
    my ($comp_path, $dir_path) = @_;

    $comp_path = "$dir_path/$comp_path" if $comp_path !~ m@^/@;
    return mason_canonpath($comp_path);
}


#
# Makes a few fixes to File::Spec::canonpath. Will go away if/when they
# accept our patch.
#
sub mason_canonpath {
    # Just like File::Spec::canonpath, but we're having trouble
    # getting a patch through to them.
    my $path = shift;
    $path =~ s|/+|/|g;                               # xx////yy  -> xx/yy
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
# Load package $pkg if not already loaded. Return 1 if file was found
# and loaded successfully. When file is not found: if optional second
# argument $nf_error is provided, die with that error message,
# otherwise return 0. Errors while loading the package are always
# passed through as fatal errors.
#
sub load_pkg {
    my ($pkg, $nf_error) = @_;

    my $file = File::Spec->catfile( split /::/, $pkg );
    $file .= '.pm';
    return 1 if exists $INC{$file};

    eval "use $pkg";

    if ($@) {
	if ($@ =~ /^Can\'t locate .* in \@INC/) {
	    if (defined($nf_error)) {
		error sprintf("Can't locate %s in \@INC. %s\n(\@INC contains: %s)",
			      $pkg, $nf_error, join(" ", @INC));
	    } else {
		undef $@;
		return 0;
	    }
	} else {
	    error $@;
	}
    }
    return 1;
}

sub taint_is_on
{
    return not eval { "$0$^X" && kill 0; 1 };
}

sub make_fh
{
    return undef if $] >= 5.6;  # Let filehandles autovivify
    return do { local *FH; *FH; };  # double *FH avoids a warning
}

sub coerce_to_array
{
    my ($val, $name) = @_;

    return ($val) unless ref $val;

    if ( UNIVERSAL::isa( $val, 'ARRAY' ) )
    {
	return @$val;
    }
    elsif ( UNIVERSAL::isa( $val, 'HASH' ) )
    {
	return %$val;
    }

    param_error "Cannot coerce $val to an array for '$name' parameter";
}

sub coerce_to_hash
{
    my ($val, $name) = @_;

    param_error "Cannot convert a single value to a hash for '$name' parameter"
	unless ref $val;

    if ( UNIVERSAL::isa( $val, 'ARRAY' ) )
    {
	return @$val;
    }
    elsif ( UNIVERSAL::isa( $val, 'HASH' ) )
    {
	return %$val;
    }

    param_error "Cannot coerce $val to a hash";
}


1;

__END__

=head1 NAME

HTML::Mason::Tools - Function library used internally in Mason

=head1 DESCRIPTION

This module contains exportable functions that are intended to be used
by other Mason modules.

The documentation here is primarily intended to be used by Mason core
developers.

Others who choose to use these functions do so at their own risk, as
they may change from release to release.  You have been warned.

=head1 FUNCTIONS

=over

=item read_file

This function takes a file name and an optional argument indicating
whether or not to open the final in binary mode.  It will return the
entire contents of the file as a scalar.

=item paths_eq

Given to paths, this function indicates whether they represent the
same location on the filesystem.  It does not account for symlinks.

=item compress_path

This turns a component path into a filesystem-friendly path by
escaping potentially meaningful characters.

=item absolute_comp_path

Given a component path and a directory path, this function returns the
absolute component path, prepending the directory path if needed.

=item mason_canonpath

This function cleans up a component path and returns its canonical
version.  It is largely the same as File::Spec::Unix::canonpath, with
a few additional cleanups.

=item pkg_installed

Given a module name, this function returns true or false to indicate
whether or not a corresponding F<.pm> file exists.

=item pkg_loaded

Given a module name, this function returns true or false to indicate
whether or not the module has been loaded into memory.

=item load_pkg

Given a module name, this function attempts to load it.  It takes an
additional boolean parameter indicating whether or not to throw an
exception if the module cannot be found.  By default, if the module
cannot be found, this function simply returns false.

All errors generate exceptions no matter what.

If the module is loaded successfully, this function returns true.

=item taint_is_on

Returns a boolean value indicating whether taint mode is on or not.

=item make_fh

This function returns something suitable to be passed to the first
argument of an C<open> function call.

=item coerce_to_array

Given a scalar, which may be a reference, this function attempts to
return an array.  It throws an HTML::Mason::Exception::Params
exception if this can't be done.

This function is called from the generated component code as part of a
component's argument handling.

=item coerce_to_hash

Given a scalar, which may be a reference, this function attempts to
return a hash.  It throws an HTML::Mason::Exception::Params exception
if this can't be done.

This function is called from the generated component code as part of a
component's argument handling.

=back

=cut
