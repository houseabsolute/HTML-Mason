# Copyright (c) 1998-2002 by Jonathan Swartz. All rights reserved.
# This program is free software; you can redistribute it and/or modify it
# under the same terms as Perl itself.

package HTML::Mason::Resolver;

use strict;

use HTML::Mason::Exceptions( abbr => ['param_error', 'virtual_error'] );

use Params::Validate qw(:all);
Params::Validate::validation_options( on_fail => sub { param_error join '', @_ } );

use HTML::Mason::ComponentSource;

use HTML::Mason::Container;
use base qw(HTML::Mason::Container);

sub new
{
    my $class = shift;
    return bless {validate(@_, $class->validation_spec)}, $class;
}

# Returns HTML::Mason::ComponentSource object
sub get_info {
    shift->_virtual;
}

sub glob_path {
    shift->_virtual;
}

sub rel2abs {
    return undef;
}

sub _virtual
{
    my $self = shift;

    my $sub = (caller(1))[3];
    $sub =~ s/.*::(.*?)$/$1/;
    virtual_error "$sub is a virtual method and must be overridden in " . ref($self);
}

1;

__END__

=head1 NAME

HTML::Mason::Resolver - base class for component path resolvers

=head1 SYNOPSIS

  # make a subclass and use it

=head1 DESCRIPTION

The resolver is responsible for translating a component path like
/foo/index.html into a component.  By default, Mason expects
components to be stored on the filesystem, and uses the
HTML::Mason::Resolver::File class to get information on these
components.

The HTML::Mason::Resolver provides a virtual parent class from which
all resolver implementations should inherit.

=head1 HTML::Mason::Container

This class is used by most of the Mason object's to manage constructor
parameters and has-a relationships with other objects.

See the documentation on this class for details on how to declare what
paremeters are valid for your subclass's constructor.

HTML::Mason::Resolver is a subclass of HTML::Mason::Container so you
do not need to subclass it yourself.

=head1 METHODS

If you are interested in creating a resolver subclass, you must
implement the following methods.

=over 4

=item new

This method is optional.  The new method included in this class does
the following:

  sub new
  {
      my $class = shift;
      return bless {validate(@_, $class->validation_spec)}, $class;
  }

If you need something more complicated done in your new method you
will need to override it in your subclass.

=item get_info

Given an absolute component path, returns a new
L<HTML::Mason::ComponentSource|HTML::Mason::ComponentSource> object.

=item glob_path

The only argument to this method is a path glob pattern, something
like "/foo/*" or "/foo/*/bar".  Given this path, it should return a
list of component paths for components which match this glob pattern.

For example, the filesystem resolver simply appends this pattern to
each component root in turn and calls the Perl C<glob()> function to
find matching files on the filesystem.

=item rel2abs ($path)

Given a relative component $path, returns its corresponding absolute path.
The function may return undef indicating that only absolute paths are
allowed - this is the default.

This function is only called if $interp->exec or $interp->load
receives a relative path. Relative paths to $m->comp and its siblings
are automatically converted to absolute before being passed to the
resolver.

=back

=head2 Using a Resolver with HTML::Mason::ApacheHandler

If you are creating a new resolver that you intend to use with the
L<HTML::Mason::ApacheHandler|HTML::Mason::ApacheHandler> module, then
you must implement the following method as well, possibly in a
different subclass.

=over 4

=item apache_request_to_comp_path ($r)

This method, given an Apache object, should return a component path.
This method is used by the
L<HTML::Mason::ApacheHandler|HTML::Mason::ApacheHandler> class to
translate web requests into component paths.  You can omit this method
if your resolver subclass will never be used in conjunction with
L<HTML::Mason::ApacheHandler|HTML::Mason::ApacheHandler>.

=back

For example, Mason includes the
L<HTML::Mason::Resolver::File|HTML::Mason::Resolver::File> and
HTML::Mason::Resolver::File::ApacheHandler classes.  The latter simply
adds an implementation of the C<apache_request_to_comp_path> method
for file based components.

=head1 SEE ALSO

L<HTML::Mason|HTML::Mason>

=cut
