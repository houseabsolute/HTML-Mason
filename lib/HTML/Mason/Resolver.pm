# Copyright (c) 1998-2002 by Jonathan Swartz. All rights reserved.
# This program is free software; you can redistribute it and/or modify it
# under the same terms as Perl itself.

package HTML::Mason::Resolver;

use strict;

use HTML::Mason::Exceptions( abbr => ['param_error', 'virtual_error'] );

use Params::Validate qw(:all);
Params::Validate::validation_options( on_fail => sub { param_error join '', @_ } );

use HTML::Mason::Container;
use base qw(HTML::Mason::Container);

sub new
{
    my $class = shift;
    return bless {validate(@_, $class->validation_spec)}, $class;
}

# Returns all info about a component including its source.
sub resolve {
    shift->_virtual;
}

sub glob_path {
    shift->_virtual;
}

sub comp_class {
    shift->_virtual;
}

# Gets just the source of the component as a string.
sub get_source {
    shift->_virtual;
}

# Returns all info about a component, but not its source.
sub get_info {
    shift->_virtual;
}

sub apache_request_to_comp_path {
    shift->_virtual
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

Give a component path, this method is expected to return a hash with
the following keys.

=over 4

=item * url_path

This is the same as incoming path parameter.

=item * last_modified

This is the last modificatoin time for the component, in Unix time
(seconds since the epoch).

=item * comp_id

This is a unique id for the component used to distinguish two
components with the same name in different component
roots.

If your resolver does not support multiple component roots, this can
simply be the same as the "url_path" key or it can be any other id you
wish.

This value will be used when constructing filesystem paths so it needs
to be something that works on different filesystems.  If it contains
forward slashes, these will be converted to the appropriate
filesystem-specific path separator.

In fact, we encourage you to make sure that your component ids have
some forward slashes in them or also B<all> of your generated object
files will end up in a single directory, which could affect
performance.

=back

This method may also return any other keys it wishes.  These keys will
be passed to the component class's constructor.  This can be handy if
you are using a custom component class in addition to a custom
resolver.

=item get_source

This method should expect to receive the hash returned by the
C<get_info> method.

It should return a single scalar containing the source of the
component.

=item resolve

This method should expect to receive a single argument, a component
path, just like the C<get_info> method.  It should return a hash
containing all of the information returned by the C<get_info> method,
in addition to a key called "comp_text" which should contain the
component source.

=item glob_path

The only argument to this method is a path glob pattern, something
like "/foo/*" or "/foo/*/bar".  Given this path, it should return a
list of component paths for components which match this glob pattern.

For example, the filesystem resolver simply appends this pattern to
each component root in turn and calls the Perl C<glob()> function to
find matching files on the filesystem.

=item comp_class

This method needs to return the class that component objects should
use.  If you do not want to create a custom component class, you can
simply use "HTML::Mason::Component".

=back

=head2 Using a Resolver with HTML::Mason::ApacheHandler

If you are creating a new resolver that you intend to use with the
L<C<HTML::Mason::ApacheHandler>> module, then you must implement the
following method as well, possibly in a different subclass.

For example, Mason includes the C<HTML::Mason::Resolver::File> and
C<HTML::Mason::Resolver::File::Apache> classes.  The latter simply
adds an implementation of this method for file based components.

=over 4

=item apache_request_to_comp_path

This method, given an Apache object, should return a component path.
This method is used by the HTML::Mason::ApacheHandler class to
translate web requests into component paths.  You can omit this method
if your resolver subclass will never be used in conjunction with
HTML::Mason::ApacheHandler.

=back

=cut
