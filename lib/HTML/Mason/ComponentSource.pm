# Copyright (c) 1998-2002 by Jonathan Swartz. All rights reserved.
# This program is free software; you can redistribute it and/or modify it
# under the same terms as Perl itself.

package HTML::Mason::ComponentSource;

use strict;
use File::Basename;
use File::Spec;
use HTML::Mason::Exceptions( abbr => [qw(param_error error)] );
use Params::Validate qw(:all);
Params::Validate::validation_options( on_fail => sub { param_error join '', @_  } );

use HTML::Mason::MethodMaker
    ( read_only => [ qw( comp_id
                         friendly_name
                         last_modified
                         comp_path
                         comp_class
                         extra
                        ) ],
      );

my %valid_params =
    (
     comp_id       => { type => SCALAR | UNDEF },
     friendly_name => { type => SCALAR },
     last_modified => { type => SCALAR },
     comp_path     => { type => SCALAR },
     comp_class    => { isa => 'HTML::Mason::Component' },
     extra         => { type => HASHREF, default => {} },
     source_callback => { type => CODEREF },
    );

sub allowed_params { \%valid_params }
sub validation_spec { return shift->allowed_params }

sub new
{
    my $class = shift;

    return bless { validate( @_, \%valid_params ) }, $class;
}

sub comp_source
{
    my $self = shift;

    return $self->{source} if exists $self->{source};

    $self->{source} = eval { $self->{source_callback}->() };

    if (my $err = @_)
    {
	UNIVERSAL::can( $err, 'rethrow' ) ? $err->rethrow : error $err;
    }

    unless ( defined $self->{source} )
    {
	error "source callback returned no source for $self->{friendly_name} component";
    }

    return $self->{source};
}

sub object_code
{
    my $self = shift;
    my %p = validate( @_, { compiler => { isa => 'HTML::Mason::Compiler' } } );

    return $p{compiler}->compile( comp_source => $self->comp_source,
				  name => $self->friendly_name,
				  comp_class => $self->comp_class );
}

1;

__END__

=head1 NAME

HTML::Mason::ComponentSource - represents information about an component

=head1 SYNOPSIS

    my $info = $resolver->get_info($comp_path);

=head1 DESCRIPTION

Mason uses the ComponentSource class abstract information about a source
component, one that has yet to be compiled.

=head1 METHODS

=over

=item new

This method takes the following arguments:

=over 4

=item * comp_path

The component's component path.

=item * last_modified

This is the last modificatoin time for the component, in Unix time
(seconds since the epoch).

=item * comp_id

This is a unique id for the component used to distinguish two
components with the same name in different component roots.

If your resolver does not support multiple component roots, this can
simply be the same as the "comp_path" key or it can be any other id
you wish.

This value will be used when constructing filesystem paths so it needs
to be something that works on different filesystems.  If it contains
forward slashes, these will be converted to the appropriate
filesystem-specific path separator.

In fact, we encourage you to make sure that your component ids have
some forward slashes in them or also B<all> of your generated object
files will end up in a single directory, which could affect
performance.

=item * comp_class

The component class into which this particular component should be
blessed when it is created.  This must be a subclass of
C<HTML::Mason::Component>.

=item * friendly_name

This is used when displaying error messages related to the component,
like parsing errors.  This should be something that will help whoever
sees the message identify the component.  For example, for component
stored on the filesystem, this should be the absolute path to the
component.

=item * source_callback

This is a subroutine reference which, when called, returns the
component source.

The reasoning behind using this parameter is that it helps avoid a
profusion of tiny little C<HTML::Mason::ComponentSource> subclasses that
don't do very much.

=item * extra

This optional parameter should be a hash reference.  It is used to
pass information that doesn't concern the interpreter from the
resolver to the component class.

This is needed since a L<C<HTML::Mason::Resolver>> subclass and a
L<C<HTML::Mason::Component>> subclass can be rather tightly coupled,
but they must communicate with each through the interpreter (this may
change in the future).

=back

=item comp_path

=item last_modified

=item comp_id

=item comp_class

=item friendly_name

=item extra

These are all simple accessors that return the value given to the
constructor.

=item comp_source

Returns the source of the component.

=item object_code ( compiler => $compiler )

Given a compiler, this method returns the object code for the
component.

=back

L<HTML::Mason>,
L<HTML::Mason::Admin>,
L<HTML::Mason::Component>

=cut
