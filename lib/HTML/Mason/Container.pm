# Copyright (c) 1998-2002 by Jonathan Swartz. All rights reserved.
# This program is free software; you can redistribute it and/or modify it
# under the same terms as Perl itself.

package HTML::Mason::Container;

use strict;

# The create_contained_objects() method lets one object
# (e.g. Compiler) transparently create another (e.g. Lexer) by passing
# creator parameters through to the created object.
#
# Any auto-created objects should be declared in a class's
# %CONTAINED_OBJECTS hash.  The keys of this hash are objects which
# can be created and the values are the default classes to use.

# For instance, the key 'lexer' indicates that a 'lexer' parameter
# should be silently passed through, and a 'lexer_class' parameter
# will trigger the creation of an object whose class is specified by
# the value.  If no value is present there, the value of 'lexer' in
# the %CONTAINED_OBJECTS hash is used.  If no value is present there,
# no contained object is created.
#
# We return the list of parameters for the creator.  If contained
# objects were auto-created, their creation parameters aren't included
# in the return value.  This lets the creator be totally ignorant of
# the creation parameters of any objects it creates.

use HTML::Mason::Exceptions (abbr => [qw(error param_error)]);

use Params::Validate qw(:types);
Params::Validate::validation_options( on_fail => sub { param_error( join '', @_ ) } );

my %VALID_PARAMS = ();
my %CONTAINED_OBJECTS = ();

sub all_specs
{
    require B::Deparse;
    my %out;

    foreach my $class (sort keys %VALID_PARAMS)
    {
	my $params = $VALID_PARAMS{$class};

	foreach my $name (sort keys %$params)
	{
	    my $spec = $params->{$name};
	    my ($type, $default);
	    if ($spec->{isa}) {
		my $obj_class;

		$type = 'object';

		if (exists $CONTAINED_OBJECTS{$class}{$name}) {
		    $obj_class = $CONTAINED_OBJECTS{$class}{$name};
		    $obj_class = $obj_class->{class} if ref $obj_class;

		    $default = "$obj_class\->new";
		}
	    } else {
		($type, $default) = ($spec->{parse}, $spec->{default});
	    }

	    if (ref($default) eq 'CODE') {
		$default = 'sub ' . B::Deparse->new()->coderef2text($default);
		$default =~ s/\s+/ /g;
	    } elsif (ref($default) eq 'ARRAY') {
		$default = '[' . join(', ', map "'$_'", @$default) . ']';
	    } elsif (ref($default) eq 'Regexp') {
		$type = 'regex';
		$default =~ s/^\(\?(\w*)-\w*:(.*)\)/\/$2\/$1/;
	    }
	    unless ($type) {
	      # Guess from the validation spec
	      $type = ($spec->{type} & ARRAYREF ? 'list' :
		       $spec->{type} & SCALAR   ? 'string' :
		       $spec->{type} & CODEREF  ? 'code' :
		       $spec->{type} & HASHREF  ? 'hash' :
		       undef);  # Oh well
	    }

	    my $descr = $spec->{descr} || '(No description available)';
	    $out{$class}{valid_params}{$name} = {type => $type, default => $default, descr => $descr};
	}

	$out{$class}{contained_objects} = {};
	next unless exists $CONTAINED_OBJECTS{$class};
	my $contains = $CONTAINED_OBJECTS{$class};

	foreach my $name (sort keys %$contains)
	{
	    $out{$class}{contained_objects}{$name} = ref($contains->{$name}) 
		? {map {$_, $contains->{$name}{$_}} qw(class delayed)}
		: {class => $contains->{$name}, delayed => 0};
	}
    }

    return %out;
}

sub valid_params
{
    my $class = shift;
    $VALID_PARAMS{$class} = {@_};
}

sub contained_objects
{
    my $class = shift;
    $CONTAINED_OBJECTS{$class} = {@_};
}

sub create_contained_objects
{
    # Typically $self doesn't exist yet, $_[0] is a string classname
    my ($class, %args) = @_;

    my %c = $class->get_contained_objects;
    while (my ($name, $spec) = each %c) {
	my $default_class = ref($spec) ? $spec->{class}   : $spec;
	my $delayed       = ref($spec) ? $spec->{delayed} : 0;
	if (exists $args{$name}) {
	    # User provided an object
	    param_error "Cannot provide a '$name' object, its creation is delayed"
		if $delayed;

	    #
	    # We still need to delete any arguments that _would_ have
	    # been given to this object's constructor (if the object
	    # had not been given).  This allows a container class to
	    # provide defaults for a contained object will still
	    # accepting an already constructed object as one of its
	    # params.
	    #
	    $class->_get_contained_args(ref $args{$name}, \%args);
	    next;
	}

	# Figure out exactly which class to make an object of
	my $contained_class = delete $args{"${name}_class"} || $default_class;
	next unless $contained_class;

	if ($delayed) {
	    $args{"_delayed_$name"}{args} = $class->_get_contained_args($contained_class, \%args);
	    $args{"_delayed_$name"}{class} = $contained_class;
	} else {
	    $args{$name} = $class->_make_contained_object($contained_class, \%args);
	}
    }

    return %args;
}

sub create_delayed_object
{
    my ($self, $name, %args) = @_;

    # It just has to exist.  An empty hash is fine.
    param_error "Unknown delayed object '$name'"
	unless exists $self->{"_delayed_$name"}{args};

    my $class = $self->{"_delayed_$name"}{class}
	or param_error "Unknown class for delayed object '$name'";

    return $class->new( %{ $self->{"_delayed_$name"}{args} }, %args );
}

sub delayed_object_params
{
    my ($self, $name, %args) = @_;

    param_error "Unknown delayed object '$name'"
	unless exists $self->{"_delayed_$name"}{args};

    if (%args)
    {
	@{ $self->{"_delayed_$name"}{args} }{ keys(%args) } = values(%args);
    }

    return %{ $self->{"_delayed_$name"}{args} };
}

sub _get_contained_args
{
    my ($class, $contained_class, $args) = @_;

    param_error "Invalid class name '$contained_class'"
	unless $contained_class =~ /^[\w:]+$/;

    unless ( eval { $contained_class->can('new') } )
    {
	no strict 'refs';
	eval "use $contained_class";
	error $@ if $@;
    }

    return {} unless $contained_class->can('allowed_params');

    # Everything this class will accept, including parameters it will
    # pass on to its own contained objects
    my $allowed = $contained_class->allowed_params($args);

    my $spec = $class->validation_spec;

    my %contained_args;
    foreach (keys %$allowed)
    {
	$contained_args{$_} = $args->{$_} if exists $args->{$_};

	# If both the container _and_ the contained object accept the
	# same param we should not delete it.
	delete $args->{$_} unless exists $spec->{$_};
    }
    return \%contained_args;
}

sub _make_contained_object
{
    my ($class, $contained_class, $args) = @_;

    my $contained_args = $class->_get_contained_args($contained_class, $args);
    return $contained_class->new(%$contained_args);
}

# Iterate through this object's @ISA and find all entries in
# 'contained_objects' list.  Return as a hash.
sub get_contained_objects
{
    my $class = ref($_[0]) || shift;

    my %c = %{ $CONTAINED_OBJECTS{$class} || {} };

    no strict 'refs';
    foreach my $superclass (@{ "${class}::ISA" }) {
	my %superparams = $superclass->get_contained_objects;
	@c{keys %superparams} = values %superparams;  # Add %superparams to %c
    }

    return %c;
}

sub allowed_params
{
    my $class = shift;
    my $args = ref($_[0]) ? shift : {@_};

    my %p = %{ $class->validation_spec };

    my %c = $class->get_contained_objects;

    foreach my $name (keys %c)
    {
	# Can accept a 'foo' parameter - should already be in the validation_spec.
	# Also, its creation parameters should already have been extracted from $args,
	# so don't extract any parameters.
	next if exists $args->{$name};

	# Can accept a 'foo_class' parameter instead of a 'foo' parameter
	# If neither parameter is present, give up - perhaps it's optional
	my $low_class = "${name}_class";

	if ( exists $args->{$low_class} )
	{
	    delete $p{$name};
	    $p{$low_class} = { type => SCALAR, parse => 'string' };  # A loose spec
	}

	# We have to get the allowed params for the contained object
	# class.  That class could be overridden, in which case we use
	# the new class provided.  Otherwise, we use our default.
	my $spec = exists $args->{$low_class} ? $args->{$low_class} : $c{$name};
	my $contained_class = ref($spec) ? $spec->{class}   : $spec;

	# we have to make sure it is loaded before we try calling
	# ->allowed_params
	unless ( eval { $contained_class->can('new') } )
	{
	    eval "use $contained_class";
	    error $@ if $@;
	}

	next unless $contained_class->can('allowed_params');

	my $subparams = $contained_class->allowed_params($args);

	#
	# What we're doing here is checking for parameters in
	# contained objects that expect an object of which the current
	# class (for which we are retrieving allowed params) is a
	# subclass (or the same class).
	#
	# For example, the HTML::Mason::Request class accepts an
	# 'interp' param that must be of the HTML::Mason::Interp
	# class.
	#
	# But the HTML::Mason::Interp class contains a request object.
	# While it makes sense to say that the interp class can accept
	# a parameter like 'autoflush' on behalf of the request, it
	# makes very little sense to say that the interp can accept an
	# interp as a param.
	#
	# This _does_ cause a potential problem if we ever want to
	# have a class that 'contains' other objects of the same
	# class.
	#
	foreach (keys %$subparams)
	{
	    if ( ref $subparams->{$_} &&
		 exists $subparams->{$_}{isa} &&
		 $class->isa( $subparams->{$_}{isa} ) )
	    {
		next;
	    }
	    $p{$_} = $subparams->{$_};
	}
    }

    return \%p;
}

sub validation_spec
{
    my $class = ref($_[0]) || shift;

    my %p = %{ $VALID_PARAMS{$class} || {} };

    no strict 'refs';
    foreach my $superclass (@{ "${class}::ISA" }) {
	my $superparams = $superclass->validation_spec;
	@p{keys %$superparams} = values %$superparams;
    }

    # We may need to allow some '_delayed_$name' parameters
    my %specs = $class->get_contained_objects;
    while (my ($name, $spec) = each %specs) {
	next unless ref $spec;
	next unless $spec->{delayed};
	$p{"_delayed_$name"} = { type => HASHREF };
    }

    return \%p;
}

1;

__END__

=head1 NAME

HTML::Mason::Container - base class for other Mason objects

=head1 SYNOPSIS

  package HTML::Mason::FooBar;

  use HTML::Mason::Container;
  use base qw(HTML::Mason::Container);

=head1 DESCRIPTION

A number of modules in Mason are subclasses of HTML::Mason::Container.
This is a class that was created to encapsulate some common behaviors
for Mason objects.  Basically, any Mason object which takes parameters
to its constructor B<must> inherit from this module.  Of course, since
all of the classes that you might consider subclassing already inherit
from HTML::Mason::Container, you shouldn't need to inherit from it
directly.  However, you may need to use some of its methods.  We will
cover a few of them here but see the HTML::Mason::Container
documentation for more details.

The modules in the Mason core distribution that are
HTML::Mason::Container subclasses are
HTML::Mason::ApacheHandler, HTML::Mason::CGIHandler,
HTML::Mason::Compiler, HTML::Mason::Interp,
HTML::Mason::Lexer, and HTML::Mason::Resolver.

The most important methods that HTML::Mason::Container provides are
C<valid_params> and C<contained_objects>, both of which are class
methods.

The first, C<valid_params>, is called in order to register a set of
parameters which are valid for a class's C<new> constructor.  The
second method, C<contained_objects>, is used to register what other
objects, if any, a given class contains.

The second method, C<contained_objects>, is not something you are
terribly likely to have to use.  It is called with a hash that
contains as its keys parameter names that the class's constructor
accepts, and as its values the default name of the contained class.

For example, HTML::Mason::Compiler contains the following code:

  __PACKAGE__->contained_objects( lexer => 'HTML::Mason::Lexer' );

This says that the C<< HTML::Mason::Compiler->new >> method will
accept a C<lexer> parameter and that, if no such parameter is given,
then an object of the HTML::Mason::Lexer class will be constructed.

Mason also implements a bit of magic here, so that if C<<
HTML::Mason::Compiler->new >> is called with a C<lexer_class>
parameter, it will load the class, instantiate a new object of that
class, and use that for the lexer.  In fact, Mason is smart enough to
notice if parameters given C<< HTML::Mason::Compiler->new >> actually
should go to this subclass, and it will make sure that they get passed
along.

The C<valid_params> method is generally a bit more complex.  It too
takes a hash.  The keys of this hash are the names of parameters while
the values are themselves hash references, each of which defines a
validation specification for the parameter.

This specification is largely the same as that used by the
Params::Validate module, with one addition.  Each parameter,
excluding those that represent contained objects, should also define a
value for C<parse>.  This tells Mason how to parse this parameter if
it is defined as part of an Apache configuration file.

The upshot of this is that your subclasses can define their own
constructor parameters and Mason will then check for these parameters
in an Apache configuration file.

As an example, HTML::Mason::Compiler contains the following:

  __PACKAGE__->valid_params
      (
       allow_globals        => { parse => 'list',   type => ARRAYREF, default => [] },
       default_escape_flags => { parse => 'string', type => SCALAR,   default => '' },
       lexer                => { isa => 'HTML::Mason::Lexer' },
       preprocess           => { parse => 'code',   type => CODEREF,  optional => 1 },
       postprocess_perl     => { parse => 'code',   type => CODEREF,  optional => 1 },
       postprocess_text     => { parse => 'code',   type => CODEREF,  optional => 1 },
      );

The C<type>, C<default>, and C<optional> parameters are part of the
validation specification used by C<Params::Validate>.  The various
constants used, C<ARRAYREF>, C<SCALAR>, etc. are all exported by
Params::Validate.  These parameters correspond to the
MasonAllowGlobals, MasonDefaultEscapeFlags, MasonLexerClass (yes,
B<Class> is added automatically because it was given to the
C<contained_objects> method.  Whee, magic!)  Don't worry about it too
much as long as you can see the pattern here.), etc. Apache
configuration parameters.

=head1 SEE ALSO

L<HTML::Mason>

=cut
