# Copyright (c) 1998-2001 by Jonathan Swartz. All rights reserved.
# This program is free software; you can redistribute it and/or modify it
# under the same terms as Perl itself.

package HTML::Mason::Container;

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

sub create_contained_objects
{
    my ($class, %args) = @_;
    #warn "--- creating $package, received args (@{[%args]})\n";

    my %c = $class->contained_objects;
    while (my ($name, $default_class) = each %c) {

	# Figure out exactly which class to make an object of
	my $contained_class = delete $args{"${name}_class"} || $default_class;
	next unless $contained_class;

	$args{$name} = $class->_make_contained_object($name, $contained_class, \%args);
    }

    return %args;
}

sub _make_contained_object
{
    my ($class, $name, $contained_class, $args) = @_;

    {
	no strict 'refs';
	unless ( defined ${ "$contained_class\::VERSION" } )
	{
	    eval "use $contained_class";
	    die $@ if $@;
	}
    }

    # Everything this class will accept, including parameters it will
    # pass onto its own contained objects
    my $allowed = $contained_class->allowed_params;

    my %contained_args;

    foreach (keys %$allowed)
    {
	$contained_args{$_} = delete $args->{$_} if exists $args->{$_};
    }

    return $args->{$name} if exists $args->{$name};

    return $contained_class->new(%contained_args);
}

sub contained_objects
{
    my $class = shift;

    {
	no strict 'refs';
	return %{ "$class\::CONTAINED_OBJECTS" };
    }
}

sub allowed_params
{
    my $class = shift;

    my %c = $class->contained_objects;

    # this is also broken.  contained_objects returns defaults, which
    # could end up being different from what should be created.  Plus
    # it has to load them.  Ugh, my head hurts.
    my %p = ( %{ $class->validation_spec },
	      map { eval "require $_";
		    die $@ if $@;
		    %{ $_->validation_spec } } values %c
	    );

    return \%p;
}

1;
