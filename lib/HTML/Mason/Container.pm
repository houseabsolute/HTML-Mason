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

    my %c = $class->contained_objects(%args);
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
    my $allowed = $contained_class->allowed_params(%$args);

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
    my %args = @_;

    my @c;
    {
	no strict 'refs';
	my @isa = ( ref $class || $class, @{ "$class\::ISA" } );
	while ( my $c = shift @isa )
	{
	    # make a copy of the global.
	    my %c = %{ \%{ "$c\::CONTAINED_OBJECTS" } };
	    foreach my $name ( keys %c )
	    {
		$c{$_} = $args{"${name}_class"} if exists $args{"${name}_class"};
	    }

	    push @c, %c;
	}
    }

    return @c;
}

sub allowed_params
{
    my ($class, %args) = @_;

    my %c = $class->contained_objects(%args);

    my @classes;
    foreach my $name (keys %c)
    {
	# it will accept a foo_class parameter
	$p{"${name}_class"} = { optional => 1 };

	if ( $args{"${name}_class"} )
	{
	    push @classes, $args{"${name}_class"};
	}
	elsif ( $c{$name} )
	{
	    push @classes, $c{$name};
	}
    }

    my %p = ( %{ $class->validation_spec },
	      map { eval "use $_";
		    die $@ if $@;
		    %{ $_->allowed_params(%args) } } @classes
	    );

    return \%p;
}

sub validation_spec
{
    my $class = shift;

    my @p;
    {
	no strict 'refs';
	my @isa = ( ref $class || $class, @{ "$class\::ISA" } );
	while ( my $c = shift @isa )
	{
	    push @p, %{ "$c\::VALID_PARAMS" };
	}
    }

    return { @p };
}

1;
