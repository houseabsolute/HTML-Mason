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

use Params::Validate qw(:all);

my %VALID_PARAMS = ();
my %CONTAINED_OBJECTS = ();


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
    my ($class, %args) = @_;

    my %c = $class->get_contained_objects(%args);
    while (my ($name, $default_class) = each %c) {
	next if exists $args{$name};

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

    die "Invalid class name '$contained_class'" unless $contained_class =~ /^[\w:]+$/;
    {
	no strict 'refs';
	unless ( defined ${ "$contained_class\::VERSION" } )
	{
	    eval "use $contained_class";
	    die $@ if $@;
	}
    }

    # Everything this class will accept, including parameters it will
    # pass on to its own contained objects
    my $allowed = $contained_class->allowed_params($args);

    my %contained_args;

    foreach (keys %$allowed)
    {
	$contained_args{$_} = delete $args->{$_} if exists $args->{$_};
    }
    return $contained_class->new(%contained_args);
}

sub get_contained_objects
{
    my $class = ref($_[0]) || shift;

    my %c = %{ $CONTAINED_OBJECTS{$class} };
    
    no strict 'refs';
    foreach my $superclass (@{ "${class}::ISA" }) {
	next unless exists $CONTAINED_OBJECTS{$superclass};
	my %superparams = $superclass->get_contained_objects;
	@c{keys %superparams} = values %superparams;
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
	# Can accept a 'foo' parameter - should already be in the validation_spec
	next if exists $args->{$name};

	# Can accept a 'foo_class' parameter instead of a 'foo' parameter
	# If neither parameter is present, give up - perhaps it's optional
	my $low_class = "${name}_class";

	if ( exists $args->{$low_class} )
	{
	    delete $p{$name};
	    $p{$low_class} = { type => STRING, parse => 'string' };  # A loose spec
	}

	# We have to get the allowed params for the contained object
	# class.  That class could be overridden, in which case we use
	# the new class provided.  Otherwise, we use our default.
	my $contained_class = exists $args{$low_class} ? $args{$low_class} : $c{$name};

	# we have to make sure it is loaded before we try calling
	# ->allowed_params
	{
	    no strict 'refs';
	    unless ( defined ${ "$contained_class\::VERSION" } )
	    {
		eval "use $contained_class";
		die $@ if $@;
	    }
	}

	my $subparams = $contained_class->allowed_params($args);
	@p{keys %$subparams} = values %$subparams;
    }

    return \%p;
}

sub validation_spec
{
    my $class = ref($_[0]) || shift;

    my %p = %{ $VALID_PARAMS{$class} };
    
    no strict 'refs';
    foreach my $superclass (@{ "${class}::ISA" }) {
	next unless exists $VALID_PARAMS{$superclass};
	my $superparams = $superclass->validation_spec;
	@p{keys %$superparams} = values %$superparams;
    }

    return \%p;
}

1;
