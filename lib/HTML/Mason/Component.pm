# Copyright (c) 1998-2002 by Jonathan Swartz. All rights reserved.
# This program is free software; you can redistribute it and/or modify it
# under the same terms as Perl itself.

package HTML::Mason::Component;

use strict;
use File::Spec;
use HTML::Mason::Exceptions( abbr => [qw(param_error)] );
use HTML::Mason::Tools qw(absolute_comp_path);
use Params::Validate qw(:all);
Params::Validate::validation_options( on_fail => sub { param_error join '', @_  } );

use HTML::Mason::Exceptions( abbr => ['error'] );
use HTML::Mason::MethodMaker
    ( read_only => [ qw( code
			 create_time
			 declared_args
			 comp_id
			 inherit_path
			 inherit_start_path
                         interp
			 object_size
			 compiler_id ) ],

      read_write => [ [ dynamic_subs_request => { isa => 'HTML::Mason::Request' } ],
		      [ mfu_count => { type => SCALAR } ],
		    ]
      );

#  XXX This needs to be a HTML::Mason::Container.
# Interesting sidenote: more people in Australia than I expected say 
# the letter H as "Haitch" instead of "Aitch".  Thus "a" instead of "an".

my %valid_params =
    (
     attr               => {type => HASHREF, default => {}},
     code               => {type => CODEREF},
     create_time        => {type => SCALAR,  optional => 1},
     declared_args      => {type => HASHREF, default => {}},
     dynamic_subs_init  => {type => CODEREF, default => sub {}},
     flags              => {type => HASHREF, default => {}},
     comp_id            => {type => SCALAR,  optional => 1},
     methods            => {type => HASHREF, default => {}},
     mfu_count          => {type => SCALAR,  default => 0},
     object_size        => {type => SCALAR,  default => 0},
     parser_version     => {type => SCALAR,  optional => 1}, # allows older components to be instantied
     compiler_id        => {type => SCALAR,  optional => 1},
     subcomps           => {type => HASHREF, default => {}},
    );

sub allowed_params { \%valid_params }
sub validation_spec { return shift->allowed_params }

sub new
{
    my $class = shift;
    my $self = bless {
		      validate(@_, $class->validation_spec),
		     }, $class;

    # Initialize subcomponent and method properties.
    while (my ($name,$c) = each(%{$self->{subcomps}})) {
	$c->assign_subcomponent_properties($self,$name);
    }
    while (my ($name,$c) = each(%{$self->{methods}})) {
	$c->assign_subcomponent_properties($self,$name);
    }

    return $self;
}

my $comp_count = 0;
sub assign_runtime_properties {
    my ($self, $interp, $info) = @_;
    $self->{interp} = $interp;
    $self->{comp_id} = defined $info->comp_id ? $info->comp_id : "[anon ". ++$comp_count . "]";

    $self->{path} = $info->comp_path;

    $self->_determine_inheritance;

    foreach my $c (values(%{$self->{subcomps}})) {
	$c->assign_runtime_properties($interp, $info);
    }
}

sub _determine_inheritance {
    my $self = shift;

    my $interp = $self->interp;

    # Assign inheritance properties
    if (exists($self->{flags}->{inherit})) {
	if (defined($self->{flags}->{inherit})) {
	    $self->{inherit_path} = absolute_comp_path($self->{flags}->{inherit}, $self->dir_path);
	}
    } else {
	if ($self->name eq $interp->autohandler_name) {
	    unless ($self->dir_path eq '/') {
		($self->{inherit_start_path}) = $self->dir_path =~ m,^(.*/)?.*,s
	    }
	} else {
	    $self->{inherit_start_path} = $self->dir_path;
	}
    }
}

sub run {
    my $self = shift;

    $self->{mfu_count}++;

    # Note: this must always preserve calling wantarray() context
    return $self->{code}->(@_);
}

sub dynamic_subs_init {
    my $self = shift;

    $self->{dynamic_subs_hash} = $self->{dynamic_subs_init}->();
    error "could not process <%shared> section (does it contain a return()?)"
	unless ref($self->{dynamic_subs_hash}) eq 'HASH';
}

sub run_dynamic_sub {
    my ($self, $key, @args) = @_;

    error "call_dynamic: assert error - could not find code for key $key in component " . $self->title
	unless exists $self->{dynamic_subs_hash}->{$key};

    $self->{dynamic_subs_hash}->{$key}->(@args);
}

# Legacy, left in for pre-0.8 obj files
sub assign_subcomponent_properties {}

#
# By default components are not persistent.
#
sub persistent { 0 }

#
# Only true in Subcomponent subclass.
#
sub is_subcomp { 0 }

#
# Only true in FileBased subclass.
#
sub is_file_based { 0 }

#
# Basic defaults for component designators: title, path, name, dir_path
#
sub title { return $_[0]->{comp_id} }
sub name { return $_[0]->{comp_id} }
sub path { return undef }
sub dir_path { return undef }

#
# Get all subcomps or particular subcomp by name
#
sub subcomps {
    my ($self,$key) = @_;
    if (defined($key)) {
	return $self->{subcomps}->{$key};
    } else {
	return $self->{subcomps};
    }
}

#
# Get all methods or particular method by name
#
sub methods {
    my ($self,$key) = @_;
    if (defined($key)) {
	return $self->{methods}->{$key};
    } else {
	return $self->{methods};
    }
}

#
# Get attribute by name
#
sub attr {
    my ($self,$name) = @_;
    my $value;
    if ($self->_locate_inherited('attr',$name,\$value)) {
	return $value;
    } else {
	error "no attribute '$name' for component " . $self->title;
    }
}

sub attr_if_exists {
    my ($self,$name) = @_;
    my $value;
    if ($self->_locate_inherited('attr',$name,\$value)) {
	return $value;
    }
}

#
# Determine if particular attribute exists
#
sub attr_exists {
    my ($self,$name) = @_;
    return $self->_locate_inherited('attr',$name);
}

#
# Call method by name
#
sub call_method {
    my ($self,$name,@args) = @_;
    my $method;
    if ($self->_locate_inherited('methods',$name,\$method)) {
	HTML::Mason::Request->instance->comp({base_comp=>$self},$method,@args);
    } else {
	error "no method '$name' for component " . $self->title;
    }
}

#
# Like call method, but return component output.
#
sub scall_method {
    my ($self,$name,@args) = @_;
    my $method;
    if ($self->_locate_inherited('methods',$name,\$method)) {
	HTML::Mason::Request->instance->scomp({base_comp=>$self},$method,@args);
    } else {
	error "no method '$name' for component " . $self->title;
    }
}

#
# Determine if particular method exists
#
sub method_exists {
    my ($self,$name) = @_;
    return $self->_locate_inherited('methods',$name);
}

#
# Locate a component slot element following inheritance path
#
sub _locate_inherited {
    my ($self,$field,$key,$ref) = @_;
    my $count = 0;
    for (my $comp = $self; $comp; $comp = $comp->parent) {
	if (exists($comp->{$field}->{$key})) {
	    $$ref = $comp->{$field}->{$key} if $ref;
	    return 1;
	}
	error "inheritance chain length > 32 (infinite inheritance loop?)"
	    if ++$count > 32;
    }
    return 0;
}

#
# Get particular flag by name
#
sub flag {
    my ($self,$name) = @_;
    my %flag_defaults =
	(
	 );
    if (exists($self->{flags}->{$name})) {
	return $self->{flags}->{$name};
    } elsif (exists($flag_defaults{$name})) {
	return $flag_defaults{$name};
    } else {
	error "invalid flag: $name";
    }
}

#
# Return parent component according to inherit flag
#
sub parent {
    my ($self) = @_;
    my $interp = $self->interp;

    my $comp;
    if ($self->inherit_path) {
	$comp = $interp->load($self->inherit_path);
    } elsif ($self->inherit_start_path) {
	$comp = $interp->find_comp_upwards($self->inherit_start_path, $interp->autohandler_name);
    }

    return $comp;
}

#
# Accessors for various files associated with component
#
sub object_file {
    my $self = shift;
    return $self->interp->object_file($self);
}

1;

__END__

=head1 NAME

HTML::Mason::Component - Mason Component Class

=head1 SYNOPSIS

    my $comp1 = $m->current_comp;
    my $comp2 = $m->callers(1);
    my $comp3 = $m->fetch_comp('foo/bar');

    foreach ($comp1,$comp2,$comp3) {
       print "My name is ".$_->title.".\n";
    }

=head1 DESCRIPTION

Mason uses the Component class to store components loaded into
memory. Components come from three distinct sources:

=over 4

=item 1

File-based: loaded from a source or object file.

=item 2

Subcomponents: embedded components defined with the C<E<lt>%defE<gt>> 
or C<E<lt>%methodE<gt>> tags.

=item 3

Anonymous: created on-the-fly with the C<make_component> Interp method.

=back

Some of the methods below return different values (or nothing at all)
depending on the component type.

The component API is primarily useful for introspection, e.g. "what
component called me" or "does the next component take a certain
argument".  You can build complex Mason sites without ever dealing
directly with a component object.

=head2 CREATING AND ACCESSING COMPONENTS

Common ways to get handles on existing component objects include the
L<Request-E<gt>current_comp|HTML::Mason::Request/current_comp>,
L<Request-E<gt>callers|HTML::Mason::Request/callers>, and
L<Request-E<gt>fetch_comp|HTML::Mason::Request/fetch_comp> methods.

There is no published C<new> method, because creating a component
requires an Interpreter. Use the L<Interp's
make_component|HTML::Mason::Interp/make_component> method to create a
new component dynamically.

Similarly, there is no C<execute> or C<call> method, because calling a
component requires a request. All of the interfaces for calling a
component (<& &>, C<$m->comp>, C<$interp-E<gt>exec>)
which normally take a component path will also take a component
object.

=head1 METHODS

=over

=item attr (name)

Looks for the specified attribute in this component and its parents,
returning the first value found. Dies with an error if not
found. Attributes are declared in the C<E<lt>%attrE<gt>> section.

=item attr_if_exists (name)

This method works exactly like the one above but returns undef if the
attribute does not exist.

=item attr_exists (name)

Returns true if the specified attribute exists in this component or
one of its parents, undef otherwise.

=item create_time

Returns the time (in Perl time() format) when this component object
was created.

=item declared_args

Returns a reference to a hash of hashes representing the arguments
declared in the C<E<lt>%argsE<gt>> section. The keys of the main hash are the
variable names including prefix (e.g. C<$foo>, C<@list>). Each	
secondary hash contains:

=over 4

=item *

'default': the string specified for default value (e.g. 'fido') or undef
if none specified.  Note that in general this is not the default value
itself but rather a Perl expression that gets evaluated every time the
component runs.

=back

For example:

  # does $comp have an argument called $fido?
  if (exists($comp->declared_args->{'$fido'})) { ... }

  # does $fido have a default value?
  if (defined($comp->declared_args->{'$fido'}->{default})) { ... }

=item dir_path

Returns the component's notion of a current directory, relative to the
component root; this is used to resolve relative component paths. For
file-based components this is the full component path minus the filename.
For subcomponents this is the same as the component that defines it.
Undefined for anonymous components.

=item flag (name)

Returns the value for the specified system flag.  Flags are declared
in the C<E<lt>%flagsE<gt>> section and affect the behavior of the component.
Unlike attributes, flags values do not get inherited from parent components.

=item is_subcomp

Returns true if this is a subcomponent of another component.

=item is_file_based

Returns true if this component was loaded from a source or object
file.

=item call_method (name, args...)

Looks for the specified user-defined method in this component and its
parents, calling the first one found. Dies with an error if not found.
Methods are declared in the C<E<lt>%methodE<gt>> section.

=item method_exists (name)

Returns true if the specified user-defined method exists in this
component or one of its parents, undef otherwise.

=item name

Returns a short name of the component.  For file-based components this
is the filename without the path. For subcomponents this is the name
specified in C<E<lt>%defE<gt>>. Undefined for anonymous components.

=item object_file

Returns the object filename for this component.

=item owner

Defined only for subcomponents; returns the component that this
subcomponent was defined in.

=item parent

Returns the parent of this component for inheritance purposes, by
default the nearest C<autohandler> in or above the component's directory.
Can be changed via the C<inherit> flag.

=item path

Returns the entire path of this component, relative to the component root.

=item scall_method (name, args...)

Like L<Component-E<gt>call_method (name, args...)|call_method (name,
args...)>, but returns the method output as a string instead of
printing it. (Think sprintf versus printf.) The method's return value,
if any, is discarded.

=item subcomps

With no arguments, returns a hashref containing the subcomponents defined
in this component, with names as keys and component objects as values.
With one argument, returns the subcomponent of that name
or undef if no such subcomponent exists. e.g.

    if (my $subcomp = $comp->subcomps('.link')) {
        ...
    }

Subcomponents are declared in C<E<lt>%defE<gt>> sections.

=item methods

This method works exactly like the subcomps method, but it returns
methods, not subcomponents.

Methods are declared in C<E<lt>%methodE<gt>> sections.

=item title

Returns a printable string denoting this component.  It is intended to
uniquely identify a component within a given interpreter although this
is not 100% guaranteed. Mason uses this string in error messages,
among other places.

For file-based components this is the component path.  For
subcomponents this is "parent_component_path:subcomponent_name". For
anonymous components this is a unique label like "[anon 17]".

=back

=head1 FILE-BASED METHODS

The following methods apply only to file-based components (those
loaded from source or object files). They return undef for other
component types.

=over

=item source_file

Returns the source filename for this component.

=item source_dir

Returns the directory of the source filename for this component.

=back

=head1 SEE ALSO

L<HTML::Mason|HTML::Mason>,
L<HTML::Mason::Devel|HTML::Mason::Devel>,
L<HTML::Mason::Request|HTML::Mason::Request>

=cut
