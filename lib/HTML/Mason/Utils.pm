# Copyright (c) 1998-2001 by Jonathan Swartz. All rights reserved.
# This program is free software; you can redistribute it and/or modify it
# under the same terms as Perl itself.

#
# Miscellaneous Mason-related utilities expected to be used by
# external applications.
#

package HTML::Mason::Utils;

use HTML::Mason::Tools qw(compress_path);
use strict;

require Exporter;

use vars qw(@ISA @EXPORT_OK);

@ISA = qw(Exporter);
@EXPORT_OK = qw(data_cache_namespace cgi_request_args create_subobjects);

# The create_subobjects() function lets one object (e.g. Compiler)
# transparently create another (e.g. Lexer) by passing creator
# parameters through to the created object.
#
# Any auto-created objects should be declared in a class's
# creates_objects() method.  creates_objects() returns a hashref whose
# keys are parameter names representing objects which can be
# auto-created.  For instance, the key 'lexer' indicates that a
# 'lexer' parameter should be silently passed through, and a
# 'lexer_package' parameter will trigger the creation of an object
# whose class is specified by the value.  If no value is present
# there, the value of 'lexer' in the creates_objects() method is used.
# If no value is present there, no sub-object is created.
#
# We return the list of parameters for the creator.  If subobjects
# were auto-created, their creation parameters aren't included in the
# return value.  This lets the creator be totally ignorant of the
# creation parameters of any objects it creates.

sub create_subobjects
{
    my ($package, %args) = @_;
    #warn "--- creating $package, received args (@{[%args]})\n";

    while (my ($name, $default_pack) = each %{$package->creates_objects}) {
	# If we've been handed this kind of object, just use it
	next if exists $args{$name};
	
	# Else, extract the parameters for its creation, then create it
	my $c_package = delete $args{"${name}_package"} || $default_pack;
	next unless $c_package;  # might be optional
	#warn "---- going to create '$c_package'\n";

	# Extract from %args the parameters for creating a $c_package
	my %these_args = get_subargs($c_package, \%args);
	delete @args{keys %these_args};
	$args{$name} = $c_package->new(%these_args);
	#warn "---- created '$name' = $args{$name}\n";
    }
    
    return %args;
}

sub get_subargs {
    my ($package, $superargs) = @_;
    my %subargs;

    while (my ($name, $default) = each %{$package->creates_objects}) {
	if (exists $superargs->{$name}) {  # Not creating it, so don't accept its params
	    $subargs{$name} = $superargs->{$name};

	} elsif (exists $superargs->{"${name}_package"}) { # Accept all its params
	    %subargs = (%subargs, get_subargs($superargs->{"${name}_package"}, $superargs));

	} elsif ($default) { # Accept params for default package
	    %subargs = (%subargs, get_subargs($default, $superargs));
	}
    }

    # Finally, accept our own parameters too.
    foreach my $key (keys %{$package->valid_params}) {
	$subargs{$key} = $superargs->{$key} if exists $superargs->{$key};
    }

    return %subargs;
}

sub data_cache_namespace
{
    my ($path) = @_;
    return compress_path($path);
}

sub cgi_request_args
{
    my ($q, $method) = @_;

    my %args;

    # Checking scalar $r->args when the method is POST is important
    # because otherwise ->url_param returns a parameter named
    # 'keywords' with a value of () (empty array).  This is apparently
    # a feature related to <ISINDEX> queries or something (see the
    # CGI.pm) docs.  It makes my head heart. - dave
    my @methods = $method eq 'GET' || ! $ENV{QUERY_STRING} ? ( 'param' ) : ( 'param', 'url_param' );
    foreach my $key ( map { $q->$_() } @methods ) {
	next if exists $args{$key};
	my @values = map { $q->$_($key) } @methods;
	$args{$key} = @values == 1 ? $values[0] : \@values;
    }

    return %args;
}
