# Copyright (c) 1998-2002 by Jonathan Swartz. All rights reserved.
# This program is free software; you can redistribute it and/or modify it
# under the same terms as Perl itself.

package HTML::Mason::Cache::BaseCache;
use strict;

#
# Override to handle busy_lock and expire_if.
#
sub get
{
    my ($self, $key, %params) = @_;
    die "must specify key" unless defined($key);

    foreach my $param (keys(%params)) {
	unless ($param =~ /^(busy_lock|expire_if)$/) {
	    die "unknown param '$param'";
	}
    }

    $self->_conditionally_auto_purge_on_get();

    my $object = $self->get_object($key) or
	return undef;

    if (my $sub = $params{expire_if}) {
	$self->expire_if($key, $sub);
    }

    if (Cache::BaseCache::Object_Has_Expired($object))
    {
	if (my $duration = $params{busy_lock}) {
	    $object->set_expires_at(time + $duration);
	    $self->set_object($key, $object);
	} else {
	    $self->remove($key);
	}
	return undef;
    }

    return $object->get_data( );
}

sub get_created_at
{
    my ($self, $key) = @_;
    die "must specify key" unless defined($key);

    if (my $obj = $self->get_object($key)) {
	return $obj->get_created_at;
    } else {
	return undef;
    }
}

sub get_accessed_at
{
    my ($self, $key) = @_;
    die "must specify key" unless defined($key);

    if (my $obj = $self->get_object($key)) {
	return $obj->get_accessed_at;
    } else {
	return undef;
    }
}

sub get_expires_at
{
    my ($self, $key) = @_;
    die "must specify key" unless defined($key);

    if (my $obj = $self->get_object($key)) {
	return $obj->get_expires_at;
    } else {
	return undef;
    }
}

sub expire
{
    my ($self, $key) = @_;

    if (my $obj = $self->get_object($key)) {
	$obj->set_expires_at(time-1);
	$self->set_object($key, $obj);
    }
}

sub expire_if
{
    my ($self, $key, $sub) = @_;
    die "must specify subroutine" unless defined($sub) and ref($sub) eq 'CODE';

    if (my $obj = $self->get_object($key)) {
	my $retval = $sub->($obj);
	if ($retval) {
	    $self->expire($key);
	}
	return $retval;
    } else {
	return 1;
    }
}


1;

__END__

=head1 NAME

HTML::Mason::Cache::BaseCache - Mason extensions to Cache::BaseCache

=head1 DESCRIPTION

This is the base module for all cache implementations used in Mason.
It provides a few additional methods on top of Cache::BaseCache.

=head1 ADDITIONAL METHODS

=over

=item get (key, %params)

Like the basic method, returns the value associated with I<key> or
undef if it is non-existent or expired. This is extended
with the following optional name/value parameters:

=over

=item busy_lock => duration

If the value has expired, set its expiration time forward by the specified
I<duration> (instead of removing it from the cache) before returning undef.

=item expire_if => sub

If the cache object exists, call I<sub> with the cache object as a
single parameter. If I<sub> returns a true value, expire the value.

=back

=item get_created_at (key)

Returns the creation time of the object associated with I<key>, or undef
if the object does not exist.

=item get_accessed_at (key)

Returns the last accessed time of the object associated with I<key>, or undef
if the object does not exist.

=item get_expires_at (key)

Returns the expiration time of the object associated with I<key>, or undef
if the object does not exist.

=item expire (key)

Expires the cache object associated with I<key>, if the object exists.

=item expire_if (key, sub)

Expires the cache object associated with I<key> if I<sub> returns a
true value.  I<sub> is called with the cache object as a single
argument.  The return value of I<sub> is returned to the caller. If
the object does not exist, simply returns a true value without calling
I<sub>.

=back

=cut
