# Copyright (c) 1998-99 by Jonathan Swartz. All rights reserved.
# This program is free software; you can redistribute it and/or modify it
# under the same terms as Perl itself.

package HTML::Mason::Component;
require 5.004;
require Exporter;
@ISA = qw(Exporter);
@EXPORT = qw();
@EXPORT_OK = qw();

use strict;
use HTML::Mason::Tools qw(read_file);
use vars qw($AUTOLOAD);

my %fields =
    (
     code => undef,
     create_time => undef,
     declared_args => undef,
     fq_path => undef,
     interp => undef,
     parser_version => undef,
     run_count => 0,
     source_ref_start => undef,
     subcomps => undef,
     );

# Create accessor routines
foreach my $f (keys %fields) {
    no strict 'refs';
    next if ($f =~ /^subcomps$/);
    *{$f} = sub {my $s=shift; return @_ ? ($s->{$f}=shift) : $s->{$f}};
}

my $compCount = 0;

sub new
{
    my $class = shift;
    my $self = {
	%fields,
	designator=>undef
    };
    my (%options) = @_;
    while (my ($key,$value) = each(%options)) {
	if (exists($fields{$key})) {
	    $self->{$key} = $value;
	} else {
	    die "HTML::Mason::Component::new: invalid option '$key'\n";
	}
    }
    bless $self, $class;

    # Assign defaults.
    $self->{designator} = "[anon ". ++$compCount . "]" if !defined($self->{fq_path});
    $self->{declared_args} = {} if !defined($self->{declared_args});
    $self->{subcomps} = {} if !defined($self->{subcomps});
    
    # Initialize subcomponent properties.
    while (my ($name,$c) = each(%{$self->{subcomps}})) {
	$c->assign_subcomponent_properties($self,$name);
    }

    return $self;
}

#
# Assign interpreter and, optionally, new fq_path to component.
# Must be run before component will fully work.
#
sub assign_runtime_properties {
    my ($self,$interp,$fq_path) = @_;
    $self->{interp} = $interp;
    $self->{fq_path} = $fq_path if $fq_path;
    foreach my $c (values(%{$self->{subcomps}})) {
	$c->assign_runtime_properties($interp);
    }
}

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
sub title { return $_[0]->{designator} }
sub name { return $_[0]->{designator} }
sub path { return undef }
sub dir_path { return undef }

#
# Is this our first time being run?
#
sub first_time { return $_[0]->{run_count} <= 1 }

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
# Accessors for various files associated with component
#
sub object_file { my $self = shift; return ($self->persistent) ? ($self->interp->object_dir . "/" . $self->fq_path) : undef }
sub cache_file { my $self = shift; return ($self->persistent) ? ($self->interp->data_cache_filename($self->fq_path)) : undef }

#
# Returns source text stored at bottom of object file
#
sub source_ref_text
{
    my ($self) = @_;
    return undef unless defined($self->{source_ref_start});
    my $content = read_file($self->object_file);
    return substr($content,$self->{source_ref_start});
}

1;
