# Copyright (c) 1998-2001 by Jonathan Swartz. All rights reserved.
# This program is free software; you can redistribute it and/or modify it
# under the same terms as Perl itself.

package HTML::Mason::Component;
require 5.004;

use strict;
use File::Basename;
use File::Spec;
use HTML::Mason::Tools qw(compress_path);
use Params::Validate qw(:all);
use vars qw($AUTOLOAD);

use HTML::Mason::MethodMaker
    ( read_only => [ qw( code
			 create_time
			 declared_args
			 fq_path
			 inherit_path
			 inherit_start_path
			 interp
			 object_size
			 parser_version ) ],

      read_write => [ qw ( dynamic_subs_request
			   mfu_count ) ]
      );

# I'm not sure which of these need to be non-optional.
my %valid_params =
    (
     attr               => {type => HASHREF, optional => 1},
     code               => {type => CODEREF, optional => 1},
     create_time        => {type => SCALAR,  optional => 1},
     declared_args      => {type => HASHREF, default => {}},
     dynamic_subs_init  => {type => CODEREF, optional => 1},
     flags              => {type => HASHREF, default => {}},
     fq_path            => {type => SCALAR,  optional => 1},
     inherit_path       => {type => SCALAR,  optional => 1},
     inherit_start_path => {type => SCALAR,  optional => 1},
     interp             => {isa => 'HTML::Mason::Interp', optional => 1},
     methods            => {type => HASHREF, default => {}},
     mfu_count          => {type => SCALAR,  default => 0},
     object_size        => {type => SCALAR,  default => 0},
     parser_version     => {type => SCALAR,  optional => 1},
     subcomps           => {type => HASHREF, default => {}},
     source_ref_start   => {type => SCALAR,  optional => 1},   # legacy, left in for pre-0.8 obj files
    );

sub valid_params { \%valid_params }

my $comp_count = 0;

sub new
{
    my $class = shift;
    my $self = bless {
		      validate(@_, $class->valid_params),
		      designator => undef,
		     }, $class;

    # Assign defaults.
    $self->{designator} = "[anon ". ++$comp_count . "]" if !defined($self->{fq_path});
    
    # Initialize subcomponent and method properties.
    while (my ($name,$c) = each(%{$self->{subcomps}})) {
	$c->assign_subcomponent_properties($self,$name);
    }
    while (my ($name,$c) = each(%{$self->{methods}})) {
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

    # Assign inheritance properties
    if (exists($self->{flags}->{inherit})) {
	if (defined($self->{flags}->{inherit})) {
	    $self->{inherit_path} = $interp->process_comp_path($self->{flags}->{inherit},$self->dir_path);
	}
    } elsif (defined($interp->autohandler_name)) {
	if ($interp->allow_recursive_autohandlers) {
	    if ($self->name eq $interp->autohandler_name) {
		unless ($self->dir_path eq '/') {
		    $self->{inherit_start_path} = dirname($self->dir_path);
		}
	    } else {
		$self->{inherit_start_path} = $self->dir_path;
	    }
	} else {
	    unless ($self->name eq $interp->autohandler_name) {
		$self->{inherit_path} = $self->dir_path."/".$interp->autohandler_name;
	    }
	}
    }
}

sub run {
    my $self = shift;

    #
    # CODEREF_NAME maps component coderefs to component names (for profiling)
    #
    $HTML::Mason::CODEREF_NAME{$self->{code}} = $self->source_file
	if $::opt_P && defined($self->source_file);

    $self->{mfu_count}++;

    # Note: this must always preserve calling wantarray() context
    return $self->{code}->(@_);
}

sub dynamic_subs_init {
    my $self = shift;

    $self->{dynamic_subs_hash} = $self->{dynamic_subs_init}->();
    HTML::Mason::Exception->throw( error => "could not process <%shared> section (does it contain a return()?)" )
	unless ref($self->{dynamic_subs_hash}) eq 'HASH';
}

sub run_dynamic_sub {
    my ($self, $key, @args) = @_;

    HTML::Mason::Exception->throw( error => "call_dynamic: assert error - could not find code for key $key in component " . $self->title )
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
sub title { return $_[0]->{designator} }
sub name { return $_[0]->{designator} }
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
# Get attribute by name
#
sub attr {
    my ($self,$name) = @_;
    my $value;
    if ($self->_locate_inherited('attr',$name,\$value)) {
	return $value;
    } else {
	HTML::Mason::Exception->throw( error => "no attribute '$name' for component " . $self->title );
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
	$HTML::Mason::Commands::m->comp({base_comp=>$self},$method,@args);
    } else {
	HTML::Mason::Exception->throw( error => "no method '$name' for component " . $self->title );
    }
}

#
# Like call method, but return component output.
#
sub scall_method {
    my ($self,$name,@args) = @_;
    my $method;
    if ($self->_locate_inherited('methods',$name,\$method)) {
	$HTML::Mason::Commands::m->scomp({base_comp=>$self},$method,@args);
    } else {
	HTML::Mason::Exception->throw( error => "no method '$name' for component " . $self->title );
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
	HTML::Mason::Exception->throw( error => "inheritance chain length > 32 (infinite inheritance loop?)" )
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
	HTML::Mason::Exception->throw( error => "invalid flag: $name" );
    }
}

#
# Return parent component according to inherit flag
#
sub parent {
    my ($self) = @_;
    my $interp = $self->interp;
    if ($self->inherit_path) {
	return $interp->load($self->inherit_path);
    } elsif ($self->inherit_start_path) {
	return $interp->find_comp_upwards($self->inherit_start_path,$interp->autohandler_name);
    } else {
	return undef;
    }
}

#
# Accessors for various files associated with component
#
sub object_file {
    my $self = shift;
    return $self->persistent ?
	File::Spec->catdir( $self->interp->object_dir, $self->fq_path ) :
	undef;
}

sub cache_file {
    my $self = shift;
    return $self->persistent ?
	File::Spec->catdir( $self->interp->data_cache_dir, compress_path($self->fq_path) ) :
	undef;
}

1;
