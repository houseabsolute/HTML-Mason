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
use File::Basename;
use HTML::Mason::Tools qw(read_file compress_path);
use vars qw($AUTOLOAD);

my %fields =
    (
     attr => undef,
     code => undef,
     create_time => undef,
     declared_args => undef,
     dynamic_subs_init => undef,
     flags => undef,
     fq_path => undef,
     inherit_path => undef,
     inherit_start_path => undef,
     interp => undef,
     methods => undef,
     mfu_count => 0,
     object_size => 0,
     parser_version => undef,
     run_count => 0,
     subcomps => undef,
     source_ref_start => undef   # legacy, left in for pre-0.8 obj files
     );

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
    $self->{flags} = {} if !defined($self->{flags});
    
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
# Get attribute by name
#
sub attr {
    my ($self,$name) = @_;
    my $value;
    if ($self->_locate_inherited('attr',$name,\$value)) {
	return $value;
    } else {
	die "no attribute '$name' for component ".$self->title;
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
	die "no method '$name' for component ".$self->title;
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
	die "no method '$name' for component ".$self->title;
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
	die "inheritance chain length > 32 (infinite inheritance loop?)" if ++$count > 32;
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
	die "invalid flag: $name";
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
sub object_file { my $self = shift; return ($self->persistent) ? ($self->interp->object_dir . $self->fq_path) : undef }
sub cache_file { my $self = shift; return ($self->persistent) ? ($self->interp->data_cache_dir . "/" . compress_path($self->fq_path)) : undef }

# Create generic read-write accessor routines
sub dynamic_subs_request { my $s=shift; return @_ ? ($s->{dynamic_subs_request}=shift) : $s->{dynamic_subs_request} }
sub dynamic_subs_hash { my $s=shift; return @_ ? ($s->{dynamic_subs_hash}=shift) : $s->{dynamic_subs_hash} }

#
# Create generic read-only accessor routines
#
sub code { return shift->{code} }
sub create_time { return shift->{create_time} }
sub declared_args { return shift->{declared_args} }
sub fq_path { return shift->{fq_path} }
sub inherit_path { return shift->{inherit_path} }
sub inherit_start_path { return shift->{inherit_start_path} }
sub interp { return shift->{interp} }
sub mfu_count { return shift->{mfu_count} }
sub object_size { return shift->{object_size} }
sub parser_version { return shift->{parser_version} }
sub run_count { return shift->{run_count} }
sub source_ref_start { return shift->{source_ref_start} }

1;
