# Copyright (c) 1998 by Jonathan Swartz. All rights reserved.
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
     # All components
     code => undef,
     create_time => undef,
     embedded => 0,
     file_based => 0,
     declared_args => undef,
     name => undef,
     parent_comp => undef,
     parent_path => undef,
     parser_version => undef,
     run_count => 0,
     subcomps => undef,
     title => undef,

     # File-based components
     comp_root => undef,
     data_dir => undef,
     path => undef,
     source_ref_start => undef,
     );

# Minor speedup: create anon. subs to reduce AUTOLOAD calls
foreach my $f (keys %fields) {
    no strict 'refs';
    *{$f} = sub {my $s=shift; return @_ ? ($s->{$f}=shift) : $s->{$f}};
}

my $compCount = 0;

sub new
{
    my $class = shift;
    my $self = {
	_permitted => \%fields,
	%fields,
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

    # Initialize subcomponent properties. Note that other properties
    # (parent_path, title) are initialized in assign_file_properties.
    while (my ($name,$c) = each(%{$self->{subcomps}})) {
	# I am a subcomponent
	$c->{embedded} = 1;
	
	# This is my parent
	$c->{parent_comp} = $self;
	
	# Access to the same subcomps and parent path
	$c->{subcomps} = $self->{subcomps};

	# Title is a combination of names
	$c->{name} = $name;
    }

    $self->{title} = "[anon ". ++$compCount . "]" if !defined($self->{title});
    
    return $self;
}

#
# Is this our first time being run?
#
sub first_time { return $_[0]->{run_count} <= 1 }

#
# For file-based components
#
sub assign_file_properties
{
    my ($self,$compRoot,$dataDir,$cacheDir,$path) = @_;
    ($self->{file_based},$self->{comp_root},$self->{data_dir},$self->{cache_dir},$self->{path},$self->{title}) =
	(1,$compRoot,$dataDir,$cacheDir,$path,$path);
    ($self->{parent_path}) = ($path =~ /^(.*)\/[^\/]+$/);
    ($self->{name}) = ($path =~ /([^\/]+)$/);

    # Initialize subcomponent properties
    while (my ($name,$c) = each(%{$self->{subcomps}})) {
	$c->{title} = "$path:$name";
	$c->{parent_path} = $self->{parent_path};
    }
}

sub source_file { my $self = shift; return ($self->file_based) ? ($self->comp_root . $self->path) : undef }
sub object_file { my $self = shift; return ($self->file_based) ? ($self->data_dir . "/obj/" . $self->path) : undef }
sub cache_file { my $self = shift; return ($self->file_based) ? ($self->cache_dir . $self->path) : undef }

sub source_ref_text
{
    my ($self) = @_;
    return undef if !$self->file_based || !defined($self->{source_ref_start});
    my $content = read_file($self->object_file);
    return substr($content,$self->{source_ref_start});
}

1;

__END__
