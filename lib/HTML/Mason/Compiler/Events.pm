# Copyright (c) 1998-99 by Jonathan Swartz. All rights reserved.
# This program is free software; you can redistribute it and/or modify it
# under the same terms as Perl itself.

package HTML::Mason::Compiler::Events;

use strict;

use base qw( HTML::Mason::Compiler );


sub new
{
    my $proto = shift;
    my $class = ref $proto || $proto;

    return bless {}, $class;
}

sub start_component
{
    my $self = shift;

    print "starting component\n";
}

sub end_component
{
    my $self = shift;

    print "ending component\n";
}

sub text
{
    my $self = shift;
    my %p = @_;

    print "TEXT\n$p{text}\n";
}

sub start_args
{
    my $self = shift;

    print "ARGS\n";
}

sub end_args
{
    my $self = shift;

    print "/ARGS\n";
}

sub start_cleanup
{
    my $self = shift;

    print "CLEANUP\n";
}

sub end_cleanup
{
    my $self = shift;

    print "/CLEANUP\n";
}

sub start_doc
{
    my $self = shift;

    print "DOC\n";
}

sub end_doc
{
    my $self = shift;

    print "/DOC\n";
}

sub start_filter
{
    my $self = shift;

    print "FILTER\n";
}

sub end_filter
{
    my $self = shift;

    print "/FILTER\n";
}

sub start_init
{
    my $self = shift;

    print "INIT\n";
}

sub end_init
{
    my $self = shift;

    print "/INIT\n";
}

sub start_once
{
    my $self = shift;

    print "ONCE\n";
}

sub end_once
{
    my $self = shift;

    print "/ONCE\n";
}

sub start_perl
{
    my $self = shift;

    print "PERL\n";
}

sub end_perl
{
    my $self = shift;

    print "/PERL\n";
}

sub start_shared
{
    my $self = shift;

    print "SHARED\n";
}

sub end_shared
{
    my $self = shift;

    print "/SHARED\n";
}

sub start_text
{
    my $self = shift;

    print "TEXT\n";
}

sub end_text
{
    my $self = shift;

    print "/TEXT\n";
}

sub raw_block
{
    my $self = shift;
    my %p = @_;

    print "  $p{text}\n";
}

sub variable_declaration
{
    my $self = shift;
    my %p = @_;

    $p{default} = '<undef>' unless defined $p{default};
    print "  $p{type} : $p{name} : $p{default}\n";
}

sub key_value_pair
{
    my $self = shift;
    my %p = @_;

    print "  $p{key} => $p{value}\n";
}

sub start_def
{
    my $self = shift;
    my %p = @_;

    print "DEF: $p{name}\n";
}

sub end_def
{
    my $self = shift;

    print "/DEF\n";
}

sub start_method
{
    my $self = shift;
    my %p = @_;

    print "METHOD: $p{name}\n";
}

sub end_method
{
    my $self = shift;

    print "/METHOD\n";
}

sub substitution
{
    my $self = shift;
    my %p = @_;

    print "SUB: $p{substitution}";
    print " | $p{escape}" if $p{escape};
    print "\n";
}

sub component_call
{
    my $self = shift;
    my %p = @_;

    print "CALL: $p{call}\n";
}

sub perl_line
{
    my $self = shift;
    my %p = @_;

    print "PLINE: $p{line}\n";
}


1;
