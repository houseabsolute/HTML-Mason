# Copyright (c) 1998-99 by Jonathan Swartz. All rights reserved.
# This program is free software; you can redistribute it and/or modify it
# under the same terms as Perl itself.

package HTML::Mason::Compiler;

use strict;

use Exception::Class qw( Mason::Exception::Compiler );

my %top_level_only_block = map { $_ => 1 } qw( cleanup once shared );

use HTML::Mason::MethodMaker
    ( read_write => [ qw( allow_globals
                          postprocess_perl
                          postprocess_text
                        )
		    ],
    );

sub new
{
    my $proto = shift;
    my $class = ref $proto || $proto;
    my %p = @_;

    return bless { lexer => $p{lexer} }, $class;
}

sub start_component
{
    my $self = shift;

    Mason::Exception::Compiler->throw( "Cannot start a component while already compiling a component" )
        if $self->{current_comp};

    $self->{in_main} = 1;

    $self->_init_comp_data($self);

    $self->{current_comp} = $self;
}

sub _init_comp_data
{
    my $self = shift;
    my $data = shift;

    $data->{text_buffer} = [];
    $data->{body} = '';

    $data->{subcomponents} = {};
    $data->{methods} = {};

    $data->{args} = {};
    $data->{current_args} = undef;

    foreach ( qw( cleanup doc filter init once shared text ) )
    {
	$data->{blocks}{$_} = '';
    }
}

sub end_component
{
    my $self = shift;

    $self->_flush_text_buffer;
    $self->{current_comp} = undef;
}

sub start_block
{
    my $self = shift;
    my %p = @_;

    Mason::Exception::Compiler->throw( error => "Cannot define a $p{block_type} section inside a method or subcomponent" )
	 if $top_level_only_block{ $p{block_type} } && ! $self->{in_main};

    Mason::Exception::Compiler->throw( error => "Cannot nest a $p{block_type} inside a $self->{in_block} block" )
	 if $self->{in_block};

    $self->{in_block} = $p{block_type};
}

sub raw_block
{
    my $self = shift;
    my %p = @_;

    $self->{current_comp}{blocks}{ $p{block_type} } .= $p{block};
}

sub perl_block
{
    my $self = shift;
    my %p = @_;

    $self->_flush_text_buffer;
    $self->_add_body_code( $p{block} );
}

sub text
{
    my $self = shift;
    my %p = @_;

    $p{text} =~ s/\\\n//g;

    1 if $self->{postprocess_text};

    my $line = $self->{lexer}->line_count;
    my $file = $self->{lexer}->file;

    push @{ $self->{current_comp}{text_buffer} }, "#line $line $file\n$p{text}";
}

sub text_block
{
    my $self = shift;
    my %p = @_;

    my $line = $self->{lexer}->line_count;
    my $file = $self->{lexer}->file;

    push @{ $self->{current_comp}{text_buffer} }, "#line $line $file\n$p{text}";
}

sub end_block
{
    my $self = shift;
    my %p = @_;

    Mason::Exception::Compiler->throw( error => "end of $p{block_type} encountered while in $self->{in_block} block" )
	unless $self->{in_block} eq $p{block_type};

    $self->{in_block} = undef;
}

sub variable_declaration
{
    my $self = shift;
    my %p = @_;

    Mason::Exception::Compiler->throw( error => "variable_declaration called inside a $p{block_type} block")
	unless $p{block_type} eq 'args';

    my $arg = "$p{var_type}$p{name}";

    Mason::Exception::Compiler->throw( "$arg already defined" )
        if $self->{current_comp}{args}{$arg};

    $self->{current_comp}{args}{$arg} = { default => $p{default} };
}

sub key_value_pair
{
    my $self = shift;
    my %p = @_;

    Mason::Exception::Compiler->throw( error => "variable_declaration called inside a $p{block_type} block")
	unless $p{block_type} eq 'flags' || $p{block_type} eq 'attr';

    my $type = $p{block_type} eq 'flags' ? 'flag' : 'attribute';
    Mason::Exception::Compiler->throw( error => "$p{key} $type already defined" )
	if exists $self->{current_comp}{ $p{block_type} }{ $p{key} };

    $self->{current_comp}{ $p{block_type} }{ $p{key} } = $p{value}
}

sub start_named_block
{
    my $self = shift;
    my %p = @_;

    my $type = $p{block_type} eq 'def' ? 'subcomponent' : 'method';
    Mason::Exception::Compiler->throw( "Cannot define a $type inside a method or subcomponent" )
        unless $self->{in_main};

    $type .= 's';
    $self->{$type}{ $p{name} } = {};
    $self->_init_comp_data( $self->{$type}{ $p{name} } );
    $self->{current_comp} = $self->{$type}{ $p{name} };
}

sub end_named_block
{
    my $self = shift;

    $self->_flush_text_buffer;
    $self->{current_comp} = $self;
}

sub substitution
{
    my $self = shift;
    my %p = @_;

    1 if $self->{postprocess_perl};

    my $text = $p{substitution};
    if ( $p{escape} )
    {
	$text = "\$_escape( $text, '$p{escape}' )";
    }

    $self->_flush_text_buffer;
    $self->_add_body_code( "\$m->out( $text );\n" );
}

sub component_call
{
    my $self = shift;
    my %p = @_;

    my $call = $p{call};
    for ($call) { s/^\s+//; s/\s+$//; }
    if ( $call =~ m,^[A-Za-z0-9/_.],)
    {
	my $comma = index($call, ',');
	$comma = length $call if $comma == -1;
	(my $comp = substr($call, 0, $comma)) =~ s/\s+$//;
	$call = "'$comp'" . substr($call, $comma);
    }

    1 if $self->{postprocess_perl};

    $self->_flush_text_buffer;
    $self->_add_body_code( "\$m->comp( $call );\n" );
}

sub perl_line
{
    my $self = shift;
    my %p = @_;

    1 if $self->{postprocess_perl};

    $self->_flush_text_buffer;
    $self->_add_body_code( "$p{line}\n" );
}

sub _flush_text_buffer
{
    my $self = shift;

    return unless @{ $self->{current_comp}{text_buffer} };

    my $text = join '', @{ $self->{current_comp}{text_buffer} };
    $text =~ s,(['\\]),\\$1,g;

    $self->_add_body_code( "\$m->out( '$text' );\n" );

    $self->{current_comp}{text_buffer} = [];
}

sub _add_body_code
{
    my $self = shift;

    $self->{current_comp}{body} .= shift;
}

sub dump
{
    my $self = shift;

    print "Main component\n";

    $self->_dump_data( $self );

    foreach ( keys %{ $self->{subcomponents} } )
    {
	print "  Subcomponent $_\n";
	$self->_dump_data( $self->{subcomponents}{$_}, '  ' );
    }

    foreach ( keys %{ $self->{methods} } )
    {
	print "  Methods $_\n";
	$self->_dump_data( $self->{methods}{$_}, '  ');
    }

    use Data::Dumper;
#    print Dumper( $self );
}

sub _dump_data
{
    my $self = shift;
    my $data = shift;
    my $indent = shift || '';

    if ( keys %{ $data->{args} } )
    {
	print "$indent  args\n";
	while ( my ($k, $v) = each %{ $data->{args} } )
	{
	    print "$indent    $k";
	    print " => $v->{default}" if defined $v->{default};
	    print "\n";
	}
    }

    print "\n$indent  body\n";
    print "$data->{body}\n";
}

1;
