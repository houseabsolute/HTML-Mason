# Copyright (c) 1998-2001 by Jonathan Swartz. All rights reserved.
# This program is free software; you can redistribute it and/or modify it
# under the same terms as Perl itself.

package HTML::Mason::Compiler;

use strict;

use HTML::Mason::Component::FileBased;
use HTML::Mason::Component::Subcomponent;
use HTML::Mason::Lexer;

use HTML::Mason::Exceptions;
use Params::Validate qw(:all);
Params::Validate::set_options( on_fail => sub { HTML::Mason::Exception::Params->throw( error => shift ) } );

use HTML::Mason::MethodMaker
    ( read_write => [ qw( default_escape_flags
                          lexer
                          lexer_class
                          preprocess
                          postprocess_perl
                          postprocess_text
                        )
		    ],
    );

my %valid_params = 
    (
     allowed_globals      => { parse => 'list',   type => ARRAYREF, default => [] },
     default_escape_flags => { parse => 'string', type => SCALAR,   default => '' },
     lexer_class          => { parse => 'string', isa => 'HTML::Mason::Lexer', default => 'HTML::Mason::Lexer' },
     preprocess           => { parse => 'code',   type => CODEREF,  optional => 1 },
     postprocess_perl     => { parse => 'code',   type => CODEREF,  optional => 1 },
     postprocess_text     => { parse => 'code',   type => CODEREF,  optional => 1 },
    );

sub valid_params { \%valid_params }

sub new
{
    my $class = shift;
    my $self = bless {validate(@_, $class->valid_params)}, $class;

    $self->_init;

    return $self;
}

my %top_level_only_block = map { $_ => 1 } qw( cleanup once shared );
my %valid_comp_flag = map { $_ => 1 } qw( inherit );
my %valid_escape_flag = map { $_ => 1 } qw( h n u );

# called from subclasses to set defaults and to make the lexer object
sub _init
{
    my $self = shift;
    my %p = validate(@_, $self->valid_params);

    foreach ( keys %p )
    {
	$self->$_( $p{$_} ) unless $self->$_();
    }

    $self->set_allowed_globals( exists $p{allowed_globals} ? @{ $p{allowed_globals} } : () );

    #
    # I want the compiler class to be the sole interface to compiling.
    # The lexer class should be as hidden as possible.  This means
    # that parameters intended for the compiler and/or lexer all go to
    # the compiler and get filtered here.
    #
    my %lexer_params;
    my @lexer_param_keys = keys %{$self->lexer_class->valid_params};
    @lexer_params{@lexer_param_keys} = delete @p{@lexer_param_keys};

    $self->lexer( $self->lexer_class->new(%lexer_params, compiler => $self) );
}

sub set_allowed_globals
{
    my $self = shift;
    my @globals = @_;

    if ( my @bad = grep { ! /^[\$@%]/ } @globals )
    {
	HTML::Mason::Exception::Params->throw( error => "allowed_globals: bad parameters '@bad', must begin with one of $, @, %\n" );
    }

    $self->{allowed_globals} = [ keys %{ { map { $_ => 1 } @globals, @{ $self->{allowed_globals} } } } ];
}

sub allowed_globals
{
    return @{ shift->{allowed_globals} };
}

sub compile
{
    my $self = shift;
    my %p = @_;

    validate( @_, { comp => { type => SCALAR },
		    name => { type => SCALAR },
		  } );

    # Preprocess the script.  The preprocessor routine is handed a
    # reference to the entire script.
    if ($self->preprocess)
    {
	eval { $self->preprocess->( \$p{comp} ) };
	HTML::Mason::Exception::Compiler->throw( error => "Error during custom preprocess step: $@" )
	    if $@;
    }

    $self->lexer->lex( comp => $p{comp}, name => $p{name} );

    return $self->compiled_component;
}

sub start_component
{
    my $self = shift;

    HTML::Mason::Exception::Compiler->throw( error => "Cannot start a component while already compiling a component" )
        if $self->{current_comp};

    $self->{in_main} = 1;
    $self->{comp_with_content_stack} = [];

    $self->_init_comp_data($self);

    $self->{current_comp} = $self;
}

sub _init_comp_data
{
    my $self = shift;
    my $data = shift;

    $data->{body} = '';

    foreach ( $self->lexer->named_block_types )
    {
	$data->{$_} = {};
    }

    $data->{args} = [];
    $data->{flags} = {};
    $data->{attr} = {};

    foreach ( $self->lexer->simple_block_types )
    {
	$data->{blocks}{$_} = [];
    }
}

sub end_component
{
    my $self = shift;

    HTML::Mason::Exception::Syntax->throw( error => "Not enough ending component with content ending tags found" )
	if @{ $self->{comp_with_content_stack} };

    $self->{current_comp} = undef;
}

sub start_block
{
    my $self = shift;
    my %p = @_;

    HTML::Mason::Exception::Syntax->throw( error => "Cannot define a $p{block_type} section inside a method or subcomponent" )
	 if $top_level_only_block{ $p{block_type} } && ! $self->{in_main};

    HTML::Mason::Exception::Syntax->throw( error => "Cannot nest a $p{block_type} inside a $self->{in_block} block: " . $self->lexer->name )
	 if $self->{in_block};

    $self->{in_block} = $p{block_type};
}

sub raw_block
{
    my $self = shift;
    my %p = @_;

    my $method = "$p{block_type}_block";
    return $self->$method(%p) if $self->can($method);

    push @{ $self->{current_comp}{blocks}{ $p{block_type} } }, $p{block};
}

sub perl_block
{
    my $self = shift;
    my %p = @_;

    $self->_add_body_code( $p{block} );
}

sub text
{
    my $self = shift;
    my %p = @_;

    $self->postprocess_text->(\$p{text}) if $self->postprocess_text;

    $p{text} =~ s,(['\\]),\\$1,g;

    $self->_add_body_code( "\$_out->( '$p{text}' );\n" ) if $p{text} ne '';
}

sub text_block
{
    my $self = shift;
    my %p = @_;

    $p{block} =~ s,(['\\]),\\$1,g;

    $self->_add_body_code( "\$_out->( '$p{block}' );\n" ) if $p{block} ne '';
}

sub end_block
{
    my $self = shift;
    my %p = @_;

    HTML::Mason::Exception::Syntax->throw( error => "end of $p{block_type} encountered while in $self->{in_block} block" )
	unless $self->{in_block} eq $p{block_type};

    $self->{in_block} = undef;
}

sub variable_declaration
{
    my $self = shift;
    my %p = @_;

    HTML::Mason::Exception::Compiler->throw( error => "variable_declaration called inside a $p{block_type} block")
	unless $p{block_type} eq 'args';

    my $arg = "$p{type}$p{name}";

    HTML::Mason::Exception::Compiler->throw( "$arg already defined" )
        if grep { "$_->{type}$_->{name}" eq "$p{type}$p{name}" } @{ $self->{current_comp}{args} };

    push @{ $self->{current_comp}{args} }, { type => $p{type},
					     name => $p{name},
					     default => $p{default} };
}

sub key_value_pair
{
    my $self = shift;
    my %p = @_;

    HTML::Mason::Exception::Compiler->throw( error => "key_value_pair called inside a $p{block_type} block")
	unless $p{block_type} eq 'flags' || $p{block_type} eq 'attr';

    my $type = $p{block_type} eq 'flags' ? 'flag' : 'attribute';
    HTML::Mason::Exception::Compiler->throw( error => "$p{key} $type already defined" )
	if exists $self->{current_comp}{ $p{block_type} }{ $p{key} };

    $self->{current_comp}{ $p{block_type} }{ $p{key} } = $p{value}
}

sub start_named_block
{
    my $self = shift;
    my %p = @_;

    HTML::Mason::Exception::Syntax->throw( "Cannot define a $p{type} inside a method or subcomponent" )
        unless $self->{in_main};

    $self->{in_main}--;

    $self->{ $p{block_type} }{ $p{name} } = {};
    $self->_init_comp_data( $self->{ $p{block_type} }{ $p{name} } );
    $self->{current_comp} = $self->{ $p{block_type} }{ $p{name} };
}

sub end_named_block
{
    my $self = shift;

    $self->{in_main}++;

    $self->{current_comp} = $self;
}

sub substitution
{
    my $self = shift;
    my %p = @_;

    my $text = $p{substitution};
    if ( $p{escape} || $self->default_escape_flags )
    {
	my %flags;
	%flags = map { $_ => 1 } split //, $p{escape} if $p{escape};
	foreach (keys %flags)
	{
	    HTML::Mason::Exception::Compiler->throw( error => "invalid <% %> escape flag: '$_'" )
		unless $valid_escape_flag{$_};
	}
	unless ( delete $flags{n} )
	{
	    foreach ( split //, $self->default_escape_flags )
	    {
		$flags{$_} = 1;
	    }
	}
	my $flags = join ', ', map { "'$_'" } keys %flags;
	$text = "\$_escape->( $text, $flags )";
    }

    my $code = "\$_out->( $text );\n";

    $self->postprocess_perl->(\$code) if $self->postprocess_perl;

    $self->_add_body_code($code);
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

    my $code = "\$m->comp( $call );\n";
    $self->postprocess_perl->(\$code) if $self->postprocess_perl;

    $self->_add_body_code($code);
}

sub component_content_call
{
    my $self = shift;
    my %p = @_;

    my $call = $p{call};
    for ($call) { s/^\s+//; s/\s+$//; }
    push @{ $self->{comp_with_content_stack} }, $call;

    my $code = "\$m->comp( { content => sub {\nmy \$_out = \$m->current_sink;\n";

    $self->postprocess_perl->(\$code) if $self->postprocess_perl;

    $self->_add_body_code($code);
}

sub component_content_call_end
{
    my $self = shift;

    HTML::Mason::Exception::Compiler->throw(error=>"found component with content ending tag but no beginning tag")
	unless @{ $self->{comp_with_content_stack} };

    my $call = pop @{ $self->{comp_with_content_stack} };

    if ( $call =~ m,^[A-Za-z0-9/_.],)
    {
	my $comma = index($call, ',');
	$comma = length $call if $comma == -1;
	(my $comp = substr($call, 0, $comma)) =~ s/\s+$//;
	$call = "'$comp'" . substr($call, $comma);
    }

    my $code = "} }, $call );\n";

    $self->postprocess_perl->(\$code) if $self->postprocess_perl;

    $self->_add_body_code($code);
}

sub perl_line
{
    my $self = shift;
    my %p = @_;

    my $code = "$p{line}\n";

    $self->postprocess_perl->(\$code) if $self->postprocess_perl;

    $self->_add_body_code($code);
}

sub _add_body_code
{
    my $self = shift;
    my $code = shift;

    my $line = $self->lexer->line_count;
    my $file = $self->lexer->name;
    my $comment = "#line $line $file\n";

    $self->{current_comp}{body} .= join '', $comment, $code;
}

sub dump
{
    my $self = shift;

    print "Main component\n";

    $self->_dump_data( $self );

    foreach ( keys %{ $self->{def} } )
    {
	print "  Subcomponent $_\n";
	$self->_dump_data( $self->{def}{$_}, '  ' );
    }

    foreach ( keys %{ $self->{method} } )
    {
	print "  Methods $_\n";
	$self->_dump_data( $self->{method}{$_}, '  ');
    }
}

sub _dump_data
{
    my $self = shift;
    my $data = shift;
    my $indent = shift || '';

    if ( @{ $data->{args} } )
    {
	print "$indent  args\n";
	foreach ( @{ $data->{args} } )
	{
	    print "$indent    $_->{type}$_->{name}";
	    print " => $_->{default}" if defined $_->{default};
	    print "\n";
	}
    }

    print "\n$indent  body\n";
    print "$data->{body}\n";
}

sub _blocks
{
    my $self = shift;

    return @{ $self->{current_comp}{blocks}{ shift() } };
}

1;

