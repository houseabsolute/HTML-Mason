# Copyright (c) 1998-2000 by Jonathan Swartz. All rights reserved.
# This program is free software; you can redistribute it and/or modify it
# under the same terms as Perl itself.

package HTML::Mason::Compiler;

use strict;

use HTML::Mason::Component::FileBased;
use HTML::Mason::Component::Subcomponent;

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

my %fields =
    ( default_escape_flags => '',
      lexer_class => 'HTML::Mason::Lexer',
      preprocess => undef,
      postprocess_perl => undef,
      postprocess_text => undef,
    );

my %top_level_only_block = map { $_ => 1 } qw( cleanup once shared );
my %valid_comp_flag = map { $_ => 1 } qw( inherit );
my %valid_escape_flag = map { $_ => 1 } qw( h n u );

# called from subclasses to set defaults and to make the lexer object
sub _init
{
    my $self = shift;
    my %p = @_;

    foreach ( keys %fields )
    {
	$self->$_( exists $p{$_} ? $p{$_} : $fields{$_} ) unless $self->$_();
    }

    $self->set_allowed_globals( exists $p{allowed_globals} ? @{ $p{allowed_globals} } : () );

    require HTML::Mason::Lexer unless $p{lexer_class};

    #
    # I want the compiler class to be the sole interface to compiling.
    # The lexer class should be as hidden as possible.  This means
    # that parameters intended for the compiler and/or lexer all go to
    # the compiler and get filtered here.
    #
    my %lexer_params;
    foreach ( $self->lexer_class->parameters )
    {
	$lexer_params{$_} = delete $p{$_} if exists $p{$_};
    }

    $self->lexer( $self->lexer_class->new( compiler => $self,
					   %lexer_params ) );
}

sub set_allowed_globals
{
    my $self = shift;
    my @globals = @_;

    if ( my @bad = grep { ! /^[\$@%]/ } @globals )
    {
	HTML::Mason::Exception::Params->throw( error => "allowed_globals: bad parameters '@bad', must begin with one of $, @, %\n" );
    }

    $self->{allowed_globals} = [ keys %{ { map { $_ => 1 } @globals } } ];
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

    $p{text} =~ s/\\\n//g;

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
    $p{escape} .= $self->default_escape_flags;
    if ( $p{escape} )
    {
	my @flags = keys %{ { map { $_ => 1 } split //, $p{escape} } };
	foreach (@flags)
	{
	    HTML::Mason::Exception::Compiler->throw( error => "invalid <% %> escape flag: '$_'" )
		unless $valid_escape_flag{$_};
	    @flags = () if $_ eq 'n';
	}
	my $flags = join ', ', map { "'$_'" } @flags;
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

    my $line = $self->lexer->line_count;
    my $file = $self->lexer->name;

    $self->{current_comp}{body} .= join '', "#line $line $file\n", shift;
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



