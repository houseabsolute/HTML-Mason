# Copyright (c) 1998-2002 by Jonathan Swartz. All rights reserved.
# This program is free software; you can redistribute it and/or modify it
# under the same terms as Perl itself.

package HTML::Mason::Compiler;

use strict;

use HTML::Mason::Component::FileBased;
use HTML::Mason::Component::Subcomponent;
use HTML::Mason::Lexer;

use HTML::Mason::Exceptions( abbr => [qw(param_error compiler_error syntax_error)] );
use Params::Validate qw(:all);
Params::Validate::validation_options( on_fail => sub { param_error join '', @_ } );

use HTML::Mason::Container;
use base qw(HTML::Mason::Container);

BEGIN
{
    __PACKAGE__->valid_params
	(
	 allow_globals        => { parse => 'list',   type => ARRAYREF, default => [],
				   descr => "An array of names of Perl variables that are allowed globally within components" },
	 default_escape_flags => { parse => 'string', type => SCALAR,   default => '',
				   descr => "Escape flags that will apply by default to all Mason tag output" },
	 lexer                => { isa => 'HTML::Mason::Lexer',
				   descr => "A Lexer object that will scan component text during compilation" },
	 preprocess           => { parse => 'code',   type => CODEREF,  optional => 1,
				   descr => "A subroutine through which all component text will be sent during compilation" },
	 postprocess_perl     => { parse => 'code',   type => CODEREF,  optional => 1,
				   descr => "A subroutine through which all Perl code will be sent during compilation" },
	 postprocess_text     => { parse => 'code',   type => CODEREF,  optional => 1,
				   descr => "A subroutine through which all plain text will be sent during compilation" },
	);

    __PACKAGE__->contained_objects( lexer => 'HTML::Mason::Lexer' );
}

use HTML::Mason::MethodMaker
    ( read_write => [ map { [ $_ => __PACKAGE__->validation_spec->{$_} ] }
		     qw( default_escape_flags
                          lexer
                          preprocess
                          postprocess_perl
                          postprocess_text
                        )
		    ],
    );


sub new
{
    my $class = shift;
    my $self = $class->SUPER::new(@_);

    # Verify the validity of the global names
    $self->allow_globals( @{$self->{allow_globals}} );

    return $self;
}

sub object_id
{
    my $self = shift;

    # Can't use object keys because they stringify differently every
    # time the program is loaded, whether they are a reference to the
    # same object or not.
    my $spec = $self->validation_spec;
    my @id_keys = grep { ! exists $spec->{$_}{isa} && ! exists $spec->{$_}{can} } keys %$spec;

    my @vals;
    foreach my $k ( @id_keys )
    {
	push @vals, $k;

	# For coderef params we simply indicate whether or not it is
	# present.  This is better than simply ignoring them but not
	# by much.  We _could_ use B::Deparse's coderef2text method to
	# do this properly but I'm not sure if that's a good idea or
	# if it works for Perl 5.005.
	push @vals, ( $spec->{$k}{parse} eq 'code'  ? ( $self->{$k} ? 1 : 0 ) :
		      UNIVERSAL::isa( $self->{$k}, 'HASH' )  ? map { $_ => $self->{$k}{$_} } sort keys %{ $self->{$k} } :
		      UNIVERSAL::isa( $self->{$k}, 'ARRAY' ) ? sort @{ $self->{$k} } :
		      $self->{$k} );
    }

    # unpack('%32C*', $x) computes the 32-bit checksum of $x
    return join '!', $self->lexer->object_id, unpack('%32C*', join "\0", @vals);
}

my %top_level_only_block = map { $_ => 1 } qw( cleanup once shared );
my %valid_comp_flag = map { $_ => 1 } qw( inherit );
my %valid_escape_flag = map { $_ => 1 } qw( h n u );

sub add_allowed_globals
{
    my $self = shift;
    my @globals = @_;

    if ( my @bad = grep { ! /^[\$@%]/ } @globals )
    {
	param_error "add_allowed_globals: bad parameters '@bad', must begin with one of \$, \@, %\n";
    }

    $self->{allow_globals} = [ keys %{ { map { $_ => 1 } @globals, @{ $self->{allow_globals} } } } ];
    return @{ $self->{allow_globals} };
}

sub allow_globals
{
    my $self = shift;

    if (@_)
    {
	$self->{allow_globals} = [];
	return if @_ == 1 and not defined $_[0]; # @_ is (undef)
	$self->add_allowed_globals(@_);
    }

    return @{ $self->{allow_globals} };
}

sub compile
{
    my $self = shift;
    my %p = validate( @_, { comp_source => { type => SCALAR },
			    name => { type => SCALAR },
			  } );

    # Preprocess the script.  The preprocessor routine is handed a
    # reference to the entire script.
    if ($self->preprocess)
    {
	eval { $self->preprocess->( \$p{comp_source} ) };
	compiler_error "Error during custom preprocess step: $@" if $@;
    }

    $self->lexer->lex( comp_source => $p{comp_source}, name => $p{name}, compiler => $self );

    return $self->compiled_component;
}

sub start_component
{
    my $self = shift;

    compiler_error "Cannot start a component while already compiling a component"
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

    $data->{dangling_print} = 0;

    $data->{body} = '';

    foreach ( qw( def method ) )
    {
	$data->{$_} = {};
    }

    $data->{args} = [];
    $data->{flags} = {};
    $data->{attr} = {};

    foreach ( qw( cleanup filter init once perl shared ) )
    {
	$data->{blocks}{$_} = [];
    }
}

sub end_component
{
    my $self = shift;

    $self->lexer->throw_syntax_error("Not enough component-with-content ending tags found")
	if @{ $self->{comp_with_content_stack} };

    $self->_close_dangling_print;

    $self->{current_comp} = undef;
}

sub start_block
{
    my $self = shift;
    my %p = @_;

    $self->lexer->throw_syntax_error("Cannot define a $p{block_type} section inside a method or subcomponent")
	 if $top_level_only_block{ $p{block_type} } && ! $self->{in_main};

    $self->lexer->throw_syntax_error("Cannot nest a $p{block_type} inside a $self->{in_block} block")
	 if $self->{in_block};

    $self->{in_block} = $p{block_type};
}

sub raw_block
{
    # These blocks contain Perl code - so don't include <%text> and so on.

    my $self = shift;
    my %p = @_;

    eval { $self->postprocess_perl->( \$p{block} ) if $self->postprocess_perl };
    compiler_error $@ if $@;

    my $method = "$p{block_type}_block";
    return $self->$method(%p) if $self->can($method);

    push @{ $self->{current_comp}{blocks}{ $p{block_type} } }, $p{block};
}

sub doc_block
{
    # Don't do anything - just discard the comment.
}

sub perl_block
{
    my $self = shift;
    my %p = @_;

    $self->_close_dangling_print;

    $self->_add_body_code( $p{block} );
}

sub init_block
{
    my $self = shift;
    my %p = @_;

    my $comment = '';
    if ( $self->lexer->line_number )
    {
	my $line = $self->lexer->line_number;
	my $file = $self->lexer->name;
	$comment = "#line $line $file\n";
    }

    push @{ $self->{current_comp}{blocks}{ $p{block_type} } }, "$comment$p{block}";
}

sub text
{
    my $self = shift;
    my %p = @_;

    eval { $self->postprocess_text->(\$p{text}) if $self->postprocess_text };
    compiler_error $@ if $@;

    $p{text} =~ s,(['\\]),\\$1,g;

    my $code;
    $code = '$m->print( ' unless $self->{current_comp}{dangling_print};
    $code .= "( '$p{text}' ),";

    $self->_add_body_code($code) if $p{text} ne '';

    $self->{current_comp}{dangling_print} = 1;
}

sub text_block
{
    my $self = shift;
    my %p = @_;
    $self->text(text => $p{block});
}

sub end_block
{
    my $self = shift;
    my %p = @_;

    $self->lexer->throw_syntax_error("End of $p{block_type} encountered while in $self->{in_block} block")
	unless $self->{in_block} eq $p{block_type};

    $self->{in_block} = undef;
}

sub variable_declaration
{
    my $self = shift;
    my %p = @_;

    $self->lexer->throw_syntax_error("variable_declaration called inside a $p{block_type} block")
	unless $p{block_type} eq 'args';

    my $arg = "$p{type}$p{name}";

    $self->lexer->throw_syntax_error("$arg already defined")
        if grep { "$_->{type}$_->{name}" eq $arg } @{ $self->{current_comp}{args} };

    push @{ $self->{current_comp}{args} }, { type => $p{type},
					     name => $p{name},
					     default => $p{default} };
}

sub key_value_pair
{
    my $self = shift;
    my %p = @_;

    compiler_error "key_value_pair called inside a $p{block_type} block"
	unless $p{block_type} eq 'flags' || $p{block_type} eq 'attr';

    my $type = $p{block_type} eq 'flags' ? 'flag' : 'attribute';
    $self->lexer->throw_syntax_error("$p{key} $type already defined")
	if exists $self->{current_comp}{ $p{block_type} }{ $p{key} };

    $self->{current_comp}{ $p{block_type} }{ $p{key} } = $p{value}
}

sub start_named_block
{
    my $self = shift;
    my %p = @_;

    $self->lexer->throw_syntax_error("Cannot define a $p{type} inside a method or subcomponent")
        unless $self->{in_main};

    $self->{in_main}--;

    $self->{ $p{block_type} }{ $p{name} } = {};
    $self->_init_comp_data( $self->{ $p{block_type} }{ $p{name} } );
    $self->{current_comp} = $self->{ $p{block_type} }{ $p{name} };
}

sub end_named_block
{
    my $self = shift;

    $self->_close_dangling_print;

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
	    $self->lexer->throw_syntax_error("invalid <% %> escape flag: '$_'")
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

    my $code;
    $code = '$m->print( ' unless $self->{current_comp}{dangling_print};
    $code .= "( $text ),";

    eval { $self->postprocess_perl->(\$code) if $self->postprocess_perl };
    compiler_error $@ if $@;

    $self->_add_body_code($code);

    $self->{current_comp}{dangling_print} = 1;
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
    eval { $self->postprocess_perl->(\$code) if $self->postprocess_perl };
    compiler_error $@ if $@;

    $self->_close_dangling_print;

    $self->_add_body_code($code);
}

sub component_content_call
{
    my $self = shift;
    my %p = @_;

    my $call = $p{call};
    for ($call) { s/^\s+//; s/\s+$//; }
    push @{ $self->{comp_with_content_stack} }, $call;

    my $code = "\$m->comp( { content => sub {\n";

    eval { $self->postprocess_perl->(\$code) if $self->postprocess_perl };
    compiler_error $@ if $@;

    $self->_close_dangling_print;

    $self->_add_body_code($code);
}

sub component_content_call_end
{
    my $self = shift;

    $self->lexer->throw_syntax_error("found component with content ending tag but no beginning tag")
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

    eval { $self->postprocess_perl->(\$code) if $self->postprocess_perl };
    compiler_error $@ if $@;

    $self->_close_dangling_print;

    $self->_add_body_code($code);
}

sub perl_line
{
    my $self = shift;
    my %p = @_;

    my $code = "$p{line}\n";

    eval { $self->postprocess_perl->(\$code) if $self->postprocess_perl };
    compiler_error $@ if $@;

    $self->_close_dangling_print;

    $self->_add_body_code($code);
}

sub _close_dangling_print
{
    my $self = shift;

    return unless $self->{current_comp}{dangling_print};

    $self->{current_comp}{body} .= " );\n";

    $self->{current_comp}{dangling_print} = 0;
}

sub _add_body_code
{
    my $self = shift;
    my $code = shift;

    my $comment = $self->{current_comp}{dangling_print} ? "\n" : '';

    if ( $self->lexer->line_number )
    {
	my $line = $self->lexer->line_number;
	my $file = $self->lexer->name;
	$comment .= "#line $line $file\n";
    }

    $self->{current_comp}{body} .= "$comment$code";
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

sub HTML::Mason::Parser::new
{
    die "The Parser module is no longer a part of HTML::Mason.  Please see ".
        "the Lexer and Compiler modules, its replacements.\n";
}

1;

__END__

=head1 NAME

HTML::Mason::Compiler - Compile Mason component source

=head1 SYNOPSIS

  package My::Funky::Compiler;

  use base qw(HTML::Mason::Compiler);

=head1 DESCRIPTION

... Ken will be adding these docs (from Appendix A of the book) as
soon as the text has been looked at a little bit.

=head1 SEE ALSO

L<HTML::Mason|HTML::Mason>,
L<HTML::Mason::Admin|HTML::Mason::Admin>,
L<HTML::Mason::Interp|HTML::Mason::Interp>

=cut
