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

use Class::Container;
use base qw(Class::Container);

BEGIN
{
    __PACKAGE__->valid_params
	(
	 allow_globals =>
         { parse => 'list', type => ARRAYREF, default => [],
           descr => "An array of names of Perl variables that are allowed globally within components" },

	 default_escape_flags =>
         { parse => 'string', type => SCALAR|ARRAYREF, default => [],
           descr => "Escape flags that will apply by default to all Mason tag output" },

	 lexer =>
         { isa => 'HTML::Mason::Lexer',
           descr => "A Lexer object that will scan component text during compilation" },

	 preprocess =>
         { parse => 'code', type => CODEREF, optional => 1,
           descr => "A subroutine through which all component text will be sent during compilation" },

	 postprocess_perl =>
         { parse => 'code', type => CODEREF, optional => 1,
           descr => "A subroutine through which all Perl code will be sent during compilation" },

	 postprocess_text =>
         { parse => 'code', type => CODEREF, optional => 1,
           descr => "A subroutine through which all plain text will be sent during compilation" },

	 use_source_line_numbers =>
	 { parse => 'boolean', type => SCALAR, default => 1,
	   descr => "Whether to use source line numbers in errors and debugger" },
	);

    __PACKAGE__->contained_objects
        ( lexer => { class => 'HTML::Mason::Lexer',
                     descr => "This class generates compiler events based on the components source" },
        );
}

use HTML::Mason::MethodMaker
    ( read_write => [ map { [ $_ => __PACKAGE__->validation_spec->{$_} ] }
                      qw( lexer
                          preprocess
                          postprocess_perl
                          postprocess_text
			  use_source_line_numbers
                        )
		    ],
    );

my $old_escape_re = qr/^[hnu]+$/;

sub new
{
    my $class = shift;
    my $self = $class->SUPER::new(@_);

    $self->default_escape_flags( $self->{default_escape_flags} )
        if defined $self->{default_escape_flags};

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

    my @id_keys =
	( grep { ! exists $spec->{$_}{isa} && ! exists $spec->{$_}{can} }
	  grep { $_ ne 'container' } keys %$spec );

    my @vals;
    foreach my $k ( @id_keys )
    {
	push @vals, $k;

	# For coderef params we simply indicate whether or not it is
	# present.  This is better than simply ignoring them but not
	# by much.  We _could_ use B::Deparse's coderef2text method to
	# do this properly but I'm not sure if that's a good idea or
	# if it works for Perl 5.005.
	push @vals,
            $HTML::Mason::VERSION,
            ( $spec->{$k}{parse} eq 'code'  ? ( $self->{$k} ? 1 : 0 ) :
              UNIVERSAL::isa( $self->{$k}, 'HASH' )  ?
              map { $_ => $self->{$k}{$_} } sort keys %{ $self->{$k} } :
              UNIVERSAL::isa( $self->{$k}, 'ARRAY' ) ? sort @{ $self->{$k} } :
              $self->{$k} );
    }

    local $^W; # ignore undef warnings
    # unpack('%32C*', $x) computes the 32-bit checksum of $x
    return join '!', $self->lexer->object_id, unpack('%32C*', join "\0", @vals);
}

my %top_level_only_block = map { $_ => 1 } qw( cleanup once shared );
my %valid_comp_flag = map { $_ => 1 } qw( inherit );

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

sub default_escape_flags
{
    my $self = shift;

    return $self->{default_escape_flags} unless @_;

    my $flags = shift;

    unless ( defined $flags )
    {
        $self->{default_escape_flags} = [];
        return;
    }

    # make sure this is always an arrayref
    unless ( ref $flags )
    {
        if ( $flags =~ /^[hu]+$/ )
        {
            $self->{default_escape_flags} = [ split //, $flags ];
        }
        else
        {
            $self->{default_escape_flags} = [ $flags ];
        }
    }

    return $self->{default_escape_flags};
}

sub compile
{
    my $self = shift;
    my %p = validate( @_, { comp_source => { type => SCALAR|SCALARREF },
			    name => { type => SCALAR },
			    fh => { type => HANDLE, optional => 1 },
			  } );
    my $src = ref($p{comp_source}) ? $p{comp_source} : \$p{comp_source};

    local $self->{current_comp} = {};
    local $self->{paused_compiles} = []; # So we're re-entrant in subcomps

    # Preprocess the source.  The preprocessor routine is handed a
    # reference to the entire source.
    if ($self->preprocess)
    {
	eval { $self->preprocess->( $src ) };
	compiler_error "Error during custom preprocess step: $@" if $@;
    }

    $self->lexer->lex( comp_source => $src, name => $p{name}, compiler => $self );

    return $self->compiled_component( exists($p{fh}) ? (fh => $p{fh}) : () );
}

sub start_component
{
    my $self = shift;
    my $c = $self->{current_comp};

    $c->{in_main} = 1;
    $c->{comp_with_content_stack} = [];

    $c->{in_block} = undef;

    $self->_init_comp_data($c);
}

sub _init_comp_data
{
    my $self = shift;
    my $data = shift;

    $data->{body} = '';
    $data->{last_body_code_type} = '';

    foreach ( qw( def method ) )
    {
	$data->{$_} = {};
    }

    $data->{args} = [];
    $data->{flags} = {};
    $data->{attr} = {};

    foreach ( qw( cleanup filter init once shared ) )
    {
	$data->{blocks}{$_} = [];
    }
}

sub end_component
{
    my $self = shift;
    my $c = $self->{current_comp};

    $self->lexer->throw_syntax_error("Not enough component-with-content ending tags found")
	if $c->{comp_with_content_stack} && @{ $c->{comp_with_content_stack} };
}

sub start_block
{
    my $self = shift;
    my $c = $self->{current_comp};
    my %p = @_;

    $self->lexer->throw_syntax_error("Cannot define a $p{block_type} section inside a method or subcomponent")
	 if $top_level_only_block{ $p{block_type} } && ! $c->{in_main};

    $self->lexer->throw_syntax_error("Cannot nest a $p{block_type} inside a $c->{in_block} block")
	 if $c->{in_block};

    $c->{in_block} = $p{block_type};
}

sub raw_block
{
    # These blocks contain Perl code - so don't include <%text> and so on.

    my $self = shift;
    my $c = $self->{current_comp};
    my %p = @_;

    eval { $self->postprocess_perl->( \$p{block} ) if $self->postprocess_perl };
    compiler_error $@ if $@;

    my $method = "$p{block_type}_block";
    return $self->$method(%p) if $self->can($method);

    my $comment = '';
    if ( $self->lexer->line_number )
    {
	my $line = $self->lexer->line_number;
	my $file = $self->lexer->name;
	$comment = "#line $line $file\n" if $self->use_source_line_numbers;
    }

    push @{ $self->{current_comp}{blocks}{ $p{block_type} } }, "$comment$p{block}";
}

sub doc_block
{
    # Don't do anything - just discard the comment.
}

sub perl_block
{
    my $self = shift;
    my %p = @_;

    $self->_add_body_code( $p{block} );

    $self->{current_comp}{last_body_code_type} = 'perl_block';
}

sub text
{
    my ($self, %p) = @_;
    my $tref = ref($p{text}) ? $p{text} : \$p{text};  # Allow a reference

    eval { $self->postprocess_text->($tref) } if $self->postprocess_text;
    compiler_error $@ if $@;

    $$tref =~ s,(['\\]),\\$1,g;

    $self->_add_body_code("\$m->print( '", $$tref, "' );\n");

    $self->{current_comp}{last_body_code_type} = 'text';
}

sub text_block
{
    my $self = shift;
    my %p = @_;
    $self->text(text => \$p{block});
}

sub end_block
{
    my $self = shift;
    my $c = $self->{current_comp};
    my %p = @_;

    $self->lexer->throw_syntax_error("End of $p{block_type} encountered while in $c->{in_block} block")
	unless $c->{in_block} eq $p{block_type};

    $c->{in_block} = undef;
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
					     default => $p{default},
					     line => $self->lexer->line_number,
					     file => $self->lexer->name,
					   };
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
    my $c = $self->{current_comp};
    my %p = @_;

    $self->lexer->throw_syntax_error
	("Cannot define a $p{block_type} block inside a method or subcomponent")
	    unless $c->{in_main};

    $self->lexer->throw_syntax_error("Invalid $p{block_type} name: $p{name}")
	if $p{name} =~ /[^.\w-]/;

    my $other_type = $p{block_type} eq 'def' ? 'method' : 'def';
    $self->lexer->throw_syntax_error
        ("Cannot define a method and subcomponent with the same name ($p{name}")
            if exists $c->{$other_type}{ $p{name} };

    $c->{in_main}--;

    $c->{ $p{block_type} }{ $p{name} } = {};
    $self->_init_comp_data( $c->{ $p{block_type} }{ $p{name} } );
    push @{$self->{paused_compiles}}, $c;
    $self->{current_comp} = $c->{ $p{block_type} }{ $p{name} };
}

sub end_named_block
{
    my $self = shift;

    $self->{current_comp} = pop @{$self->{paused_compiles}};
    $self->{current_comp}{in_main}++;
}

sub substitution
{
    my $self = shift;
    my %p = @_;

    my $text = $p{substitution};

    if ( ( exists $p{escape} && defined $p{escape} ) ||
         @{ $self->{default_escape_flags} }
       )
    {
        my @flags;
        if ( defined $p{escape} )
        {
            $p{escape} =~ s/\s+$//;

            if ( $p{escape} =~ /$old_escape_re/ )
            {
                @flags = split //, $p{escape};
            }
            else
            {
                @flags = split /\s*,\s*/, $p{escape};
            }
        }

        # is there any way to check the flags for validity and still
        # allow them to be dynamically set from components?

        unshift @flags, @{ $self->default_escape_flags }
            unless grep { $_ eq 'n' } @flags;

        my %seen;
	my $flags =
            ( join ', ',
              map { $seen{$_}++ ? () : "'$_'" }
              grep { $_ ne 'n' } @flags
            );

        $text = "\$m->interp->apply_escapes( (join '', ($text)), $flags )" if $flags;
    }

    my $code = "\$m->print( $text );\n";

    eval { $self->postprocess_perl->(\$code) } if $self->postprocess_perl;
    compiler_error $@ if $@;

    $self->_add_body_code($code);

    $self->{current_comp}{last_body_code_type} = 'substitution';
}

sub component_call
{
    my $self = shift;
    my %p = @_;

    my $call = $p{call};
    for ($call) { s/^\s+//; s/\s+$//; }
    if ( $call =~ m,^[\w/.],)
    {
	my $comma = index($call, ',');
	$comma = length $call if $comma == -1;
	(my $comp = substr($call, 0, $comma)) =~ s/\s+$//;
	$call = "'$comp'" . substr($call, $comma);
    }

    my $code = "\$m->comp( $call );\n";
    eval { $self->postprocess_perl->(\$code) } if $self->postprocess_perl;
    compiler_error $@ if $@;

    $self->_add_body_code($code);

    $self->{current_comp}{last_body_code_type} = 'component_call';
}

sub component_content_call
{
    my $self = shift;
    my $c = $self->{current_comp};
    my %p = @_;

    my $call = $p{call};
    for ($call) { s/^\s+//; s/\s+$//; }
    push @{ $c->{comp_with_content_stack} }, $call;

    my $code = "\$m->comp( { content => sub {\n";

    eval { $self->postprocess_perl->(\$code) } if $self->postprocess_perl;
    compiler_error $@ if $@;

    $self->_add_body_code($code);

    $c->{last_body_code_type} = 'component_content_call';
}

sub component_content_call_end
{
    my $self = shift;
    my $c = $self->{current_comp};

    $self->lexer->throw_syntax_error("found component with content ending tag but no beginning tag")
	unless @{ $c->{comp_with_content_stack} };

    my $call = pop @{ $c->{comp_with_content_stack} };

    if ( $call =~ m,^[\w/.],)
    {
	my $comma = index($call, ',');
	$comma = length $call if $comma == -1;
	(my $comp = substr($call, 0, $comma)) =~ s/\s+$//;
	$call = "'$comp'" . substr($call, $comma);
    }

    my $code = "} }, $call );\n";

    eval { $self->postprocess_perl->(\$code) } if $self->postprocess_perl;
    compiler_error $@ if $@;

    $self->_add_body_code($code);

    $c->{last_body_code_type} = 'component_content_call_end';
}

sub perl_line
{
    my $self = shift;
    my %p = @_;

    my $code = "$p{line}\n";

    eval { $self->postprocess_perl->(\$code) } if $self->postprocess_perl;
    compiler_error $@ if $@;

    $self->_add_body_code($code);

    $self->{current_comp}{last_body_code_type} = 'perl_line';
}

sub _add_body_code
{
    my $self = shift;

    # We know a perl-line is always _one_ line, so we know that the
    # line numbers are going to match up as long as the first line in
    # a series has a line number comment before it.  Adding a comment
    # can break certain constructs like qw() list that spans multiple
    # perl-lines.
    if ( $self->lexer->line_number &&
         $self->{current_comp}{last_body_code_type} ne 'perl_line' )
    {
	my $line = $self->lexer->line_number;
	my $file = $self->lexer->name;
	$self->{current_comp}{body} .= "#line $line $file\n" if $self->use_source_line_numbers;
    }

    $self->{current_comp}{body} .= $_ foreach @_;
}

sub dump
{
    my $self = shift;
    my $c = $self->{current_comp};

    warn "Main component\n";

    $self->_dump_data( $c );

    foreach ( keys %{ $c->{def} } )
    {
	warn "  Subcomponent $_\n";
	$self->_dump_data( $c->{def}{$_}, '  ' );
    }

    foreach ( keys %{ $c->{method} } )
    {
	warn "  Methods $_\n";
	$self->_dump_data( $c->{method}{$_}, '  ');
    }
}

sub _dump_data
{
    my $self = shift;
    my $data = shift;
    my $indent = shift || '';

    if ( @{ $data->{args} } )
    {
	warn "$indent  args\n";
	foreach ( @{ $data->{args} } )
	{
	    warn "$indent    $_->{type}$_->{name}";
	    warn " => $_->{default}" if defined $_->{default};
	    warn "\n";
	}
    }

    warn "\n$indent  body\n";
    warn $data->{body}, "\n";
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

The compiler starts the compilation process by calling its lexer's
C<lex> method and passing itself as the C<compiler> parameter.  The
lexer then calls various methods in the compiler as it parses the
component source.

=head1 PARAMETERS TO THE new() CONSTRUCTOR

=over 4

=item allow_globals

List of variable names, complete with prefix (C<$@%>), that you intend
to use as globals in components.  Normally global variables are
forbidden by C<strict>, but any variable mentioned in this list is
granted a reprieve via a "use vars" statement. For example:

    allow_globals => [qw($DBH %session)]

In a mod_perl environment, C<$r> (the request object) is automatically
added to this list.

=item default_escape_flags

Escape flags to apply to all <% %> expressions by default. The current
valid flags are

    h - escape for HTML ('<' => '&lt;', etc.)
    u - escape for URL (':' => '%3A', etc.)

The developer can override default escape flags on a per-expression
basis; see DEVEL<escaping expressions>.

If you want to set I<multiple> flags as the default, this should be
given as a reference to an array of flags.

=item lexer

The Lexer object to associate with this Compiler. By default a new
object of class P<lexer_class> will be created.

=item lexer_class

The class to use when creating a lexer. Defaults to L<HTML::Mason::Lexer|HTML::Mason::Lexer>.

=item preprocess

Sub reference that is called to preprocess each component before the compiler does
it's magic.  The sub is called with a single parameter, a scalar reference
to the script.  The sub is expected to process the script in-place.   This is
one way to extend the HTML::Mason syntax with new tags, etc., although a much
more flexible way is to subclass the Lexer or Compiler class. See also
P<postprocess_text> and P<postprocess_perl>.

=item postprocess_text

Sub reference that is called to postprocess the text portion of a
compiled component, just before it is assembled into its final
subroutine form.  The sub is called with a single parameter, a scalar
reference to the text portion of the component.  The sub is expected
to process the string in-place. See also
P<preprocess> and P<postprocess_perl>.

=item postprocess_perl

Sub reference that is called to postprocess the Perl portion of a
compiled component, just before it is assembled into its final
subroutine form.  The sub is called with a single parameter, a scalar
reference to the Perl portion of the component.  The sub is expected
to process the string in-place. See also
P<preprocess> and P<postprocess_text>.

=item use_source_line_numbers

True or false, default is true. Indicates whether component line
numbers that appear in error messages, stack traces, etc. are in terms
of the source file instead of the object file. Mason does this by
inserting '#line' directives into compiled components.  While source
line numbers are more immediately helpful, object file line numbers
may be more appropriate for in-depth debugging sessions.

=back

=head1 METHODS

There are several methods besides the compilation callbacks below that
a Compiler subclass needs to implement.

=over 4

=item compile(comp_source => <string>, name => <string>, comp_class => <string>)

The "comp_class" parameter may be ignored by the compiler.

=item object_id

This method should return a unique id for the given compiler object.
This is used by the interpreter when loading previously compiled
objects in order to determine whether or not the object should be
re-compiled.

=back

=head2 Compilation Callbacks

These are methods called by the Lexer while processing a component
source.  You may wish to override some of these methods if you're
implementing your own custom Compiler class.

=over 4

=item start_component()

This method is called by the Lexer when it starts processing a
component.

=item end_component()

This method is called by the Lexer when it finishes processing a
component.

=item start_block(block_type => <string>)

This method is called by the Lexer when it encounters an opening Mason
block tag like C<< <%perl> >> or C<< <%args> >>.  Its main purpose is
to keep track of the nesting of different kinds of blocks within each
other.  The type of block ("init", "once", etc.) is passed via the
"block_type" parameter.

=item end_block(block_type => <string>)

This method is called by the Lexer when it encounters a closing Mason
block tag like C<< </%perl> >> or C<< </%args> >>.  Like
C<start_block()>, its main purpose is to help maintain syntactic
integrity.

=item *_block(block => <string>, [ block_type => <string> ])

Several compiler methods like C<doc_block()>, C<text_block()>, and
C<raw_block()> are called by the Lexer after C<start_block()> when it
encounters blocks of certain types.  These methods actually do the
work of putting the body of a block into the compiled data structure.

The methods that follow this pattern are C<init_block()>,
C<perl_block()>, C<doc_block()>, C<text_block()>, and C<raw_block()>.
The last method is called for all C<< <%once> >>, C<< <%cleanup> >>,
C<< <%filter> >>, C<< <%init> >>, C<< <%perl> >>, and C<< <%shared> >>
blocks.

=item text(text => <string>)

Inserts the text contained in a C<text> parameter into the component
for verbatim output.

This is called when the lexer finds plain text in a component.

=item variable_declaration( type => <string>, name => <string>, default => <string> )

Inserts a variable declaration from the C<< <%args> >> section into
the component.

The type will be either "$", "@", or "%", indicating a scalar, array,
or hash.  The name is the variable name without the leading sigil.
The default is everything found after the first "=>" on an C<< <%args> >>
block line, and may include a comment.

=item key_value_pair(block_type => <string>, key => <string>, value => <string>)

Inserts a key-value pair from a C<< <%flags> >> or C<< <%attr> >>
section into the component.

The "block_type" parameter will be either "flags" or "attr".

=item start_named_block(block_type => <string>, name => <name>)

Analogous to L<item_start_block|start_block>, but starts a "named" block 
(C<< <%method> >> or C<< <%def> >>).

=item end_named_block()

Called by the Lexer to end a "named" block.

=item substitution(substitution => <string>, escape => <string>)

Called by the Lexer when it encounters a substitution tag 
(C<< <% ... %> >>).

The value of the "escape" parameter will be everything found after the
pipe (|) in the substitution tag, and may be more than one character
such as "nh".

=item component_call(call => <string>)

Called by the Lexer when it encounters a component call tag without
embedded content (C<< <& ... &> >>).

The "call" parameter contains the entire contents of the tag.

=item component_content_call(call => <string>)

Called by the Lexer when it encounters a component call tag with
embedded content (C<< <&| ... &> >>).

=item component_content_call_end()

Called by the Lexer when it encounters an ending tag for a component
call with content (C<< </&> >>).  Note that there is no corresponding
C<component_call_end()> method for component calls without content,
because these calls don't have ending tags.

=item perl_line(line => <string>)

Called by the Lexer when it encounters a C<%>-line.

=back

=head1 SEE ALSO

L<HTML::Mason|HTML::Mason>,
L<HTML::Mason::Admin|HTML::Mason::Admin>,
L<HTML::Mason::Interp|HTML::Mason::Interp>

=cut
