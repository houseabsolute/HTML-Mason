# Copyright (c) 1998-2002 by Jonathan Swartz. All rights reserved.
# This program is free software; you can redistribute it and/or modify it
# under the same terms as Perl itself.

package HTML::Mason::Lexer;

use strict;

use HTML::Mason::Exceptions( abbr => [qw(param_error syntax_error error)] );

use Params::Validate qw(:all);
Params::Validate::validation_options( on_fail => sub { param_error join '', @_ } );

use Class::Container;
use base qw(Class::Container);

# This is a block name and what method should be called to lex its
# contents if it is encountered.  'def' & 'method' blocks are special
# cases we actually call ->start again to recursively parse the
# contents of a subcomponent/method.  Theoretically, adding a block is
# as simple as adding an entry to this hash, and possibly a new
# contents lexing methods.
my %blocks = ( args    => 'variable_list_block',
	       attr    => 'key_val_block',
	       flags   => 'key_val_block',
	       cleanup => 'raw_block',
	       doc     => 'doc_block',
	       filter  => 'raw_block',
	       init    => 'raw_block',
	       once    => 'raw_block',
	       perl    => 'raw_block',
	       shared  => 'raw_block',
	       text    => 'text_block',
	     );

sub block_names
{
    return keys %blocks;
}

sub block_body_method
{
    return $blocks{ $_[1] };
}

{
    my $blocks_re;

    my $re = join '|', __PACKAGE__->block_names;
    $blocks_re = qr/$re/i;

    sub blocks_regex
    {
	return $blocks_re;
    }
}

sub lex
{
    my $self = shift;
    my %p = validate(@_,
		     {comp_source => SCALAR,
		      name => SCALAR,
		      compiler => {isa => 'HTML::Mason::Compiler'}}
		    );

    # Holds information about the current lex.  Make it local() so
    # we're fully re-entrant.
    local $self->{current} = \%p;
    my $current = $self->{current}; # For convenience

    # Clean up Mac and DOS line endings
    $current->{comp_source} =~ s/\r\n?/\n/g;

    # Initialize lexer state
    $current->{lines} = 1;
    $current->{in_def} = $current->{in_method} = 0;
    $current->{pos} = undef;

    # This will be overridden if entering a def or method section.
    $current->{ending} = qr/\G\z/;

    # We need to untaint the component or else the regexes will fail
    # to a Perl bug.  The delete is important because we need to
    # create an entirely new scalar, not just modify the existing one.
    ($current->{comp_source}) = delete($current->{comp_source}) =~ /(.*)/s;

    eval
    {
	$current->{compiler}->start_component;
	$self->start;
    };
    # Call this out here because it may be needed to break circular
    # refs inside the compiler
    $current->{compiler}->end_component;

    if ($@)
    {
	$@->rethrow if UNIVERSAL::can( $@, 'rethrow' );
	error $@;
    }
}

sub object_id
{
    my $self = shift;

    my @vals;
    foreach my $k ( sort keys %{ $self->validation_spec } )
    {
	next if $k eq 'container';

	push @vals, $k;
	push @vals, ( UNIVERSAL::isa( $self->{$k}, 'HASH' )  ? map { $_ => $self->{$k}{$_} } sort keys %{ $self->{$k} } :
		      UNIVERSAL::isa( $self->{$k}, 'ARRAY' ) ? sort @{ $self->{$k} } :
		      $self->{$k} );
    }

    local $^W; # ignore undef warnings
    # unpack('%32C*', $x) computes the 32-bit checksum of $x
    return unpack('%32C*', join "\0", class => ref($self), @vals);
}

sub start
{
    my $self = shift;

    my $end;
    while ( defined $self->{current}{pos} ?
	    $self->{current}{pos} < length $self->{current}{comp_source} :
	    1 )
    {
	last if $end = $self->match_end;

	$self->match_block && next;

	$self->match_named_block && next;

	$self->match_substitute && next;

	$self->match_comp_call && next;

	$self->match_perl_line && next;

	$self->match_comp_content_call && next;

	$self->match_comp_content_call_end && next;

	$self->match_text && next;

	if ( ( $self->{current}{in_def} || $self->{current}{in_method} ) &&
	     $self->{current}{comp_source} =~ /\G\z/ )
	{
	    my $type = $self->{current}{in_def} ? 'def' : 'method';
	    $self->throw_syntax_error("Missing closing </%$type> tag");
	}

	# We should never get here - if we do, we're in an infinite loop.
	$self->throw_syntax_error("Infinite parsing loop encountered - Lexer bug?");
    }

    if ( $self->{current}{in_def} || $self->{current}{in_method} )
    {
	my $type = $self->{current}{in_def} ? 'def' : 'method';
	unless ( $end =~ m,</%\Q$type\E>\n?,i )
	{
	    my $block_name = $self->{current}{"in_$type"};
	    $self->throw_syntax_error("No </%$type> tag for <%$type $block_name> block");
	}
    }
}

sub match_block
{
    my $self = shift;

    my $blocks_re = $self->blocks_regex;

    if ( $self->{current}{comp_source} =~ /\G<%($blocks_re)>/igcs )
    {
	my $type = lc $1;
	$self->{current}{compiler}->start_block( block_type => $type );

	my $method = $self->block_body_method($type);
	$self->$method( block_type => $type );

	return 1;
    }
}

sub generic_block
{
    my $self = shift;
    my %p = @_;

    my ($block, $nl) = $self->match_block_end( block_type => $p{block_type},
					       allow_text => 1 );

    my $method = $p{method};
    $self->{current}{compiler}->$method( block_type => $p{block_type},
					 block => $block );

    $self->{current}{lines} += $block =~ tr/\n/\n/;
    $self->{current}{lines}++ if $nl;

    $self->{current}{compiler}->end_block( block_type => $p{block_type} );
}

sub text_block
{
    my $self = shift;
    $self->generic_block(@_, method => 'text_block');
}

sub raw_block
{
    my $self = shift;
    $self->generic_block(@_, method => 'raw_block');
}

sub doc_block
{
    my $self = shift;
    $self->generic_block(@_, method => 'doc_block');
}

sub variable_list_block
{
    my ($self, %p) = @_;

    my $ending = qr/ \n | <\/%\Q$p{block_type}\E> /x;
    while ( $self->{current}{comp_source} =~ m,
                       \G               # last pos matched
                       (?:
                        [ \t]*
                        ( [\$\@\%] )    # variable type
                        ( [^\W\d]\w* )  # only allows valid Perl variable names
                        [ \t]*
                        # if we have a default arg we'll suck up
                        # any comment it has as part of the default
                        # otherwise explcitly search for a comment
                        (?:
                         (?:              # this entire entire piece is optional
                           =>
                          ( [^\n]+? )     # default value
                         )
                         |
                         (?:              # an optional comment
                          [ \t]*
                          \#
                          [^\n]*
                         )
                        )?
                        (?= $ending )
                        |
                        [ \t]*          # a comment line
                        \#
                        [^\n]*
                        (?= $ending )
                        |
                        [ \t]*          # just space
                       )
                       (\n |          # newline or
                          (?= <\/%\Q$p{block_type}\E> ) )   # end of block (don't consume it)
                      ,xgc
	  )
    {
	if ( defined $1 && defined $2 && length $1 && length $2 )
	{
	    $self->{current}{compiler}->variable_declaration( block_type => $p{block_type},
							      type => $1,
							      name => $2,
							      default => $3,
							    );
	}

	$self->{current}{lines}++ if $4;
    }

    my $nl = $self->match_block_end( block_type => $p{block_type},
				     allow_text => 0 );
    $self->{current}{lines}++ if $nl;

    $self->{current}{compiler}->end_block( block_type => $p{block_type} );
}

sub key_val_block
{
    my ($self, %p) = @_;

    my $ending = qr, (?: \n |           # newline or
                         (?= </%\Q$p{block_type}\E> ) )   # end of block (don't consume it)
                   ,x;

    while ( $self->{current}{comp_source} =~ /
                      \G
                      [ \t]*
                      ([\w_]+)          # identifier
                      [ \t]*=>[ \t]*    # separator
                      (\S[^\n]*?)        # value ( must start with a non-space char)
                      $ending
                      |
                      \G\n
                      |
                      \G[ \t]+?
                      $ending
                     /xgc )
    {
	if ( defined $1 && defined $2 && length $1 && length $2 )
	{
	    $self->{current}{compiler}->key_value_pair( block_type => $p{block_type},
							key => $1,
							value => $2
						      );
	}

	$self->{current}{lines}++;
    }

    my $nl = $self->match_block_end( block_type => $p{block_type},
				     allow_text => 0 );
    $self->{current}{lines}++ if $nl;

    $self->{current}{compiler}->end_block( block_type => $p{block_type} );
}

sub match_block_end
{
    my ($self, %p) = @_;

    my $re = $p{allow_text} ? qr,\G(.*?)</%\Q$p{block_type}\E>(\n?),is
                            : qr,\G()\s*</%\Q$p{block_type}\E>(\n?),is;
    if ( $self->{current}{comp_source} =~ /$re/gc )
    {
	return $p{allow_text} ? ($1, $2) : $2;
    }
    else
    {
	$self->throw_syntax_error("Invalid <%$p{block_type}> section line");
    }
}

sub match_named_block
{
    my ($self, %p) = @_;

    if ( $self->{current}{comp_source} =~ /\G<%(def|method)(?:\s+([^\n]+?))?\s*>/igcs )
    {
	my ($type, $name) = (lc $1, $2);

	$self->throw_syntax_error("$type block without a name")
	    unless defined $name && length $name;

	$self->{current}{compiler}->start_named_block( block_type => $type,
						       name => $name );

	# This will cause ->start to return once it hits the
	# appropriate ending tag.
	local $self->{current}{ending} = qr,\G</%\Q$type\E>\n?,i;

	local $self->{current}{"in_$type"} = $name;

	$self->start();

	$self->{current}{compiler}->end_named_block( block_type => $type );

	return 1;
    }
}

sub match_substitute
{
    my $self = shift;

    if ( $self->{current}{comp_source} =~ /\G<%/gcs )
    {
	if ( $self->{current}{comp_source} =~ /\G(.+?)(\s*\|\s*([a-z]+)?\s*)?%>/igcs )
	{
	    my ($sub, $escape) = ($1, $3);
	    $self->{current}{compiler}->substitution( substitution => $sub,
						      escape => $escape );

	    # Add it in just to count lines
	    $sub .= $2 if $2;
	    $self->{current}{lines} += $sub =~ tr/\n/\n/;

	    return 1;
	}
	else
	{
	    $self->throw_syntax_error("'<%' without matching '%>'");
	}
    }
}

sub match_comp_call
{
    my $self = shift;

    if ( $self->{current}{comp_source} =~ /\G<&(?!\|)/gcs )
    {
	if ( $self->{current}{comp_source} =~ /\G(.*?)&>/gcs )
	{
	    my $call = $1;
	    $self->{current}{compiler}->component_call( call => $call );
	    $self->{current}{lines} += $call =~ tr/\n/\n/;

	    return 1;
	}
	else
	{
	    $self->throw_syntax_error("'<&' without matching '&>'");
	}
    }
}


sub match_comp_content_call
{
    my $self = shift;

    if ( $self->{current}{comp_source} =~ /\G<&\|/gcs )
    {
	if ( $self->{current}{comp_source} =~ /\G(.*?)&>/gcs )
	{
	    my $call = $1;
	    $self->{current}{compiler}->component_content_call( call => $call );
	    $self->{current}{lines} += $call =~ tr/\n/\n/;

	    return 1;
	}
	else
	{
	    $self->throw_syntax_error("'<&|' without matching '&>'");
	}
    }
}

sub match_comp_content_call_end
{
    my $self = shift;

    if ( $self->{current}{comp_source} =~ m,\G</&>,gc )
    {
        $self->{current}{compiler}->component_content_call_end;

        return 1;
    }
}

sub match_perl_line
{
    my $self = shift;

    if ( $self->{current}{comp_source} =~ /\G(?<=^)%([^\n]*)(?:\n|\z)/gcm )
    {
	$self->{current}{compiler}->perl_line( line => $1 );
	$self->{current}{lines}++;

	return 1;
    }
}

sub match_text
{
    my $self = shift;

    #
    # This regex is a bit weird, to say the least!
    #
    # Basically, we need to first check for "text" at the current
    # position that we don't pass through as text.  This is either an
    # escaped newline or an EOF.
    #
    # If we find one of those, we have matched some "text" but nothing
    # that we pass through to the compiler.
    #
    # Otherwise, we try to match _at least_ one character of text
    # following by some non-text production, which may be some sort of
    # Mason syntax or an escaped newline or an EOF.
    #
    # It is important that we match at least one character or we can
    # fail incorrectly on something like this:
    #
    #   <%foo>
    #
    # It doesn't match any of the previous productions but if found at
    # the beginning of a component it will not match as text unless we
    # can consume the first character, "<", before trying to match any
    # more Mason syntax.
    #
    # Otherwise we'd match the empty string at the beginning, followed
    # by "<%".
    #
    # - Dave - 5/6/2002
    #
    if ( $self->{current}{comp_source} =~ m,
                    \G
                    (?:
                     (\\\n)      # an escaped newline  - throw away
                     |
                     \z          # or EOF.
                     |
                     (?:
                      (.+?)       # anything
	              (           # followed by
                       (?<=\n)(?=%) # an eval line - consume the \n
                       |
                       (?=</?%)   # a substitution or tag start or end  - don't consume
                       |
                       (?=</?&)   # a comp call start or end  - don't consume
                       |
                       \\\n       # an escaped newline  - throw away
                       |
                       \z         # or EOF.
                      )
                     )
                    )
                   ,gcsx
       )
    {
	my $consumed = join '', grep { defined } $1, $2, $3;
	return 0 unless length $consumed;

	$self->{current}{compiler}->text( text => $2 ) if defined $2 && length $2;

	$self->{current}{lines} += $consumed =~ tr/\n/\n/;
	return 1;
    }
    return 0;
}

sub match_end
{
    my $self = shift;

    # $self->{current}{ending} is a qr// 'string'.  No need to escape.  It will
    # also include the needed \G marker
    if ( $self->{current}{comp_source} =~ /($self->{current}{ending})/gcs )
    {
	my $text = $1;
	if (defined $text)
	{
	    $self->{current}{lines} += $text =~ tr/\n/\n/;
	}

	return $1 || 1;
    }
}

# goes from current pos, skips a newline if its the next character,
# and then goes to the next newline.  Alternately, the caller can
# provide a starting position.
sub _next_line
{
    my $self = shift;
    my $pos = shift;

    $pos = ( defined $pos ?
	     $pos :
	     ( substr( $self->{current}{comp_source}, pos($self->{current}{comp_source}), 1 ) eq "\n" ?
	       pos($self->{current}{comp_source}) + 1 :
	       pos($self->{current}{comp_source}) ) );

    my $to_eol = ( index( $self->{current}{comp_source}, "\n", $pos ) != -1 ?
		   ( index( $self->{current}{comp_source}, "\n" , $pos ) ) - $pos :
		   length $self->{current}{comp_source} );
    return substr( $self->{current}{comp_source}, $pos, $to_eol );
}

sub line_number
{
    my $self = shift;

    return $self->{current}{lines};
}

sub name
{
    my $self = shift;

    return $self->{current}{name};
}

sub throw_syntax_error
{
    my ($self, $error) = @_;

    HTML::Mason::Exception::Syntax->throw( error => $error,
					   comp_name => $self->name,
					   source_line => $self->_next_line,
					   line_number => $self->line_number );
}

1;

__END__

=head1 NAME

HTML::Mason::Lexer - Generates events based on component source lexing

=head1 SYNOPSIS

  my $lexer = HTML::Mason::Lexer->new;

  $lexer->lex( comp_source => $source, name => $comp_name, compiler => $compiler );

=head1 DESCRIPTION

The Lexer works in tandem with the Compiler to turn Mason component
source into something else, generally Perl code.

As the lexer finds component elements, like a tag or block, it calls
the appropriate event methods in the compiler object it was given.

It has only a few public methods.

You can replace this lexer with one of your own simply by telling the
Compiler to use a different lexer class.  Your lexer class simply
needs to call the appropriate methods in the Component Class's API as
it scans the source.

=head1 METHODS

The lexer has very few public methods.

=over 4

=item new

This method creates a new Lexer object.  This methods takes no
parameters.

=item lex ( comp_source => ..., name => ..., compiler => ... )

This method tells the lexer to start scanning the given component
source.  All of these parameters are required.  The C<name> parameter
will be used in any error messages generated during lexing.  The
C<compiler> object must be an object that implements the Mason
Component API.

=item line_number

The current line number that the lexer has reached.

=item name

The name of the component currently being lexed.

=item throw_syntax_error ($error)

This throws an C<HTML::Mason::Exception::Syntax> error with the given
error message as well as additional information about the component
source.

This method is used by both the Lexer and the Compiler.

=back

=head1 SUBCLASSING

Any subclass of the lexer should declare itself to be a subclass of
C<HTML::Mason::Lexer>, even if it plans to override all of its public
methods.

If you want your subclass to work with the existing Compiler classes
in Mason, you must implement the methods listed above.  If you plan to
use a custom Compiler class that you're writing, you can do whatever
you want.

=cut
