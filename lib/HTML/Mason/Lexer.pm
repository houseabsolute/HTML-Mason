# Copyright (c) 1998-2001 by Jonathan Swartz. All rights reserved.
# This program is free software; you can redistribute it and/or modify it
# under the same terms as Perl itself.

package HTML::Mason::Lexer;

use strict;

use HTML::Mason::Exceptions( abbr => [qw(param_error syntax_error error)] );

use Params::Validate qw(:all);
Params::Validate::validation_options( on_fail => sub { param_error join '', @_ } );

use HTML::Mason::Container;
use base qw(HTML::Mason::Container);

__PACKAGE__->valid_params();
__PACKAGE__->contained_objects();


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

sub block_body_method
{
    return $blocks{ $_[1] };
}

{
    my $blocks_re;

    my $re = join '|', keys %blocks;
    $blocks_re = qr/$re/i;

    sub blocks_regex
    {
	return $blocks_re
    }
}

sub simple_block_types
{
    return grep { $blocks{$_} eq 'raw_block'} keys %blocks;
}

# make this settable somehow
sub named_block_types
{
    return ('def', 'method');
}

sub new
{
    my $proto = shift;
    my $class = ref $proto || $proto;
    return bless { validate(@_, $class->validation_spec) }, $class;
}

sub lex
{
    my $self = shift;
    my %p = validate(@_,
		     {comp_text => SCALAR,
		      name => SCALAR,
		      compiler => {isa => 'HTML::Mason::Compiler'}}
		    );

    # Holds information about the current lex.  Make it local() so
    # we're fully re-entrant.
    local $self->{current} = \%p;
    my $current = $self->{current}; # For convenience

    # Clean up DOS line endings
    $current->{comp_text} =~ s/\r\n/\n/g;

    # Initialize lexer state
    $current->{lines} = 1;
    $current->{in_def} = $current->{in_method} = 0;
    $current->{pos} = undef;

    # This will be overridden if entering a def or method section.
    $current->{ending} = qr/\G\z/;

    # We need to untaint the component or else the regexes will fail
    # to a Perl bug.  The delete is important because we need to
    # create an entirely new scalar, not just modify the existing one.
    ($current->{comp_text}) = delete($current->{comp_text}) =~ /(.*)/s;

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
	push @vals, $k;
	push @vals, ( UNIVERSAL::isa( $self->{$k}, 'HASH' )  ? map { $_ => $self->{$k}{$_} } keys %{ $self->{$k} } :
		      UNIVERSAL::isa( $self->{$k}, 'ARRAY' ) ? @{ $self->{$k} } :
		      $self->{$k} );
    }

    # unpack('%32C*', $x) computes the 32-bit checksum of $x
    return unpack('%32C*', join "\0", class => ref($self), @vals);
}

sub start
{
    my $self = shift;

    my $end;
    while ( defined $self->{current}{pos} ? $self->{current}{pos} < length $self->{current}{comp_text} : 1 )
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

	# We should never get here - if we do, we're in an infinite loop.
	syntax_error "Infinite parsing loop encountered - Lexer bug?";
    }

    if ( $self->{current}{in_def} || $self->{current}{in_method} )
    {
	my $type = $self->{current}{in_def} ? 'def' : 'method';
	unless ( $end =~ m,</%\Q$type\E>\n?,i )
	{
	    my $block_name = $self->{current}{"in_$type"};
	    syntax_error "No </%$type> tag for <%$type $block_name> block";
	}
    }
}

sub match_block
{
    my $self = shift;

    my $blocks_re = $self->blocks_regex;

    if ( $self->{current}{comp_text} =~ /\G<%($blocks_re)>/igcs )
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
    my $self = shift;
    my %p = @_;

    while ( $self->{current}{comp_text} =~ m,
                       \G               # last pos matched
                       (?:
                        [ \t]*
                        ( [\$\@\%] )    # variable type
                        ( [^\W\d]\w* )  # only allows valid Perl variable names
                        [ \t]*
			(?:             # this entire entire piece is optional
			 =>
                         ( [^\n]+ )     # default value
		        )?
                        (?:             # an optional comment
                         [ \t]*
                         \#
                         [^\n]*
                        )?
                        |
                        [ \t]*          # a comment line
                        \#
                        [^\n]*
                        |
                        [ \t]*          # just space
                       )
                       \n
                      ,xgcs
	  )
    {
	if ( length $1 && length $2 )
	{
	    $self->{current}{compiler}->variable_declaration( block_type => $p{block_type},
							      type => $1,
							      name => $2,
							      default => $3,
							    );
	}

	$self->{current}{lines}++;
    }

    my $nl = $self->match_block_end( block_type => $p{block_type},
				     allow_text => 0 );
    $self->{current}{lines}++ if $nl;

    $self->{current}{compiler}->end_block( block_type => $p{block_type} );
}

sub key_val_block
{
    my $self = shift;
    my %p = @_;

    while ( $self->{current}{comp_text} =~ /
                      \G
                      [ \t]*
                      ([\w_]+)          # identifier
                      [ \t]*=>[ \t]*    # separator
                      (\S[^\n]*)        # value ( must start with a non-space char)
                      \n
                      |
                      \G[ \t]*\n
                     /gcx )
    {
	if ( length $1 && length $2 )
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
    my $self = shift;
    my %p = @_;

    my $re = $p{allow_text} ? qr,\G(.*?)</%\Q$p{block_type}\E>(\n?),is : qr,\G()</%\Q$p{block_type}\E>(\n?),is;
    if ( $self->{current}{comp_text} =~ /$re/gc )
    {
	return $p{allow_text} ? ($1, $2) : $2;
    }
    else
    {
	my $line = $self->_next_line;
	syntax_error "Invalid <%$p{block_type}> section line at line $self->{current}{lines}:\n$line";
    }
}

sub match_named_block
{
    my $self = shift;
    my %p = @_;

    if ( $self->{current}{comp_text} =~ /\G<%(def|method)\s+([^\n]+?)>/igcs )
    {
	my ($type, $name) = ($1, $2);
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

    if ( $self->{current}{comp_text} =~ /\G<%/gcs )
    {
	if ( $self->{current}{comp_text} =~ /\G(.+?)(\s*\|\s*([a-z]+)?\s*)?%>/igcs )
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
	    my $line = $self->_next_line( $self->{current}{pos} - 2 );
	    syntax_error "'<%' without matching '%>' at $self->{current}{lines}:\n$line";
	}
    }
}

sub match_comp_call
{
    my $self = shift;

    if ( $self->{current}{comp_text} =~ /\G<&(?!\|)/gcs )
    {
	if ( $self->{current}{comp_text} =~ /\G(.*?)&>/gcs )
	{
	    my $call = $1;
	    $self->{current}{compiler}->component_call( call => $call );
	    $self->{current}{lines} += $call =~ tr/\n/\n/;

	    return 1;
	}
	else
	{
	    my $line = $self->_next_line( $self->{current}{pos} - 2 );
	    syntax_error "'<&' without matching '&>' at $self->{current}{lines}:\n$line";
	}
    }
}


sub match_comp_content_call
{
    my $self = shift;

    if ( $self->{current}{comp_text} =~ /\G<&\|/gcs )
    {
	if ( $self->{current}{comp_text} =~ /\G(.*?)&>/gcs )
	{
	    my $call = $1;
	    $self->{current}{compiler}->component_content_call( call => $call );
	    $self->{current}{lines} += $call =~ tr/\n/\n/;

	    return 1;
	}
	else
	{
	    my $line = $self->_next_line( $self->{current}{pos} - 3 );
	    syntax_error "'<&|' without matching '&>' at $self->{current}{lines}:\n$line";
	}
    }
}

sub match_comp_content_call_end
{
    my $self = shift;

    if ( $self->{current}{comp_text} =~ m,\G</&>,gc )
    {
        $self->{current}{compiler}->component_content_call_end;

        return 1;
    }
}

sub match_perl_line
{
    my $self = shift;

    if ( $self->{current}{comp_text} =~ /\G%([^\n]*)(?:\n|\z)/gcs )
    {
	$self->{current}{compiler}->perl_line( line => $1 );
	$self->{current}{lines}++;

	return 1;
    }
}

sub match_text
{
    my $self = shift;

    if ( $self->{current}{comp_text} =~ m,
                    \G
                    (.*?)       # anything
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
                   ,gcsx
       )
    {
	my $consumed = "$1$2";
	return 0 unless length $consumed;

	$self->{current}{compiler}->text( text => $1 );
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
    if ( $self->{current}{comp_text} =~ /($self->{current}{ending})/gcs )
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
	     ( substr( $self->{current}{comp_text}, pos($self->{current}{comp_text}), 1 ) eq "\n" ?
	       pos($self->{current}{comp_text}) + 1 :
	       pos($self->{current}{comp_text}) ) );

    my $to_eol = ( index( $self->{current}{comp_text}, "\n", $pos ) != -1 ?
		   ( index( $self->{current}{comp_text}, "\n" , $pos ) ) - $pos :
		   length $self->{current}{comp_text} );
    return substr( $self->{current}{comp_text}, $pos, $to_eol );
}

sub line_count
{
    my $self = shift;

    return $self->{current}{lines};
}

sub name
{
    my $self = shift;

    return $self->{current}{name};
}

1;
