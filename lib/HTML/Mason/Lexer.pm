# Copyright (c) 1998-2001 by Jonathan Swartz. All rights reserved.
# This program is free software; you can redistribute it and/or modify it
# under the same terms as Perl itself.

package HTML::Mason::Lexer;

use strict;

use HTML::Mason::Exceptions;

use Params::Validate qw(:all);
Params::Validate::set_options( on_fail => sub { HTML::Mason::Exception::Params->throw( error => shift ) } );

my %valid_params = ();
sub valid_params { \%valid_params }

my %creates_objects = ();
sub creates_objects { \%creates_objects }


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
	       doc     => 'raw_block',
	       filter  => 'raw_block',
	       init    => 'raw_block',
	       once    => 'raw_block',
	       perl    => 'raw_block',
	       shared  => 'raw_block',
	       text    => 'raw_block',
	     );

my $blocks_re;
{
    my $re = join '|', keys %blocks;
    $blocks_re = qr/$re/i;
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
    return bless { validate(@_, $class->valid_params) }, $class;
}

sub lex
{
    my $self = shift;
    my %p = validate(@_,
		     {comp_text => SCALAR, 
		      name => SCALAR,
		      compiler => {isa => 'HTML::Mason::Compiler'}}
		    );
    
    # Make these local, because they only apply to the current parse.
    # This also avoids a circular ref between the compiler & lexer.  They
    # aren't really data members of $self, but $self is a convenient place
    # to store them temporarily.
    local $self->{compiler}  = $p{compiler};
    local $self->{comp_text} = $p{comp_text};
    local $self->{name}      = $p{name};

    # Clean up DOS line endings
    $self->{comp_text} =~ s/\r\n/\n/g;

    # Initialize lexer state
    $self->{lines} = 1;
    $self->{in_def} = $self->{in_method} = 0;
    $self->{pos} = undef;

    # This will be overridden if entering a def or method section.
    $self->{ending} = qr/\G\z/;

    eval
    {
	$self->{compiler}->start_component;

	$self->start;
    };
    # Call this out here because it may be needed to break circular
    # refs inside the compiler
    $self->{compiler}->end_component;

    if ($@)
    {
	$@->rethrow if UNIVERSAL::can( $@, 'rethrow' );
	HTML::Mason::Exception->throw( error => $@ );
    }
}

sub start
{
    my $self = shift;

    my $end;
    while ( defined  $self->{pos} ? $self->{pos} < length $self->{comp_text} : 1 )
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
    }

    if ( $self->{in_def} || $self->{in_method} )
    {
	my $type = $self->{in_def} ? 'def' : 'method';
	unless ( $end =~ m,</%\Q$type\E>\n?, )
	{
	    my $block_name = $self->{"in_$type"};
	    HTML::Mason::Exception::Syntax->throw( error => "No </%$type> tag for <%$type $block_name> block" );
	}
    }
}

sub match_block
{
    my $self = shift;

    my $comp = $self->{comp_text};
    pos($comp) = $self->{pos};
    if ( $comp =~ /\G<%($blocks_re)>/igcs )
    {
	$self->{pos} = pos($comp);

	my $type = lc $1;
	$self->{compiler}->start_block( block_type => $type );

	my $method = $blocks{$type};
	$self->$method( block_type => $type );

	return 1;
    }
}

sub raw_block
{
    my $self = shift;
    my %p = @_;

    my $comp = $self->{comp_text};
    pos($comp) = $self->{pos};
    if ( $comp =~ m,\G(.*?)</%\Q$p{block_type}\E>(\n?),igcs )
    {
	$self->{pos} = pos($comp);

	my $block = $1;
	if (defined $block)
	{
	    $self->{compiler}->raw_block( block_type => $p{block_type},
					  block => $block );
	    $self->{lines} += $block =~ tr/\n/\n/;
	    $self->{lines}++ if $2;
	}

	$self->{compiler}->end_block( block_type => $p{block_type} );
    }
    else
    {
	HTML::Mason::Exception::Syntax->throw( error => "<%$p{block_type}> tag at line $self->{lines} has no matching </%$p{block_type}> tag" );
    }
}

sub variable_list_block
{
    my $self = shift;
    my %p = @_;

    my $comp = $self->{comp_text};
    pos($comp) = $self->{pos};
    while ( $comp =~ m,\G               # last pos matched
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
	$self->{pos} = pos($comp);

	if ( $1 && $2 )
	{
	    $self->{compiler}->variable_declaration( block_type => $p{block_type},
						     type => $1,
						     name => $2,
						     default => $3,
						   );
	}
	$self->{lines}++;
    }

    if ( $comp =~ m,\G</%\Q$p{block_type}\E>(\n?),igcs )
    {
	$self->{pos} = pos($comp);
	$self->{compiler}->end_block( block_type => $p{block_type} );
	$self->{lines}++ if $1;
    }
    else
    {
	my $line = $self->_next_line;
	HTML::Mason::Exception::Syntax->throw( error => "Invalid <%$p{block_type}> section line at line $self->{lines}:\n$line" );
    }
}

sub key_val_block
{
    my $self = shift;
    my %p = @_;

    my $comp = $self->{comp_text};
    pos($comp) = $self->{pos};
    while ( $comp =~ /\G
                      [ \t]*
                      ([\w_]+)          # identifier
                      [ \t]*=>[ \t]*    # separator
                      (\S[^\n]*)        # value ( must start with a non-space char)
                      \n
                      |
                      \G[ \t]*\n
                     /gcx )
    {
	$self->{pos} = pos($comp);
	if ($1 && $2)
	{
	    $self->{compiler}->key_value_pair( block_type => $p{block_type},
					       key => $1,
					       value => $2
					     );
	}
	$self->{lines}++;
    }

    if ( $comp =~ m,\G</%\Q$p{block_type}\E>(\n?),igcs )
    {
	$self->{pos} = pos($comp);
	$self->{compiler}->end_block( block_type => $p{block_type} );
	$self->{lines}++ if $1;
    }
    else
    {
	my $line = $self->_next_line;
	HTML::Mason::Exception::Syntax->throw( error => "Invalid <%$p{block_type}> section line at line $self->{lines}:\n$line" );
    }
}

sub match_named_block
{
    my $self = shift;
    my %p = @_;

    my $comp = $self->{comp_text};
    pos($comp) = $self->{pos};
    if ( $comp =~ /\G<%(def|method)\s+([^\n]+?)>/igcs )
    {
	$self->{pos} = pos($comp);
	my ($type, $name) = ($1, $2);
	$self->{compiler}->start_named_block( block_type => $type,
					      name => $name );

	# This will cause ->start to return once it hits the
	# appropriate ending tag.
	local $self->{ending} = qr,\G</%\Q$type\E>\n?,i;

	$self->{"in_$type"} = $name;

	$self->start();

	$self->{"in_$type"} = undef;

	$self->{compiler}->end_named_block( block_type => $type );

	return 1;
    }
}

sub match_substitute
{
    my $self = shift;

    my $comp = $self->{comp_text};
    pos($comp) = $self->{pos};
    if ( $comp =~ /\G<%/gcs )
    {
	$self->{pos} = pos($comp);
	if ( $comp =~ /\G(.+?)(\s*\|\s*([a-z]+)?\s*)?%>/igcs )
	{
	    $self->{pos} = pos($comp);
	    my ($sub, $escape) = ($1, $3);
	    $self->{compiler}->substitution( substitution => $sub,
					     escape => $escape );

	    # Add it in just to count lines
	    $sub .= $2 if $2;
	    $self->{lines} += $sub =~ tr/\n/\n/;

	    return 1;
	}
	else
	{
	    my $line = $self->_next_line( $self->{pos} - 2 );
	    HTML::Mason::Exception::Syntax->throw( error => "'<%' without matching '%>' at $self->{lines}:\n$line" );
	}
    }
}

sub match_comp_call
{
    my $self = shift;

    my $comp = $self->{comp_text};
    pos($comp) = $self->{pos};
    if ( $comp =~ /\G<&(?!\|)/gcs )
    {
	$self->{pos} = pos($comp);
	if ( $comp =~ /\G(.*?)&>/gcs )
	{
	    $self->{pos} = pos($comp);

	    my $call = $1;
	    $self->{compiler}->component_call( call => $call );
	    $self->{lines} += $call =~ tr/\n/\n/;

	    return 1;
	}
	else
	{
	    my $line = $self->_next_line( $self->{pos} - 2 );
	    HTML::Mason::Exception::Syntax->throw( error => "'<&' without matching '&>' at $self->{lines}:\n$line" );
	}
    }
}


sub match_comp_content_call
{
    my $self = shift;

    my $comp = $self->{comp_text};
    pos($comp) = $self->{pos};
    if ( $comp =~ /\G<&\|/gcs )
    {
	$self->{pos} = pos($comp);
	if ( $comp =~ /\G(.*?)&>/gcs )
	{
	    $self->{pos} = pos($comp);

	    my $call = $1;
	    $self->{compiler}->component_content_call( call => $call );
	    $self->{lines} += $call =~ tr/\n/\n/;

	    return 1;
	}
	else
	{
	    my $line = $self->_next_line( $self->{pos} - 3 );
	    HTML::Mason::Exception::Syntax->throw( error => "'<&|' without matching '&>' at $self->{lines}:\n$line" );
	}
    }
}

sub match_comp_content_call_end
{
    my $self = shift;

    my $comp = $self->{comp_text};
    pos($comp) = $self->{pos};

    if ( $comp =~ m,\G</&>,gc )
    {
        $self->{pos} = pos($comp);

        $self->{compiler}->component_content_call_end;

        return 1;
    }
}

sub match_perl_line
{
    my $self = shift;

    my $comp = $self->{comp_text};
    pos($comp) = $self->{pos};
    if ( $comp =~ /\G(?:(?<=\n)|\A)%([^\n]+)(?:\n|\z)/gcs )
    {
	$self->{pos} = pos($comp);

	$self->{compiler}->perl_line( line => $1 );
	$self->{lines}++;

	return 1;
    }
}

sub match_text
{
    my $self = shift;

    my $comp = $self->{comp_text};
    pos($comp) = $self->{pos};
    if ( $comp =~ m,\G
                    (.*?)       # anything
		    (           # followed by
                     %          # an eval line
                     |
                     <%         # a substitution or tag start
                     |
                     </%        # a tag end
                     |
                     <&         # a comp call start
                     |
                     </&        # a comp call end
                     |
                     \\\n       # an escaped newline
                     |
                     \z         # or EOF.
                    )
                   ,gcsx
       )
    {
	# put back anything but an escaped newline or EOF
	pos($comp) -= length $2 if defined $2 && $2 ne "\\\n";

	$self->{pos} = pos($comp);

	my $text = $1;

	$self->{compiler}->text( text => $text );
	$self->{lines} += $text =~ tr/\n/\n/;
    }
}

sub match_end
{
    my $self = shift;

    # $self->{ending} is a qr// 'string'.  No need to escape.  It will
    # also include the needed \G marker
    my $comp = $self->{comp_text};
    pos($comp) = $self->{pos};
    if ( $comp =~ /($self->{ending})/gcs )
    {
	$self->{pos} = pos($comp);

	my $text = $1;
	if (defined $text)
	{
	    $self->{lines} += $text =~ tr/\n/\n/;
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
	     ( substr( $self->{comp_text}, $self->{pos}, 1 ) =~ /\n/ ?
	       $self->{pos} + 1 :
	       $self->{pos} ) );

    my $to_eol = ( index( $self->{comp_text}, "\n", $pos ) != -1 ?
		   ( index( $self->{comp_text}, "\n" , $pos ) ) - $pos :
		   length $self->{comp_text} );
    return substr( $self->{comp_text}, $pos, $to_eol );
}

sub line_count
{
    my $self = shift;

    return $self->{lines};
}

sub name
{
    my $self = shift;

    return $self->{name};
}

1;
