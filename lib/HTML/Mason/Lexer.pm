# Copyright (c) 1998-2000 by Jonathan Swartz. All rights reserved.
# This program is free software; you can redistribute it and/or modify it
# under the same terms as Perl itself.

package HTML::Mason::Lexer;

use strict;

use Exception::Class qw( Mason::Exception::Lexer );

my %fields =
    ( compiler => undef,
    );


# This is a block name and what method should be called to lex its
# contents if it is encountered.  'def' & 'method' blocks are special
# cases we actually call ->start again to recursively parse the
# contents of a subcomponent/method.  Theoretically, adding a block is
# as simple as adding an entry to this hash, and possibly a new
# contents lexing method.
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

sub new
{
    my $proto = shift;
    my $class = ref $proto || $proto;
    my %p = @_;

    my $self = bless {}, $class;

    foreach ( keys %p )
    {
	if ( exists $fields{$_} )
	{
	    $self->{$_} = $p{$_} || $fields{$_};
	}
	else
	{
	    Mason::Lexer::Exception::Params->throw( error => "Invalid option to new: '$_'");
	}
    }
    foreach ( keys %fields )
    {
	$self->{$_} ||= $fields{$_};
    }

    Mason::Lexer::Exception::Params->throw( error => "No compiler object provided in call to HTML::Mason::Lexer->new" )
	unless ref $self->{compiler} && $self->{compiler}->isa('HTML::Mason::Compiler');

    return $self;
}

sub lex
{
    my $self = shift;
    my %p = @_;

    $self->{comp} = $p{comp};
    $self->{comp} =~ s/\r//g;

    $self->{name} = $p{name};
    $self->{lines} = 1;

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

    die $@ if $@;
}

sub start
{
    my $self = shift;

    my $end;
    while ( defined  $self->{pos} ? $self->{pos} < length $self->{comp} : 1 )
    {
	last if $end = $self->match_end;

	$self->match_block && next;

	$self->match_named_block && next;

	$self->match_substitute && next;

	$self->match_comp_call && next;

	$self->match_perl_line && next;

	$self->match_text && next;
    }

    if ( $self->{in_def} || $self->{in_method} )
    {
	my $type = $self->{in_def} ? 'def' : 'method';
	my $expect = "</%$type>";
	unless ( $end eq $expect )
	{
	    my $block_name = $self->{"in_$type"};
	    Mason::Exception::Lexer->throw( error => "No $expect tag for <%$type $block_name> named block" );
	}
    }
}

sub match_block
{
    my $self = shift;

    my $comp = $self->{comp};
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

    my $comp = $self->{comp};
    pos($comp) = $self->{pos};
    if ( $comp =~ m,\G(.*?)</%\Q$p{block_type}\E>,igs )
    {
	$self->{pos} = pos($comp);

	my $block = $1;
	if (defined $block)
	{
	    $self->{compiler}->raw_block( block_type => $p{block_type},
					  block => $block );
	    $self->{lines} += $block =~ tr/\n\r/\n\n/;
	}

	$self->{compiler}->end_block( block_type => $p{block_type} );
    }
    else
    {
	Mason::Exception::Lexer->throw( error => "<%$p{block_type}> tag at line $self->{lines} has no matching </%$p{block_type}> tag" );
    }
}

sub variable_list_block
{
    my $self = shift;
    my %p = @_;

    my $comp = $self->{comp};
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
						     var_type => $1,
						     name => $2,
						     default => $3,
						   );
	}
	$self->{lines}++;
    }

    if ( $comp =~ m,\G</%\Q$p{block_type}\E>,gcs )
    {
	$self->{pos} = pos($comp);
	$self->{compiler}->end_block( block_type => $p{block_type} );
    }
    else
    {
	my $line = $self->_next_line;
	Mason::Exception::Lexer->throw( error => "Invalid <%$p{block_type}> section line at line $self->{lines}:\n$line" );
    }
}

sub key_val_block
{
    my $self = shift;
    my %p = @_;

    my $comp = $self->{comp};
    pos($comp) = $self->{pos};
    while ( $comp =~ /\G
                              [ \t]*
                              (\w+)             # identifier
                              [ \t]*=>[ \t]*    # separator
                              (\S[^\n]*)        # value ( must start with a non-space char)
                              \n
                              |
                              \G\n
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

    if ( $comp =~ m,\G</%\Q$p{block_type}\E>,gcs )
    {
	$self->{pos} = pos($comp);
	$self->{compiler}->end_block( block_type => $p{block_type} );
    }
    else
    {
	my $line = $self->_next_line;
	Mason::Exception::Lexer->throw( error => "Invalid <%$p{block_type}> section line at line $self->{lines}:\n$line" );
    }
}

sub match_named_block
{
    my $self = shift;
    my %p = @_;

    my $comp = $self->{comp};
    pos($comp) = $self->{pos};
    if ( $comp =~ /\G<%(def|method)\s+([^\n]+?)>/igcs )
    {
	$self->{pos} = pos($comp);
	my ($type, $name) = ($1, $2);
	$self->{compiler}->start_named_block( block_type => $type,
					      name => $name );

	# This will cause ->start to return once it hits the
	# appropriate ending tag.
	local $self->{ending} = qr,\G</%\Q$type\E>,i;

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

    my $comp = $self->{comp};
    pos($comp) = $self->{pos};
    if ( $comp =~ /\G<%/gcs )
    {
	$self->{pos} = pos($comp);
	if ( $comp =~ /\G(.+?)(\s*\|\s*([a-z]+)\s*)?%>/gcs )
	{
	    $self->{pos} = pos($comp);
	    my ($sub, $escape) = ($1, $3);
	    $self->{compiler}->substitution( substitution => $sub,
					     escape => $escape );

	    # Add it in just to count lines
	    $sub .= $2 if $2;
	    $self->{lines} += $sub =~ tr/\n\r/\n\n/;

	    return 1;
	}
	else
	{
	    my $line = $self->_next_line( $self->{comp} - 2 );
	    Mason::Exception::Lexer->throw( error => "'<%' without matching '%>' at $self->{lines}:\n$line" );
	}
    }
}

sub match_comp_call
{
    my $self = shift;

    my $comp = $self->{comp};
    pos($comp) = $self->{pos};
    if ( $comp =~ /\G<&/gcs )
    {
	$self->{pos} = pos($comp);
	if ( $comp =~ /\G(.*?)&>/gcs )
	{
	    $self->{pos} = pos($comp);

	    my $call = $1;
	    $self->{compiler}->component_call( call => $call );
	    $self->{lines} += $call =~ tr/\n\r/\n\n/;

	    return 1;
	}
	else
	{
	    my $line = $self->_next_line( $self->{pos} - 2 );
	    Mason::Exception::Lexer->throw( error => "'<&' without matching '&>' at $self->{lines}:\n$line" );
	}
    }
}

sub match_perl_line
{
    my $self = shift;

    my $comp = $self->{comp};
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

    my $comp = $self->{comp};
    pos($comp) = $self->{pos};
    if ( $comp =~ /\G
                           (.+?)        # anything
			   (?=          # followed by (use lookahead so as to not consume text)
                             %          # an eval line
                             |
                             <%         # a substitution or tag start
                             |
                             <\/%       # a tag end
                             |
                             <&         # a comp call
                             |
                             \z         # or EOF.
                           )
                          /gcsx
       )
    {
	$self->{pos} = pos($comp);

	my $text = $1;
	$self->{compiler}->text( text => $text );
	$self->{lines} += $text =~ tr/\n\r/\n\n/;
    }
}

sub match_end
{
    my $self = shift;

    # $self->{ending} is a qr// 'string'.  No need to escape.  It will
    # also include the needed \G marker
    my $comp = $self->{comp};
    pos($comp) = $self->{pos};
    if ( $comp =~ /($self->{ending})/gcs )
    {
	$self->{pos} = pos($comp);

	my $text = $1;
	if (defined $text)
	{
	    $self->{lines} += $text =~ tr/\n\r/\n\n/;
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
	     ( substr( $self->{comp}, $self->{pos}, 1 ) =~ /\n/ ?
	       $self->{pos} + 1 :
	       $self->{pos} ) );

    my $to_eol = ( index( $self->{comp}, "\n", $pos ) != -1 ?
		   ( index( $self->{comp}, "\n" , $pos ) ) - $pos :
		   length $self->{comp} );
    return substr( $self->{comp}, $pos, $to_eol );
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
