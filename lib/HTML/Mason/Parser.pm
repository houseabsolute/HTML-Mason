# Copyright (c) 1998-99 by Jonathan Swartz. All rights reserved.
# This program is free software; you can redistribute it and/or modify it
# under the same terms as Perl itself.

package HTML::Mason::Parser;
require 5.004;

use strict;
use Data::Dumper;
use File::Path;
use File::Basename;
use File::Find;
use HTML::Mason::Component::FileBased;
use HTML::Mason::Component::Subcomponent;
use HTML::Mason::Request;
use HTML::Mason::Tools qw(dumper_method read_file);

# Fields that can be set in new method, with defaults
my %fields =
    (allow_globals => [],
     default_escape_flags => '',
     ignore_warnings_expr => 'Subroutine .* redefined',
     in_package => 'HTML::Mason::Commands',
     postamble => '',
     postprocess => undef,
     preamble => '',
     preprocess => undef,
     taint_check => 0,
     use_strict => 1,
     );

my %valid_comp_flags = (map(($_,1),qw(inherit)));
my %valid_escape_flags = map(($_,1),qw(h n u));

#
# This version number, less than or equal to the Mason version, marks the
# points at which the Parser produces incompatible object files.
#
sub version
{
    return 0.8;
}

sub new
{
    my $class = shift;
    my $self = {
	_permitted => \%fields,
	%fields,
    };
    my (%options) = @_;
    while (my ($key,$value) = each(%options)) {
	if (exists($fields{$key})) {
	    $self->{$key} = $value;
	} else {
	    die "HTML::Mason::Parser::new: invalid option '$key'\n";
	}
    }
    bless $self, $class;
    return $self;
}

sub allow_globals
{
    my ($self, @decls) = @_;
    if (my @bad = grep(!/^[\$@%]/,@decls)) {
	die "allow_globals: bad parameter '$bad[0]', must begin with one of $, @, %\n";
    }
    my %h = map {$_=>1} (@{$self->{'allow_globals'}},@decls);
    $self->{'allow_globals'} = [keys(%h)];
    return @{$self->{'allow_globals'}};
}

sub make_component
{
    my ($self, %options) = @_;
    my $objectTextRef = $options{object_text};

    my $objectText = $self->parse_component(%options) or return undef;
    $$objectTextRef = $objectText if defined($objectTextRef);
    return $self->eval_object_text(object_text=>$objectText,error=>$options{error});
}

#
# Old parse function, left in for sake of content management
#
sub parse
{
    my ($self, %options) = @_;
    my $error;
    my %subopts = ();
    foreach my $key (qw(script script_file)) {
	$subopts{$key} = $options{$key} if exists($options{$key});
    }
    my $objectText = $self->parse_component(%subopts,error=>\$error);
    $self->eval_object_text(object_text=>$objectText,error=>\$error) if $objectText;
    if ($objectText && !$error && exists($options{save_to})) {
	$self->write_object_file(object_text=>$objectText,object_file=>$options{save_to});
    }
    if ($objectText and my $ref = $options{result_text}) {
	$$ref = $objectText;
    }
    if ($error and my $ref = $options{error}) {
	$$ref = $error;
    }
    return $error ? 0 : 1;
}

sub parse_component
{
    my ($self, %options) = @_;

    #
    # We want to store all this in the object so it can be easily
    # parsed in multiple methods.  We alias to $state for ease of
    # typing.
    #
    # It's declared local so that if parse_component is entered again
    # to parse a subcomponent we won't lose all the stuff we've
    # already saved.
    #
    local $self->{parser_state} = {};
    my $state = $self->{parser_state};
    $state->{script} = $options{script};

    $state->{comp_class} = $options{comp_class} || 'HTML::Mason::Component';
    $state->{embedded} = $options{embedded};

    $state->{error_ref} = $options{error};
    $state->{errpos_ref} = $options{errpos};

    #
    # If script_file option used, read script from file.
    #
    if (!defined($state->{script})) {
	die "parse: must specify script or script_file\n" 
	    unless defined $options{script_file};
	$state->{script} = read_file($options{script_file});
    }

    #
    # Eliminate DOS ctrl-M chars
    #
    $state->{script} =~ s/\cM//g;

    #
    # _Everything_ gets wrapped in an eval so we can die anywhere and
    # just call _handle_parse_error.
    #
    my $result;
    eval
    {
	#
	# Preprocess the script.  The preprocessor routine is handed a
	# reference to the entire script.
	#
	if ($self->{preprocess}) {
	    eval {$self->{preprocess}->(\$state->{script})};
	    if ($@) {
		die { err => "error during custom preprocess step:\n$@" };
	    }
	}

	#
	# $curpos is fairly obvious, as is $script_length
	#
	# $startline keeps track of whether the next text range starts
	# at the beginning of a line. This becomes important later,
	# when looking for %-lines.
	#
	my $curpos = 0;
	my $startline = 1;
	my $script_length = length($state->{script});

	#
	# output_sections contains a list of sections parsed by
	# various routines called from _parse_textseg, which is
	# everything that is not in a mason section besides <%perl>.
	#
	# subcomponents is a hash of subcomponent names to parsed
	# subcomponents.
	#
	# methods is a hash of method names to parsed methods (which
	# are just subcomponents.
	#
	# declared_args contains something like:
	#   { '$foo' => { default => 'x' },
	#     '@bar' => { default => '(1, 2, 3)' },
	#     '%baz' => { default => undef } }
	#
	$state->{output_sections} = [];
	$state->{subcomponents} = {};
	$state->{methods} = {};
	$state->{declared_args} = {};

	# def and method are special cases.
	my @tags = qw( args attr cleanup
		       doc filter flags
		       init once shared text );

	foreach my $t (@tags)
	{
	    $state->{$t} = '';
	}

	my $comp_names = join '|', @tags;
	while ( $state->{script} =~
		/(                     # $1: the full tag match
                  <%
                   (?:perl_)?          # optional perl_ prefix
                   ($comp_names|       # $2: allowed tag names plus ...
                    (?:def|method)      # def or method followed by anything
                    ( [^>\n]* )         # that's not '>' or a newline
                                        # (which is the name)
                                        # $3: subcomp or method name
                   )
                  >
                 )/xigo
	      )
	{
	    my $section_name = lc $2;
	    $section_name = 'def' if substr($section_name,0,3) eq 'def';
	    $section_name = 'method' if substr($section_name,0,6) eq 'method';

	    my $section_start = pos($state->{script});
	    my $section_tag_pos = $section_start - length($1);
	    my $subcomp_name = $3;
	    if (defined($subcomp_name)) {
		for ($subcomp_name) { s/^\s+//; s/\s+$//; }
	    }

	    $self->_parse_textseg( segbegin => $curpos,
				   length => $section_tag_pos - $curpos,
				   startline => $startline )
		if $curpos < $section_tag_pos;

	    if ($state->{script} =~ m/(<\/%(?:perl_)?$section_name>\n?)/ig) {
		my $ending_tag = $1;
		my $section_end = pos($state->{script}) - length($ending_tag);
		my $section = substr($state->{script}, $section_start, $section_end - $section_start);
		if ($section_name eq 'text') {
		    # Special case for <%text> sections: add a special
		    # segment that won't get parsed
		    $self->_parse_textseg( segbegin => $section_start,
					   length => $section_end - $section_start,
					   startline => 0,
					   noparse => 1 );
		} elsif ( $section_name eq 'def' || $section_name eq 'method' ) {
		    if ($state->{embedded}) {
			die $self->_make_error( error => "<%$section_name> not allowed inside <%def> or <%method>",
						errpos => $section_tag_pos );
		    }
		    my $method = '_parse_' . lc $section_name . '_section';
		    $self->$method( name => $subcomp_name,
				    section => $section,
				    section_start => $section_start,
				  );
		} else {
		    # For now at least, allow repeated sections
		    #if ( $state->{ lc $section_name } ) {
		    #    die $self->_make_error( error => "repeated <%$section_name> section",
		    #  		errpos => $section_tag_pos );
		    #}
		    if ( $state->{embedded} and ($section_name eq 'shared' or $section_name eq 'once') ) {
			die $self->_make_error( error => "<%$section_name> not allowed inside <%def> or <%method>",
						errpos => $section_tag_pos );
		    }
		    my $method = '_parse_' . lc $section_name . '_section';
		    $self->$method( section => $section );
		}
		$curpos = pos($state->{script});
		$startline = substr($ending_tag, -1, 1) eq "\n";
	    } else {
		die $self->_make_error( error => "<%$section_name> with no matching </%$section_name>",
					errpos => $section_tag_pos );
	    }
	}

	$self->_parse_textseg( segbegin => $curpos,
			       length => $script_length - $curpos,
			       startline => $startline )
	    if $curpos < $script_length;

	if ($state->{embedded}) {
	    $result = $self->_build_embedded_component;
	} else {
	    $result = $self->_build_main_component;
	}
    };
    # End of eval {}

    # $@ will contain a string with relevant info if anything died.
    $self->_handle_parse_error($@) if $@;

    # Clear out everything.
    $self->{parser_state} = {};

    return $result;
}

sub _parse_def_section
{
    my $self = shift;
    my %params = @_;

    my $objtext = $self->_parse_subcomponent_or_method(@_, type => 'subcomponent');

    $self->{parser_state}{subcomponents}{ $params{name} } = $objtext;
}

sub _parse_method_section
{
    my $self = shift;
    my %params = @_;

    my $objtext = $self->_parse_subcomponent_or_method(@_, type => 'method');

    $self->{parser_state}{methods}{ $params{name} } = $objtext;
}

sub _parse_subcomponent_or_method
{
    my $self = shift;
    my %params = @_;

    my $state = $self->{parser_state};

    # Make 'subcomponents' or 'methods';
    my $key = $params{type} . 's';

    # Special case for <%def> sections: compile section as component
    # and put object text in subcomps or methods hash (as
    # appropriate), keyed on def name
    if ($params{name} !~ /\S/) {
	die $self->_make_error( error => "must supply name for $params{type}",
				errpos => $params{section_start} );
    } elsif ($params{name} !~ /^[\w\-\.]+$/) {
	die $self->_make_error( error => "invalid $params{type} name '$params{name}': valid characters are [A-Za-z0-9._-]",
				errpos => $params{section_start} );
    } elsif (exists $state->{$key}{ $params{name} }) {
	die $self->_make_error( error => "multiple definitions for $params{type} '$params{name}'",
				errpos => $params{section_start} );
    } else {
	my ($suberr, $suberrpos);
	my $objtext = $self->parse_component( script => $params{section},
					      embedded => 1,
					      comp_class => 'HTML::Mason::Component::Subcomponent',
					      error => \$suberr,
					      errpos => \$suberrpos );
	if ($objtext) {
	    return $objtext;
	} else {
	    die $self->_make_error( error => "Error while parsing $params{type} '$params{name}':\n$suberr",
				    errpos => $params{section_start} + $suberrpos,
				    suberror => s/(line .*)\n$/($params{name} line .*)\n/ );
	}
    }
}

sub _parse_args_section
{
    my $self = shift;
    my %params = @_;

    my $state = $self->{parser_state};

    foreach my $v ( $self->_parse_var_decls( $params{section} ) ) {
	# %ARGS is automatic, so ignore explicit declaration.
	next if "$v->{type}$v->{name}" eq '%ARGS';

	$state->{declared_args}{"$v->{type}$v->{name}"} = {default=>$v->{default}};

	my $default_val = defined($v->{default}) ? $v->{default} : 
	    qq| die "no value sent for required parameter '$v->{name}'"|;
	$default_val .= "\n" if (defined($v->{default}) && $v->{default} =~ /\#/);   # allow comments

	# Scalar
	if ($v->{type} eq "\$") {
	    $state->{args} .= "my $v->{type}$v->{name} = (!exists \$ARGS{'$v->{name}'} ? $default_val : \$ARGS{'$v->{name}'});";
	}
	# Array
	elsif ($v->{type} eq "\@") {
	    $state->{args} .= "my $v->{type}$v->{name} = (!exists \$ARGS{'$v->{name}'} ? $default_val : ";
	    $state->{args} .= "ref(\$ARGS{'$v->{name}'}) eq 'ARRAY' ? \@{\$ARGS{'$v->{name}'}} : (\$ARGS{'$v->{name}'}));";
	}
	# Hash
	elsif ($v->{type} eq "\%") {
	    $state->{args} .= "my $v->{type}$v->{name} = (!exists \$ARGS{'$v->{name}'} ? $default_val : ";
	    $state->{args} .= "ref \$ARGS{'$v->{name}'} eq 'ARRAY' ? \@{\$ARGS{'$v->{name}'}} : ";
	    $state->{args} .= "ref \$ARGS{'$v->{name}'} eq 'HASH' ? \%{\$ARGS{'$v->{name}'}} : ";
	    $state->{args} .= "die \"single value sent for hash parameter '$v->{type}$v->{name}'\");";
	}

	$state->{args} .= "\n";
    }
}

sub _parse_var_decls
{
    my ($self, $section) = @_;

    my @decls = grep {/\S/} split /\n/, $section;

    my @vars;
    foreach my $decl (@decls)
    {
	my ($var,$default);
	my $split = index($decl,'=>');
	if ($split !=-1) {
	    $var = substr($decl,0,$split);
	    $default = substr($decl,$split+2);
	} else {
	    ($var) = ($decl =~ /^\s*(\S+)/);
	}
	for ($var) { s/^\s+//; s/\s+$//; }

	# Note this would allow illegal variable names like $81.
	my $type = substr($var, 0, 1);
	my $name = substr($var, 1);
	unless ( ( $type eq '$' ||
		   $type eq '@' ||
		   $type eq '%' ) &&
		 defined $name)
	{
	    die $self->_make_error( error => "unknown type for argument/attribute '$var': first character must be \$, \@, or \%" );
	}

	push @vars, {name=>$name,type=>$type,default=>$default};
    }

    return @vars;
}

sub _parse_filter_section
{
    my $self = shift;
    my %params = @_;

    my $state = $self->{parser_state};

    for ($params{section}) { s/^\s+//; s/\s+$//; }

    $state->{filter} .= join "\n", ( '{ my ($_c,$_r);',
				     'if ($m->call_self(\$_c,\$_r)) {'.'for ($_c) {',
				     $params{section},
				     '}',
				     '$m->out($_c);',
				     'return $_r }};');
}

sub _parse_init_section
{
    my $self = shift;
    my %params = @_;

    my $state = $self->{parser_state};

    $state->{init} .= $params{section}."\n";
}

sub _parse_cleanup_section
{
    my $self = shift;
    my %params = @_;

    my $state = $self->{parser_state};

    $state->{cleanup} .= $params{section}."\n";
}


sub _parse_once_section
{
    my $self = shift;
    my %params = @_;

    my $state = $self->{parser_state};

    $state->{once} .= $params{section}."\n";
}

sub _parse_shared_section
{
    my $self = shift;
    my %params = @_;

    my $state = $self->{parser_state};

    $state->{shared} .= $params{section}."\n";
}

sub _parse_flags_section
{
    my $self = shift;
    my %params = @_;

    my $state = $self->{parser_state};

    my ($hash,@keys) = $self->_parse_hash_pairs($params{section});
    foreach my $key (@keys) {
	die $self->_make_error( error => "invalid flag '$key'" ) unless $valid_comp_flags{$key};
    }

    $state->{flags} .= "," if $state->{flags} =~ /\S/;
    $state->{flags} .= $hash;
}

sub _parse_attr_section
{
    my $self = shift;
    my %params = @_;

    my $state = $self->{parser_state};

    my ($hash) = $self->_parse_hash_pairs($params{section});

    $state->{attr} .= "," if $state->{attr} =~ /\S/;
    $state->{attr} .= $hash;
}

# used for attr & flags sections
sub _parse_hash_pairs
{
    my ($self, $section) = @_;

    my @keys;
    my @lines = grep {/\S/} split /\n/, $section;
    my $error_msg = "invalid <%$section%> syntax: each line must be a hash pair of the form 'name => value'";
    foreach my $line (@lines) {
	die $self->_make_error( error => $error_msg ) if (index($line,'=>')==-1);
	my ($key,$value) = split('=>',$line);
	for ($key) { s/^\s+//; s/\s+$//; }
	push(@keys,$key);
	$line .= "\n" if $value =~ /\#/;   # allow comments following value
    }
    
    my $hash = join ",\n", @lines;

    return ($hash,@keys);
}

sub _parse_doc_section
{
    # no-op
    1;
}

sub _parse_textseg
{
    my $self = shift;
    my %params = @_;

    my $state = $self->{parser_state};

    #
    # Parse in-line insertions, which take one of five forms:
    #   - Lines beginning with %
    #   - Text delimited by <%perl> </%perl>
    #   - Text delimited by <% %> 
    #   - Text delimited by <& &>
    # All else is a string to be delimited by single quotes and output.
    #
    # $s->{startline} keeps track of whether the first character is
    # the start of a line.
    # $s->{curpos} keeps track of where we are in $s->{text}.
    #
    my $s = $self->{parser_state}{text_parse_state} = {};
    $s->{startline} = $params{startline};
    $s->{curpos} = 0;

    my $text = substr($state->{script}, $params{segbegin}, $params{length});

    # Special case for <%text> sections.  Just push the whole chunk
    # onto output without checking for various embedded tags and such.
    if ($params{noparse}) {
	$self->_add_output_section( $self->_format_plaintext( text => $text,
							      start => 0,
							      end => length($text) ) );
	return;
    }

 WHILE:
    while ($s->{curpos} < $params{length}) {

	# Special case.  First line of the segment starts with '%';
	if ($s->{startline} && substr($text,$s->{curpos},1) eq '%') {
	    $self->_parse_perl_line( index => $s->{curpos},
				     segbegin => $params{segbegin},
				     text => $text );
	    next;
	}

	$s->{startline} = 0;

	my %h;
	$h{perl_line} = index($text,"\n%",$s->{curpos});
	$h{substitute_tag} = index($text,'<%', $s->{curpos});
	$h{call_tag}  = index($text,'<&',$s->{curpos});

	# Sort keys by values (thanks, Randall!)
	my @keys = ( map {$_->[0]}
		     sort { $a->[1] <=> $b->[1] }
		     map { [ $_, $h{$_} ] } keys %h );

	foreach my $k (@keys)
	{
	    next if $h{$k} == -1;

	    # We want to keep the newline from the match as part
	    # of the alphatext so we advance the cursor 1
	    # character.
	    $h{$k} += 1 if $k eq 'perl_line';

	    # Grab the alpha text before the tag we just found and
	    # add it in if there is any such text;
	    $self->_add_output_section( $self->_format_plaintext( text => $text,
								  start => $s->{curpos},
								  end => $h{$k} ) )
		if $s->{curpos} < $h{$k};

	    # See if <% is actually the first part of <%perl>.
	    my $method;
	    if ($k eq 'substitute_tag' and lc(substr($text,$h{$k},7)) eq '<%perl>') {
		$method = '_parse_perl_tag';
	    } else {
		$method = "_parse_$k";
	    }

	    $self->$method( index => $h{$k},
			    segbegin => $params{segbegin},
			    text => $text );
	    next WHILE;
	}

	# Grab whatever's left
	$self->_add_output_section( $self->_format_plaintext( text => $text,
							      start => $s->{curpos},
							      end => $params{length} ) );

	# This is the cue to exit the while loop.
	$s->{curpos} = $params{length};
    }
}

#
# Line beginning with %
#
sub _parse_perl_line
{
    my $self = shift;
    my %params = @_;

    my $s = $self->{parser_state}{text_parse_state};

    # Find beginning and end of code.
    my $endline = index($params{text},"\n",$params{index});
    $endline = length($params{text}) if $endline == -1;
    my $length = $endline - $params{index};

    $s->{startline} = 1;

    # From the char after % to the char before the newline.
    my $perl = substr($params{text}, $params{index} + 1, $length - 1);

    $perl = $self->{postprocess}->($perl, 'perl') if $self->{postprocess};
    $self->_add_output_section($perl);

    # Move cursor to newline
    $s->{curpos} = $endline + 1;
}

#
#<%perl></%perl>
#
sub _parse_perl_tag
{
    my $self = shift;
    my %params = @_;

    my $s = $self->{parser_state}{text_parse_state};

    pos($params{text}) = $s->{curpos};
    unless ($params{text} =~ m{</\%perl>}ig) {
	die $self->_make_error( error => "<%perl> with no matching </%perl>",
				errpos => $params{segbegin} + $params{index} );
    }

    # Move cursor to spot of last match (which is immediately after
    # the </%perl> tag
    $s->{curpos} = pos($params{text});

    # Subtract the index position (where the original match occurred)
    # plus the length of the <%perl> tag from the current position
    # (after the regex) minus the length of the </%perl> tag to get
    # the length.
    my $length = $s->{curpos} - 8 - ($params{index} + 7);

    my $perl = substr($params{text}, $params{index} + 7, $length);
    $perl = $self->{postprocess}->($perl, 'perl') if $self->{postprocess};
    $self->_add_output_section($perl);
}

#
# <% %> section
#
sub _parse_substitute_tag
{
    my $self = shift;
    my %params = @_;

    my $s = $self->{parser_state}{text_parse_state};

    # See if this is a mistaken <%xxx> command
    if (substr($params{text}, $params{index} + 2, 20) =~ /^(\w+)>/) {
	die $self->_make_error( error => "unknown section <%$1>",
				errpos => $params{segbegin} + $params{index} );
    }

    # Find the closing part of the tag.
    my $close = index($params{text},"%>", $params{index} + 2);
    if ($close == -1) {
	die $self->_make_error( error => "'<%' with no matching '%>'",
				errpos => $params{segbegin} + $params{index} );
    }
    my $length = $close - ($params{index} + 2);

    # Process escape flags, default and/or provided.
    my $expr = substr($params{text}, $params{index} + 2, $length);
    my $escape_flags = $self->default_escape_flags;
    if (my ($extra_escape_flags) = ($expr =~ /\|([A-Za-z ]+)$/)) {
	$expr = substr($expr,0,length($expr)-length($extra_escape_flags)-1);
	$escape_flags = '' if ($extra_escape_flags =~ /n/);
	$extra_escape_flags =~ s/[ n]//g;
	$escape_flags .= $extra_escape_flags;
    }
    my $perl;
    if ($escape_flags) {
	my %uniqf = map(($_,1),split('',$escape_flags));
	my @flag_list = keys(%uniqf);
	if (my (@invalids) = grep(!$valid_escape_flags{$_},@flag_list)) {
	    die $self->_make_error( error => "invalid <% %> escape flag: '$invalids[0]'",
				    errpos => $params{segbegin} + $params{index} );
	}
	$perl = '$_out->($_escape->('.$expr.','.join(",",map("'$_'",@flag_list)).'));';
    } else {
	$perl = '$_out->('.$expr.');';
    }

    $perl = $self->{postprocess}->($perl, 'perl') if $self->{postprocess};
    $self->_add_output_section($perl);

    $s->{curpos} = $params{index} + 2 + $length + 2;
}

sub _parse_call_tag
{
    my $self = shift;
    my %params = @_;

    my $s = $self->{parser_state}{text_parse_state};

    my $close = index($params{text} , "&>", $params{index} + 2);
    if ($close == -1) {
	die $self->_make_error( error => "'<&' with no matching '&>'",
				errpos => $params{segbegin} + $params{index} );
    }
    my $length = $close - ($params{index} + 2);

    my $call = substr($params{text}, $params{index} + 2, $length);
    for ($call) { s/^\s+//; s/\s+$//; }

    if ($call =~ /^[A-Za-z0-9\/_.]/) {
	# Literal component path; put quotes around it
	my $comma = index($call, ',');
	$comma = length $call if $comma == -1;
	(my $comp = substr($call, 0, $comma)) =~ s/\s+$//;
	$call = "'$comp'" . substr($call, $comma);
    }

    my $perl = "\$m->comp($call);";
    $perl = $self->{postprocess}->($perl, 'perl') if $self->{postprocess};
    $self->_add_output_section($perl);

    $s->{curpos} = $params{index} + 2 + $length + 2;
}

sub _format_plaintext
{
    my $self = shift;
    my %params = @_;

    return unless $params{end} - $params{start} > 0;

    my $alpha = substr($params{text}, $params{start}, $params{end} - $params{start});

    # Remove trailing newline (and backslash) if newline has been
    # 'escaped' with a backslash.
    if (substr($params{text}, $params{end} - 2, 2) eq "\\\n") {
	chop $alpha; chop $alpha;
    }

    $alpha =~ s{([\\\'])} {\\$1}g;   # escape backslashes and single quotes
    $alpha = sprintf q|$_out->('%s');|,$alpha;
    $alpha = $self->{postprocess}->($alpha, 'alpha') if $self->{postprocess};
    return $alpha;
}

sub _add_output_section
{
    my $self = shift;
    my $section = shift;

    my $state = $self->{parser_state};

    push @{ $state->{output_sections} }, $section;
}

sub _build_main_component
{
    my $self = shift;

    my $state = $self->{parser_state};

    my $body = $self->_build_body;
    my $cparams = $self->_build_params($body);

    # Add the params that are only applicable to main components
    $cparams->{parser_version} = $self->version;
    $cparams->{create_time} = time();

    # If component has a shared section, relocate subroutine definitions
    # under one scope with the shared code at the top.
    if ($state->{shared}) {
	my %subs;
	while (my ($name,$pref) = each(%{$state->{subcomponents}})) {
	    my $key = "subcomp_$name";
	    $subs{$key} = $pref->{'code'};
	    $pref->{'code'} = "sub {\n\$m->call_dynamic('$key',\@_)\n}";
	}
	while (my ($name,$pref) = each(%{$state->{methods}})) {
	    my $key = "method_$name";
	    $subs{$key} = $pref->{'code'};
	    $pref->{'code'} = "sub {\n\$m->call_dynamic('$key',\@_)\n}";
	}
	$subs{'main'} = $cparams->{'code'};
	$cparams->{'code'} = "sub {\n\$m->call_dynamic('main',\@_)\n}";

	$cparams->{dynamic_subs_init} = 
	    join("",
		 "sub {\n",
		 $state->{shared},
		 "return {\n",
		 join(",\n",map("'$_'=>".$subs{$_},sort keys %subs)),
		 "\n}\n}");
    }
    
    my $header = $self->_build_header;
    $cparams->{object_size} = length($header) + length (join("",values(%$cparams)));
    my $constructor = $self->_build_constructor($state->{comp_class},$cparams).";";
    
    return $header . $constructor;
}

sub _build_embedded_component
{
    my $self = shift;

    my $state = $self->{parser_state};

    my $body = $self->_build_body;
    return $self->_build_params($body);
}

sub _build_header
{
    my $self = shift;

    my $state = $self->{parser_state};

    #
    # Wrap body in subroutine and add header, including <%once> section.
    #
    my $header = '';
    my $pkg = $self->{in_package};
    unless ($state->{embedded})
    {
	$header .= "package $pkg;\n";
	$header .= "use strict;\n" if $self->use_strict;
	$header .= sprintf( "use vars qw(\%s);\n",
			    join(' ', '$m', @{$self->{'allow_globals'}} ) );
	$header .= 'my $_escape = \&HTML::Mason::Parser::_escape_perl_expression;'."\n";

    }
    $header .= $state->{once} if $state->{once};

    $header .= $self->_build_subcomponents_header;
    $header .= $self->_build_methods_header;

    return $header;
}

sub _build_subcomponents_header
{
    my $self = shift;

    return $self->_build_subcomponent_or_method_header('subcomponents');
}

sub _build_methods_header
{
    my $self = shift;

    return $self->_build_subcomponent_or_method_header('methods');
}

sub _build_subcomponent_or_method_header
{
    my ($self, $type) = @_;

    my $state = $self->{parser_state};

    return '' unless %{ $state->{$type} };

    my $code = '';
    my $comp_class = 'HTML::Mason::Component::Subcomponent';
    $code .= "my \%_$type =\n(\n";
    $code .= join ( ",\n",
		    map { "'$_' => ".$self->_build_constructor($comp_class,$state->{$type}{$_}) }
		    sort keys %{ $state->{$type} } );
    $code .= "\n);\n";

    return $code;
}

sub _build_params
{
    my ($self, $body) = @_;
    my $state = $self->{parser_state};

    #
    # Assemble parameters for component.
    #
    my %cparams = ();
    $cparams{'subcomps'} = '\%_subcomponents'
	if %{ $state->{subcomponents} };
    $cparams{'methods'} = '\%_methods'
	if %{ $state->{methods} };

    if (%{ $state->{declared_args} })
    {
	my $d = Data::Dumper->new([$state->{declared_args}]);
	# Brought in from Tools.
	my $dump = dumper_method($d);
	for ($dump) { s/\$VAR1\s*=//g; s/;\s*$// }
	$cparams{'declared_args'} = $dump;
    }

    $cparams{'flags'} = "{".$state->{flags}."}" if $state->{flags};
    $cparams{'attr'} = "{".$state->{attr}."}" if $state->{attr};

    $cparams{'code'} = "sub {\n$body\n}";

    return \%cparams;
}

sub _build_constructor
{
    my ($self,$comp_class,$cparams) = @_;

    my $cparams_string = join(",\n",map("'$_'=>".$cparams->{$_},sort keys %$cparams));
    return "$comp_class\->new\n(\n$cparams_string\n)\n";
}

sub _build_body
{
    my $self = shift;

    my $state = $self->{parser_state};

    #
    # Start body of subroutine with user preamble and args declare.
    #
    my $body = $self->preamble();

    $body .= 'my %ARGS;' . "\n";
    if ($state->{args})
    {
	$body .= 'if (@_ % 2 == 0) { %ARGS = @_ } else { die "Odd number of parameters passed to component expecting name/value pairs" }' . "\n";
	$body .= $state->{args};
    }
    else
    {
	$body .= '%ARGS = @_ unless (@_ % 2);' . "\n";
    }
    $body .= 'my $_out = $m->current_sink;'."\n";
    
    $body .= '$m->debug_hook($m->current_comp->path) if (%DB::);' . "\n\n";
    $body .= $state->{filter} if $state->{filter};
    $body .= $state->{init} if $state->{init};

    foreach my $o (@{ $state->{output_sections} })
    {
	$body .= "$o\n";
    }

    #
    # Insert <%cleanup> section.
    #
    $body .= $state->{cleanup} if $state->{cleanup};

    #
    # Insert user postamble and return undef by default.
    #
    $body .= $self->postamble();
    $body .= "return undef;\n";
}

#
# This is lame but it's a hack solution until we require 5.005+
#
sub _make_error
{
    my $self = shift;
    my %params = @_;

    my $d = Data::Dumper->new([\%params]);
    # Brought in from Tools.
    my $dump = dumper_method($d);
    for ($dump) { s/\$VAR1\s*=//g; s/;\s*$// }

    return "MASON: $dump\n";
}

sub _handle_parse_error
{
    my ($self, $errdump) = @_;

    # Just in case this isn't a die from one our methods but is some
    # sort of 'real' error generated in another module.  We need real
    # exceptions.  bleah.
    die $errdump unless substr($errdump,0,7) eq "MASON: ";
    my $err = eval(substr($errdump,7));
    die "assert: could not read _make_error output: $@" if $@;

    my $state = $self->{parser_state};

    #
    # Process parsing errors.
    #
    if (exists $err->{errpos}) {
	my $linenum = (substr($state->{script},0,$err->{errpos}) =~ tr/\n//) + 1;
	if ($err->{suberr}) {
	    $err->{error} .= " (main script line $linenum)";
	} else {
	    $err->{error} .= " (line $linenum)";
	}
	${ $state->{errpos_ref} } = $err->{errpos} if exists $state->{errpos_ref};
    }
    $err->{error} .= "\n";
    ${ $state->{error_ref} } = $err->{error} if exists $state->{error_ref};
}

#
# Process escape flags in <% %> tags
#   h - html escape
#   u - url escape
#
my %html_escape = ('&' => '&amp;', '>'=>'&gt;', '<'=>'&lt;', '"'=>'&quot;');
sub _escape_perl_expression
{
    my ($expr,@flags) = @_;
    foreach my $flag (@flags) {
	if ($flag eq 'h') {
	    $expr =~ s/([<>&\"])/$html_escape{$1}/mgoe;
	} elsif ($flag eq 'u') {
	    $expr =~ s/([^a-zA-Z0-9_.-])/uc sprintf("%%%02x",ord($1))/eg;
	}
    }
    return $expr;
}

#
# write_object_file
#   (object_text=>..., object_file=>..., files_written=>...)
# Save object text in an object file.
#
# We attempt to handle several cases in which a file already exists
# and we wish to create a directory, or vice versa.  However, not
# every case is handled; to be complete, mkpath would have to unlink
# any existing file in its way.
#
sub write_object_file
{
    my ($self, %options) = @_;
    my ($objectText,$objectFile,$filesWrittenRef) =
	@options{qw(object_text object_file files_written)};
    my @newfiles = ($objectFile);

    if (!-f $objectFile) {
	my ($dirname) = dirname($objectFile);
	if (!-d $dirname) {
	    unlink($dirname) if (-e $dirname);
	    push(@newfiles,mkpath($dirname,0,0775));
	    die "Couldn't create directory $dirname: $!" if (!-d $dirname);
	}
	rmtree($objectFile) if (-d $objectFile);
    }
    
    my $fh = new IO::File ">$objectFile" or die "Couldn't write object file $objectFile: $!";
    print $fh $objectText;
    $fh->close;
    @$filesWrittenRef = @newfiles if (defined($filesWrittenRef))
}

#
# eval_object_text
#   (object_text, object_file, error)
# Evaluate an object file or object text.  Return a component object
# or undef if error.
#
sub eval_object_text
{    
    my ($self, %options) = @_;
    my ($objectText,$objectFile,$errref) =
	@options{qw(object_text object_file error)};

    #
    # Evaluate object file or text with warnings on
    #
    my $ignoreExpr = $self->ignore_warnings_expr;
    my ($comp,$err);
    {
	my $warnstr;
	local $^W = 1;
	local $SIG{__WARN__} = $ignoreExpr ? sub { $warnstr .= $_[0] if $_[0] !~ /$ignoreExpr/ } : sub { $warnstr .= $_[0] };
	if ($objectFile) {
	    ($objectFile) = ($objectFile =~ /^(.*)$/s) if $self->taint_check;
	    $comp = do($objectFile);
	} else {
	    ($objectText) = ($objectText =~ /^(.*)$/s) if $self->taint_check;
	    $comp = eval($objectText);
	}
	$err = $warnstr . $@;
    }

    #
    # Detect various forms of older, incompatible object files:
    #  -- zero-sized files (previously signifying pure text components)
    #  -- pre-0.7 files that return code refs
    #  -- valid components but with an earlier parser_version
    #
    if ($objectFile) {
	my $parserVersion = version();
	my $incompat = "Incompatible object file ($objectFile);\nobject file was created by %s and you are running parser version $parserVersion.\nAsk your administrator to clear the object directory.\n";
	if (-z $objectFile) {
	    $err = sprintf($incompat,"a pre-0.7 parser");
	} elsif ($comp) {
	    if (ref($comp) eq 'CODE') {
		$err = sprintf($incompat,"a pre-0.7 parser");
	    } elsif (ref($comp) !~ /HTML::Mason::Component/) {
		$err = "object file ($objectFile) did not return a component object!";
	    } elsif ($comp->parser_version != $parserVersion) {
		$err = sprintf($incompat,"parser version ".$comp->parser_version);
	    }
	}
    }

    #
    # Return component or error
    #
    if ($err) {
	# attempt to stem very long eval errors
	if ($err =~ /has too many errors\./) {
	    $err =~ s/has too many errors\..*/has too many errors./s;
	}
	$$errref = $err if defined($errref);
	return undef;
    } else {
	return $comp;
    }
}

sub make_dirs
{
    my ($self, %options) = @_;
    my $compRoot = $options{comp_root} or die "make_dirs: must specify comp_root\n";
    my $dataDir = $options{data_dir} or die "make_dirs: must specify data_dir\n";
    die "make_dirs: source_dir '$compRoot' does not exist\n" if (!-d $compRoot);
    die "make_dirs: object_dir '$dataDir' does not exist\n" if (!-d $dataDir);
    my $sourceDir = $compRoot;
    my $objectDir = "$dataDir/obj";
    my $errorDir = "$dataDir/errors";
    my @paths = (exists($options{paths})) ? @{$options{paths}} : ('/');
    my $verbose = (exists($options{verbose})) ? $options{verbose} : 1;
    my $predicate = $options{predicate} || sub { $_[0] !~ /\~/ };
    my $dirCreateMode = $options{dir_create_mode} || 0775;
    my $reloadFile = $options{update_reload_file} ? "$dataDir/etc/reload.lst" : undef;
    my ($relfh);
    if (defined($reloadFile)) {
	$relfh = new IO::File ">>$reloadFile" or die "make_dirs: cannot open '$reloadFile' for writing: $!";
	$relfh->autoflush(1);
    }
    
    my $compilesub = sub {
	my ($srcfile) = $File::Find::name;
	return if (!-f $srcfile);
	return if defined($predicate) && !($predicate->($srcfile));
	my $compPath = substr($srcfile,length($sourceDir));
 	(my $objfile = $srcfile) =~ s@^$sourceDir@$objectDir@;
	my ($objfiledir) = dirname($objfile);
	if (!-d $objfiledir) {
	    if (defined($dirCreateMode)) {
		print "creating directory $objfiledir\n" if $verbose;
		mkpath($objfiledir,0,$dirCreateMode);
		die "make_dirs: cannot create directory '$objfiledir': $!" if (!-d $objfiledir);
	    } else {
		die "make_dirs: no such directory '$objfiledir'";
	    }
	}
	my $makeflag;
	if (!-e $objfile) {
	    $makeflag = 1;
	} else {
	    my $srcfilemod = [stat($srcfile)]->[9];
	    my $objfilemod = [stat($objfile)]->[9];
	    $makeflag = ($srcfilemod > $objfilemod);
	}
	if ($makeflag) {
	    my ($errmsg,$objText);
	    print "compiling $srcfile\n" if $verbose;
	    if ($self->make_component(script_file=>$srcfile, object_text=>\$objText, error=>\$errmsg)) {
		$self->write_object_file(object_file=>$objfile, object_text=>$objText);
	    } else {
		if ($verbose) {
		    print "error";
		    if ($errorDir) {
			(my $errfile = $srcfile) =~ s@^$sourceDir@$errorDir@;
			$self->write_object_file(object_file=>$errfile, object_text=>$objText);
			print " in $errfile";
		    }
		    print ":\n$errmsg\n";
		}
	    }
	}
    };

    foreach my $path (@paths) {
	my $fullpath = $sourceDir . $path;
	$fullpath =~ s@/$@@g;
	if (-f $fullpath) {
	    $compilesub->($fullpath);
	} elsif (-d $fullpath) {
	    my $sub = sub {$compilesub->($_) if -f};
	    find($sub,$fullpath);
	} else {
	    die "make_dirs: no such file or directory '$fullpath'";
	}
    }
}

# Create generic read-write accessor routines

sub default_escape_flags { my $s=shift; return @_ ? ($s->{default_escape_flags}=shift) : $s->{default_escape_flags} }
sub ignore_warnings_expr { my $s=shift; return @_ ? ($s->{ignore_warnings_expr}=shift) : $s->{ignore_warnings_expr} }
sub in_package { my $s=shift; return @_ ? ($s->{in_package}=shift) : $s->{in_package} }
sub postamble { my $s=shift; return @_ ? ($s->{postamble}=shift) : $s->{postamble} }
sub postprocess { my $s=shift; return @_ ? ($s->{postprocess}=shift) : $s->{postprocess} }
sub preamble { my $s=shift; return @_ ? ($s->{preamble}=shift) : $s->{preamble} }
sub preprocess { my $s=shift; return @_ ? ($s->{preprocess}=shift) : $s->{preprocess} }
sub taint_check { my $s=shift; return @_ ? ($s->{taint_check}=shift) : $s->{taint_check} }
sub use_strict { my $s=shift; return @_ ? ($s->{use_strict}=shift) : $s->{use_strict} }

1;

__END__
