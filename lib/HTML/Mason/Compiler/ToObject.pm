# Copyright (c) 1998-99 by Jonathan Swartz. All rights reserved.
# This program is free software; you can redistribute it and/or modify it
# under the same terms as Perl itself.

package HTML::Mason::Compiler::ToObject;

use strict;

use base qw( HTML::Mason::Compiler );

use Exception::Class qw( Mason::Exception::Compiler );

use HTML::Mason::MethodMaker
    ( read_write => [ qw( comp_class
                          in_package
			  postamble
			  preamble
			  use_strict
                        )
		    ],
    );

my %fields =
    ( comp_class => 'HTML::Mason::Component',
      in_package => 'HTML::Mason::Commands',
      postamble => '',
      preamble => '',
      use_strict => 1,
    );

sub new
{
    my $proto = shift;
    my $class = ref $proto || $proto;
    my %p = @_;

    my $self = bless {}, $class;

    foreach ( keys %p )
    {
	$self->{$_} = $p{$_};
    }
    foreach ( keys %fields )
    {
	$self->{$_} ||= $fields{$_};
    }

    $self->_init;

    return $self;
}

sub compiled_component
{
    my $self = shift;

    $self->{compiled_subcomponents} = $self->_compile_subcomponents;
    $self->{compiled_methods} = $self->_compile_methods;

    $self->{current_comp} = $self;

    my $header = $self->_make_main_header;
    my $params = $self->_component_params;

    # Maybe use some sort of checksum of lexer & compiler names and versions?
    $params->{parser_version} = '';
    $params->{create_time} = time;

    $params->{subcomponents} = '\%_subcomponents' if %{ $self->{subcomponents} };
    $params->{methods} = '\%_methods' if %{ $self->{methods} };

    if ( $self->{shared} )
    {
	my %subs;
	while ( my ($name, $pref) = each %{ $self->{compiled_subcomponents} } )
	{
	    my $key = "subcomponent_$name";
	    $subs{$key} = $pref->{code};
	    $pref->{code} = "sub {\n\$m->call_dynamic('$key',\@_)\n}";
	}
	while (my ($name, $pref) = each %{ $self->{compiled_methods} } )
	{
	    my $key = "method_$name";
	    $subs{$key} = $pref->{code};
	    $pref->{code} = "sub {\n\$m->call_dynamic( '$key', \@_ )\n}";
	}
	$subs{main} = $params->{code};
	$params->{code} = "sub {\n\$m->call_dynamic( 'main', \@_ )\n}";

	$params->{dynamic_subs_init} =
	    join '', ( "sub {\n",
		       $self->{current_comp}{shared},
		       "return {\n",
		       join( ",\n", map { "'$_' => $subs{$_}" } sort keys %subs ),
		       "\n}\n}"
		     );
    }

    $params->{object_size} = (length $header) + (length join '', values %$params, keys %$params);

    my $object = join '', ( $header,
			    $self->_constructor( $self->{comp_class},
						 $params ),
			    $self->_subcomponents_footer,
			    $self->_methods_footer,
			  );

    $self->{current_comp} = undef;

    return $object;
}

sub _compile_subcomponents
{
    my $self = shift;

    return $self->_compile_subcomponents_or_methods('subcomponents');
}

sub _compile_methods
{
    my $self = shift;

    return $self->_compile_subcomponents_or_methods('methods');
}

sub _compile_subcomponents_or_methods
{
    my $self = shift;
    my $type = shift;

    return unless %{ $self->{$type} };

    my %compiled;
    foreach ( keys %{ $self->{$type} } )
    {
	$self->{current_comp} = $self->{$type}{$_};
	$compiled{$_} = $self->_component_params;
    }

    return \%compiled;
}

sub _make_main_header
{
    my $self = shift;

    my $pkg = $self->in_package;
    return join '', ( "package $pkg;\n",
		      $self->use_strict ? "use strict;\n" : '',
		      sprintf( "use vars qw(\%s);\n",
			       join ' ', '$m', $self->allow_globals ),
		      "my \$_escape = \\&HTML::Mason::Parser::_escape_perl_expression;\n",
		      $self->{once},
		    );
}

sub _subcomponents_footer
{
    my $self = shift;

    return $self->_subcomponent_or_method_footer('subcomponents');
}

sub _methods_footer
{
    my $self = shift;

    return $self->_subcomponent_or_method_footer('methods');
}

sub _subcomponent_or_method_footer
{
    my $self = shift;
    my $type = shift;

    return '' unless %{ $self->{current_comp}{$type} };

    my $comp_class = 'HTML::Mason::Component::Subcomponent';

    return join '', ( "my %_$type =\n(\n",
		      join ( ",\n",
			     map { "'$_' => " .
				   $self->_constructor( $comp_class,
							$self->{"compiled_$type"}{$_} ) }
			     keys %{ $self->{"compiled_$type"} }
			   ),
		      "\n);\n"
		    );
}

sub _constructor
{
    my $self = shift;
    my $class = shift;
    my $params = shift;

    return join '', ( "$class\->new\n(\n",
		      join ( ",\n",
			     map {"'$_' => $params->{$_}" } sort keys %$params ),
		      "\n)\n",
		    );
}

sub _component_params
{
    my $self = shift;

    my %params = ( code => join ( '', "sub {\n", $self->_body, "}" ),
		 );

    $params{flags} = join '', "{\n", $self->_flags, "\n}" if keys %{ $self->{current_comp}{flags} };
    $params{attr}  = join '', "{\n", $self->_attr, "\n}" if keys %{ $self->{current_comp}{attr} };
    $params{declared_args} = join '', "{\n", $self->_declared_args, "\n}" if keys %{ $self->{current_comp}{args} };

    return \%params;
}

sub _body
{
    my $self = shift;

    my @args;
    if ( $self->{current_comp}{args} )
    {
	@args = ( <<'EOF',
if (@_ % 2 == 0) { %ARGS = @_ } else { die "Odd number of parameters passed to component expecting name/value pairs" }
EOF
		  $self->_arg_declarations,
		);
    }
    else
    {
	@args = ( "{ local \$^W; \%ARGS = \@_ unless (\@_ % 2); }\n" );
    }

    return join '', ( $self->preamble,
		      "my \%ARGS;\n",
		      @args,
		      "my \$_out = \$m->current_sink;\n",
		      "\$m->debug_hook( \$m->current_comp->path ) if ( \%DB:: );\n\n",
		      $self->{current_comp}{filter},
		      $self->{current_comp}{init},
		      $self->{current_comp}{body},
		      $self->{current_comp}{cleanup},
		      $self->postamble,
		      "return undef;\n",
		    );
}

sub _arg_declarations
{
    my $self = shift;

    my @args;
    foreach ( values %{ $self->{current_comp}{args} } )
    {
	my $default_val = ( defined $_->{default} ?
			    $_->{default} :
			    qq|die "no value sent for required parameter '$_->{name}'"|,
			  );
	# allow for comments after default declaration
	$default_val .= "\n" if defined $_->{default} && $_->{default} =~ /\#/;

	if ( $_->{type} eq '$' )
	{
	    push @args,
		"my $_->{type}$_->{name} = ( !exists \$ARGS{'$_->{name}'} ? $default_val : \$ARGS{'$_->{name}'} );";
	}
	# Array
	elsif ( $_->{type} eq '@' )
	{
	    push @args, ( "my $_->{type}$_->{name} = ( !exists \$ARGS{'$_->{name}'} ? $default_val : ",
			  "UNIVERSAL::isa( \$ARGS{'$_->{name}'}, 'ARRAY' ) ? \@{ \$ARGS{'$_->{name}'}}  : ( \$ARGS{'$_->{name}'} ) );",
			);
	}
	# Hash
	elsif ($_->{type} eq "\%") {
	    push @args, ( "my $_->{type}$_->{name} = ( !exists \$ARGS{'$_->{name}'} ? $default_val : ",
			  "UNIVERSAL::isa( \$ARGS{'$_->{name}'}, 'ARRAY' ) ? \@{ \$ARGS{'$_->{name}'} } : ",
			  "UNIVERSAL::isa( \$ARGS{'$_->{name}'}, 'HASH' ) ? \%{ \$ARGS{'$_->{name}'} } : ",
			  qq|die "single value sent for hash parameter '$_->{type}$_->{name}'");|,
			);
	}

	push @args, "\n";
    }

    return @args;
}

sub _flags
{
    my $self = shift;

    return $self->_flags_or_attr('flags');
}

sub _attr
{
    my $self = shift;

    return $self->_flags_or_attr('attr');
}

sub _flags_or_attr
{
    my $self = shift;
    my $type = shift;

    return join ",\n", ( map { "$_ => $self->{current_comp}{$type}{$_}" }
			 keys %{ $self->{current_comp}{$type} } );
}

sub _declared_args
{
    my $self = shift;

    my @args;

    foreach my $key ( sort keys %{ $self->{current_comp}{args} } )
    {
	my $val = $self->{current_comp}{args}{$key};
	my $def = defined $val->{default} ? "$val->{default}" : 'undef';
	$def =~ s,([\\']),\\$1,g;
	$def = "'$def'" unless $def eq 'undef';

	push @args, "'$key' => { default => $def }";
    }

    return join ",\n", @args;
}

1;
