# Copyright (c) 1998-2002 by Jonathan Swartz. All rights reserved.
# This program is free software; you can redistribute it and/or modify
# it under the same terms as Perl itself.

package HTML::Mason::Compiler::ToObject;

use strict;

use Params::Validate qw(SCALAR);

use HTML::Mason::Compiler;
use base qw( HTML::Mason::Compiler );

use HTML::Mason::MethodMaker
    ( read_write => [ qw( comp_class
                          in_package
			  postamble
			  preamble
			  use_strict
                        )
		    ],
    );

__PACKAGE__->valid_params
    (
     comp_class    => { parse => 'string',  type => SCALAR, default => 'HTML::Mason::Component' },
     subcomp_class => { parse => 'string',  type => SCALAR, default => 'HTML::Mason::Component::Subcomponent' },
     in_package => { parse => 'string',  type => SCALAR, default => 'HTML::Mason::Commands' },
     postamble  => { parse => 'string',  type => SCALAR, default => '' },
     preamble   => { parse => 'string',  type => SCALAR, default => '' },
     use_strict => { parse => 'boolean', type => SCALAR, default => 1 },
    );

__PACKAGE__->contained_objects();

sub compile
{
    my $self = shift;
    my %p = @_;

    local $self->{comp_class} = $p{comp_class} if exists $p{comp_class};
    return $self->SUPER::compile( comp_text => $p{comp_text}, name => $p{name} );
}

sub object_id
{
    my $self = shift;

    local $self->{comp_class} = '';

    return $self->SUPER::object_id;
}

sub compiled_component
{
    my $self = shift;

    $self->{compiled_def} = $self->_compile_subcomponents if %{ $self->{def} };
    $self->{compiled_method} = $self->_compile_methods if %{ $self->{method} };

    $self->{current_comp} = $self;

    my $header = $self->_make_main_header;
    my $params = $self->_component_params;

    my $id = $self->object_id;
    $id =~ s,([\\']),\\$1,g;
    $params->{compiler_id} = "'$id'";
    $params->{create_time} = time;

    $params->{subcomps} = '\%_def' if %{ $self->{def} };
    $params->{methods} = '\%_method' if %{ $self->{method} };

    if ( $self->_blocks('shared') )
    {
	my %subs;
	while ( my ($name, $pref) = each %{ $self->{compiled_def} } )
	{
	    my $key = "subcomponent_$name";
	    $subs{$key} = $pref->{code};
	    $pref->{code} = "sub {\n\$m->call_dynamic('$key',\@_)\n}";
	}
	while (my ($name, $pref) = each %{ $self->{compiled_method} } )
	{
	    my $key = "method_$name";
	    $subs{$key} = $pref->{code};
	    $pref->{code} = "sub {\n\$m->call_dynamic( '$key', \@_ )\n}";
	}
	$subs{main} = $params->{code};
	$params->{code} = "sub {\n\$m->call_dynamic( 'main', \@_ )\n}";

	$params->{dynamic_subs_init} =
	    join '', ( "sub {\n",
		       $self->_blocks('shared'),
		       "return {\n",
		       join( ",\n", map { "'$_' => $subs{$_}" } sort keys %subs ),
		       "\n}\n}"
		     );
    }

    $params->{object_size} = (length $header) + (length join '', values %$params, keys %$params);

    my $object = join '', ( $header,
			    $self->_subcomponents_footer,
			    $self->_methods_footer,
			    $self->_constructor( $self->comp_class,
						 $params ),
			    ';',
			  );

    $object .= "\n\n# MASON COMPILER ID: $id\n";

    $self->{current_comp} = undef;

    return $object;
}

sub _compile_subcomponents
{
    my $self = shift;

    return $self->_compile_subcomponents_or_methods('def');
}

sub _compile_methods
{
    my $self = shift;

    return $self->_compile_subcomponents_or_methods('method');
}

sub _compile_subcomponents_or_methods
{
    my $self = shift;
    my $type = shift;

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
		      "my \$_escape = \\&HTML::Mason::Tools::escape_perl_expression;\n",
		      $self->_blocks('once'),
		    );
}

sub _subcomponents_footer
{
    my $self = shift;

    return $self->_subcomponent_or_method_footer('def');
}

sub _methods_footer
{
    my $self = shift;

    return $self->_subcomponent_or_method_footer('method');
}

sub _subcomponent_or_method_footer
{
    my $self = shift;
    my $type = shift;

    return '' unless %{ $self->{current_comp}{$type} };

    return join '', ( "my %_$type =\n(\n",
		      join ( ",\n",
			     map { "'$_' => " .
				   $self->_constructor( $self->{subcomp_class},
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
    $params{declared_args} = join '', "{\n", $self->_declared_args, "\n}"
	if @{ $self->{current_comp}{args} };

    return \%params;
}

sub _body
{
    my $self = shift;

    my @args;
    if ( @{ $self->{current_comp}{args} } )
    {
	@args = ( <<'EOF',
if (@_ % 2 == 0) { %ARGS = @_ } else { HTML::Mason::Exception::Params->throw( error => "Odd number of parameters passed to component expecting name/value pairs" ) }
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
		      $self->_filter_code,
		      "\$m->debug_hook( \$m->current_comp->path ) if ( \%DB:: );\n\n",
		      $self->_blocks('init'),
		      $self->{current_comp}{body},
		      $self->_blocks('cleanup'),
		      $self->postamble,
		      $self->_finish_filter,
		      "return undef;\n",
		    );
}

sub _filter_code
{
    my $self = shift;
    return unless $self->_blocks('filter');

    return ( "my \$filter = sub { local \$_ = shift;\n",
	     ( join ";\n", $self->_blocks('filter') ),
	     ";\n",
	     "return \$_;\n",
	     "};\n",
	     "\$m->push_buffer_stack( \$m->top_buffer->new_child( filter => \$filter ) );\n",
	   );
}

sub _finish_filter
{
    my $self = shift;
    return unless $self->_blocks('filter');

    return ( "\$m->top_buffer->flush;\n",
	     "\$m->pop_buffer_stack;\n",
	   );
}

sub _arg_declarations
{
    my $self = shift;

    my @args;
    foreach ( @{ $self->{current_comp}{args} } )
    {
	my $default_val = ( defined $_->{default} ?
			    $_->{default} :
			    qq|HTML::Mason::Exception::Params->throw( error => "no value sent for required parameter '$_->{name}'" )|,
			  );
	# allow for comments after default declaration
	$default_val .= "\n" if defined $_->{default} && $_->{default} =~ /\#/;

	if ( $_->{type} eq '$' )
	{
	    push @args,
		( "my $_->{type}$_->{name} = !exists \$ARGS{'$_->{name}'} ? $default_val : \$ARGS{'$_->{name}'};" );
	}
	# Array
	elsif ( $_->{type} eq '@' )
	{
	    push @args, ( "my $_->{type}$_->{name} = ( !exists \$ARGS{'$_->{name}'} ? $default_val :",
			  "UNIVERSAL::isa( \$ARGS{'$_->{name}'}, 'ARRAY' ) ? \@{ \$ARGS{'$_->{name}'}}  : ( \$ARGS{'$_->{name}'} ) );",
			);
	}
	# Hash
	elsif ( $_->{type} eq "\%" )
	{
	    push @args, ( "my $_->{type}$_->{name} = ( !exists \$ARGS{'$_->{name}'} ? $default_val :",
			  "UNIVERSAL::isa( \$ARGS{'$_->{name}'}, 'ARRAY' ) ? \@{ \$ARGS{'$_->{name}'} } : ",
			  "UNIVERSAL::isa( \$ARGS{'$_->{name}'}, 'HASH' ) ? \%{ \$ARGS{'$_->{name}'} } : ",
			  qq|HTML::Mason::Exception::Params->throw( error => "single value sent for hash parameter '$_->{type}$_->{name}'" ) );|,
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

    foreach my $arg ( sort {"$a->{type}$a->{name}" cmp "$b->{type}$b->{name}" }
		      @{ $self->{current_comp}{args} } )
    {
	my $def = defined $arg->{default} ? "$arg->{default}" : 'undef';
	$def =~ s,([\\']),\\$1,g;
	$def = "'$def'" unless $def eq 'undef';

	push @args, "  '$arg->{type}$arg->{name}' => { default => $def }";
    }

    return join ",\n", @args;
}

1;
