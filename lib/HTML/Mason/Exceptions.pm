package HTML::Mason::Exceptions;

use strict;

use vars qw($VERSION);

$VERSION = sprintf '%2d.%02d', q$Revision$ =~ /(\d+)\.(\d+)/;

my %e;

BEGIN
{
    %e = ( 'HTML::Mason::Exception' =>
	   { description => 'generic base class for all Mason exceptions',
	     abbr => 'error'},

	   'HTML::Mason::Exception::Abort' =>
	   { isa => 'HTML::Mason::Exception',
	     abbr => 'abort_error',
	     description => 'a component called $m->abort' },

	   'HTML::Mason::Exception::Compiler' =>
	   { isa => 'HTML::Mason::Exception',
	     abbr => 'compile_error',
	     description => 'error thrown from the compiler' },

	   'HTML::Mason::Exception::Compilation' =>
	   { isa => 'HTML::Mason::Exception',
	     abbr => 'compile_error',
	     description => "error thrown in eval of the code for a component" },

	   'HTML::Mason::Exception::Compilation::IncompatibleCompiler' =>
	   { isa => 'HTML::Mason::Exception',
	     abbr => 'wrong_compiler_error',
	     description => "a component was compiled by a compiler/lexer with incompatible options.  recompilation is needed" },

	   'HTML::Mason::Exception::Params' =>
	   { isa => 'HTML::Mason::Exception',
	     abbr => 'param_error',
	     description => 'invalid parameters were given to a method/function' },

	   'HTML::Mason::Exception::Syntax' =>
	   { isa => 'HTML::Mason::Exception',
	     abbr => 'syntax_error',
	     description => 'invalid syntax was found in a component' },

	   'HTML::Mason::Exception::System' =>
	   { isa => 'HTML::Mason::Exception',
	     abbr => 'system_error',
	     description => 'a system call of some sort failed' },

	   'HTML::Mason::Exception::VirtualMethod' =>
	   { isa => 'HTML::Mason::Exception',
	     abbr => 'virtual_error',
	     description => 'a virtual method was not overridden' },

	 );
}

use Exception::Class (%e);

if ($HTML::Mason::DEBUG)
{
    Exception::Class::Base->Trace(1);
}

my %abbrs = map { $e{$_}{abbr} => $_ } grep {exists $e{$_}{abbr}} keys %e;

# The import() method allows this:
#  use HTML::Mason::Exceptions('abbr' => {short_error => 'HTML::Mason::Exception::Longname',...});
# ...
#  short_error("something went wrong");

sub import
{
    my ($class, %args) = @_;
    return unless %args;

    if ($args{abbr})
    {
	my $caller = caller;
	foreach my $name (@{$args{abbr}}) 
	{
	    die "Unknown exception abbreviation '$name'" unless exists $abbrs{$name};
	    no strict 'refs';
	    *{"${caller}::$name"} = sub { $abbrs{$name}->throw( error => shift ) };
	}
    }
}

1;
