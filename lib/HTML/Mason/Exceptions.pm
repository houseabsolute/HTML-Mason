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
	     abbr => 'compiler_error',
	     description => 'error thrown from the compiler' },

	   'HTML::Mason::Exception::Compilation' =>
	   { isa => 'HTML::Mason::Exception',
	     abbr => 'compilation_error',
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

# Turn on tracing for each exception class.
foreach my $pkg (keys(%e)) {
    $pkg->do_trace(1);
}

if ($HTML::Mason::DEBUG)
{
    Exception::Class::Base->Trace(1);
}

my %abbrs = map { $e{$_}{abbr} => $_ } grep {exists $e{$_}{abbr}} keys %e;

# The import() method allows this:
#  use HTML::Mason::Exceptions(abbr => ['error1', 'error2', ...]);
# ...
#  error1 "something went wrong";

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

package HTML::Mason::Exception;

use overload
    '""' => \&as_string,
    fallback => 1;

sub as_string
{
    my ($self) = @_;
    
    return $self->error;
}

sub filtered_frames
{
    my ($self) = @_;

    my (@frames);
    my $trace = $self->trace;
    my @ignore_subs =
	qw[
	   (eval)
	   Exception::Class::Base::throw
	   HTML::Mason::Commands::__ANON__
	   HTML::Mason::Component::run
	   HTML::Mason::Exceptions::__ANON__
	   HTML::Mason::Request::_run_comp
	   ];
    while (my $frame = $trace->next_frame)
    {
	last if ($frame->subroutine eq 'HTML::Mason::Request::exec');
	unless (grep($frame->subroutine eq $_, @ignore_subs) or
		($frame->subroutine eq 'HTML::Mason::Request::comp' and $frame->filename =~ /Request\.pm/)) {
	    push(@frames, $frame);
	}
    }
    return @frames;
}

sub analyze_error
{
    my ($self) = @_;

    my ($file, @msgs, @frames);
    
    @frames = $self->filtered_frames;
    if (UNIVERSAL::isa($self, 'HTML::Mason::Exception::Compilation')) {
	my $error = $self->error;
	while ($error =~ /(.*) at (.*) line (\d+)\./g) {
	    push(@msgs, [$1, $3]);
	    $file = $2;
	}
    } else {
	(my $msg = $self->error) =~ s/\n//g;
	@msgs = ([$msg, $self->line]);
	$file = $frames[0]->filename;
    }

    return { file    => $file,
	     errors  => \@msgs,
	     frames  => \@frames };
}

sub as_log_line
{
    my ($self) = @_;

    my $info = $self->analyze_error;
    my @fields;
    
    push(@fields, [File => $info->{file}]);
    push(@fields, [Errors => join(", ", map { sprintf("[%d:%s]", $_->[1], $_->[0]) } @{$info->{errors}})]);
    push(@fields, [Stack => join(", ", map { sprintf("[%s:%d]", $_->filename, $_->line) } @{$info->{frames}})]);

    return (join("\t", map { sprintf("%s: %s", $_->[0], $_->[1]) } @fields));
}

1;
