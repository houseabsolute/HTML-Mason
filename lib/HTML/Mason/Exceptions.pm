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

	   'HTML::Mason::Exception::TopLevelNotFound' =>
	   { isa => 'HTML::Mason::Exception',
	     abbr => 'top_level_not_found_error',
	     description => 'the top level component could not be found' },

	   'HTML::Mason::Exception::VirtualMethod' =>
	   { isa => 'HTML::Mason::Exception',
	     abbr => 'virtual_error',
	     description => 'a virtual method was not overridden' },

	 );
}

use Exception::Class (%e);

HTML::Mason::Exception->Trace(1);

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

sub filtered_frames
{
    my ($self) = @_;

    my (@frames);
    my $trace = $self->trace;
    my %ignore_subs = map { $_ => 1 }
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
	unless ($ignore_subs{ $frame->subroutine } or
		($frame->subroutine eq 'HTML::Mason::Request::comp' and $frame->filename =~ /Request\.pm/)) {
	    push(@frames, $frame);
	}
    }
    return @frames;
}

sub analyze_error
{
    my ($self) = @_;

    my ($file, $msg, @lines, @frames);

    @frames = $self->filtered_frames;
    $msg = $self->error;
    if ($self->isa('HTML::Mason::Exception::Syntax')) {
	$file = $self->comp_name;
	push(@lines, $self->line_number);
    } else {
	$file = $frames[0]->filename;
	@lines = $frames[0]->line;
    }
    my @context = $self->get_file_context($file, \@lines);

    return {
	file    => $file,
	frames  => \@frames,
	lines   => \@lines,
	msg     => $msg,
	context => \@context,
    };
}

sub get_file_context
{
    my ($self, $file, $line_nums) = @_;

    my @context;
    my $fh = do { local *FH; *FH; };
    unless (open($fh, $file)) {
	@context = (['unable to open file', '']);
    } else {
	# Put the file into a list, indexed at 1.
	my @file = <$fh>;
	chomp(@file);
	unshift(@file, undef);

	# Mark the important context lines.
	# We do this by going through the error lines and incrementing hash keys to
	# keep track of which lines we eventually need to print, and we color the
	# line which the error actually occured on in red.
	my (%marks, %red);
	my $delta = 4;
	foreach my $line_num (@$line_nums) {
	    foreach my $l (($line_num - $delta) .. ($line_num + $delta)) {
		next if ($l <= 0 or $l > @file);
		$marks{$l}++;
	    }
	    $red{$line_num} = 1;
	}

	# Create the context list.
	# By going through the keys of the %marks hash, we can tell which lines need
	# to be printed. We add a '...' line if we skip numbers in the context.
	my $last_num = 0;
	foreach my $line_num (sort { $a <=> $b } keys %marks) {
	    push(@context, ["...", "", 0]) unless $last_num == ($line_num - 1);
	    push(@context, ["$line_num:", $file[$line_num], $red{$line_num}]);;
	    $last_num = $line_num;
	}
	push(@context, ["...", "", 0]) unless $last_num == @file;
	close $fh;
    }
    return @context;
}

sub as_string
{
    my ($self) = @_;

    my $info = $self->analyze_error;
    (my $msg = $info->{msg}) =~ s/\n/\t/g;
    my $stack = join(", ", map { sprintf("[%s:%d]", $_->filename, $_->line) } @{$info->{frames}});
    return sprintf("%s\tStack: %s\n", $msg, $stack);
}

sub as_html
{
    my ($self) = @_;

    my $info = $self->analyze_error;

    my $out;
    my $msg = HTML::Mason::Tools::html_escape($info->{msg});
    $msg =~ s/\n/<br>/g;
    $out .= qq[<html><body>\n];
    $out .= qq[<p align="center"><font face="Verdana, Arial, Helvetica, sans-serif"><b>System error</b></font></p>\n];
    $out .= qq[<table border="0" cellspacing="0" cellpadding="1">\n];
    $out .= qq[<tr>];
    $out .= qq[<td nowrap align="left" valign="top"><b>error:</b>&nbsp;</td>\n];
    $out .= sprintf(qq[<td align="left" valign="top" nowrap>%s</td>\n], $msg);
    $out .= qq[</tr>\n];
    $out .= qq[<tr>\n];
    $out .= qq[<td nowrap align="left" valign="top"><b>context:</b>&nbsp;</td>\n];
    $out .= qq[<td align="left" valign="top" nowrap><table border="0" cellpadding="0" cellspacing="0">\n];
    foreach my $entry (@{$info->{context}}) {
	my ($line_num, $line, $highlight) = @$entry;
	$line = HTML::Mason::Tools::html_escape($line);
	$line =~ s/ /&nbsp;/g;
	$out .= qq[<tr>\n];
	$out .= sprintf(qq[<td nowrap align="left" valign="top"><b>%s</b>&nbsp;</td>\n], $line_num);
	$out .= sprintf(qq[<td align="left" valign="top" nowrap>%s%s%s</td>\n],
			($highlight ? "<font color=red>" : ""),
			$line,
			($highlight ? "</font>" : ""));
	$out .= qq[</tr>\n];
    }
    $out .= qq[</table></td></tr>\n];
    $out .= qq[<tr>\n];
    $out .= qq[<td align="left" valign="top" nowrap><b>code stack:</b>&nbsp;</td>\n];
    $out .= qq[<td align="left" valign="top" nowrap><table border="0" cellpadding="0" cellspacing="0">\n];
    foreach my $frame (@{$info->{frames}}) {
	$out .= sprintf(qq[%s:%d\n<br>], HTML::Mason::Tools::html_escape($frame->filename), HTML::Mason::Tools::html_escape($frame->line));
    }
    $out .= qq[</td>\n];
    $out .= qq[</tr>\n];
    $out =~ s/(<td [^\>]+>)/$1<font face="Verdana, Arial, Helvetica, sans-serif" size="-2">/g;
    $out =~ s/<\/td>/<\/font><\/td>/g;

    return $out;
}

package HTML::Mason::Exception::Syntax;

use base qw(HTML::Mason::Exception);

sub new
{
    my $proto = shift;
    my $class = ref $proto || $proto;
    my %p = @_;

    my $self = $class->SUPER::new(%p);

    $self->{source_line} = $p{source_line};
    $self->{comp_name} = $p{comp_name},
    $self->{line_number} = $p{line_number};

    return $self;
}

sub as_string
{
    my $self = shift;

    # unholy intermixing with parent class because we want the trace
    # to go after the error message but we want to massage the message
    # first.
    local $self->{message} = $self->{message};
    $self->{message} .= " in $self->{comp_name} at $self->{line_number}:\n$self->{source_line}\n";

    return $self->SUPER::as_string;
}

sub source_line { $_[0]->{source_line} }

sub comp_name { $_[0]->{comp_name} }

sub line_number { $_[0]->{line_number} }


1;
