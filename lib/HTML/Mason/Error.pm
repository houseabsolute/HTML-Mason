# Copyright (c) 1998-2001 by Jonathan Swartz. All rights reserved.
# This program is free software; you can redistribute it and/or modify it
# under the same terms as Perl itself.

#
# Error message formatter. Created originally by Matthew Lewinski at
# AvantGo, Inc.
#

package HTML::Mason::Error;

use strict;

require Exporter;

use vars qw(@ISA @EXPORT_OK);

@ISA = qw(Exporter);
@EXPORT_OK = qw(error_process error_display_html);

use HTML::Mason::Tools qw(html_escape make_fh);

sub error_process {
    my ($error, $req) = @_;

    my %conf = (
		'runtime_error' => 'runtime error',
		'component_error' => 'undefined component',
		'compile_error' => 'compilation error'
		);

    my %error_info = ();
    my @callstack = ();
    my @backtrace = ();
    my @errors = ();
    my @misc_info = ();

    # Remove junk underneath HTML::Mason::Request::exec, since this is always the same
    if ((my $i = index($error,'HTML::Mason::Request::exec'))!=-1) {
	$error = substr($error,0,$i);
    }

    # Get backtrace information
    if ($req->{error_backtrace} and @{$req->{error_backtrace}}) {
        @backtrace = map({component=>($_->title)},@{$req->{error_backtrace}});
	$error_info{'component'} = $backtrace[-1];
    }

    my @error = split(/\n/, $error);
    foreach my $line (@error) {
	# Handle the call stack lines.
	if ($line =~ /called at/) {
	    my ($func, $file, $linenum) =
		($line =~ /\t(.*) called at (\S*) line (\d*)/);

	    # Ignore superfluous call stack entries.
	    if (defined $file) {
		next if $file =~ m#/dev/null#;
		next if $file =~ m#HTML/Mason/ApacheHandler#;
	    }

	    if (defined $func) {
		next if $func =~ m#eval|require 0# and defined $file and $file =~ m#HTML/Mason/Request#;
		next if $func =~ m#HTML::Mason::Request::exec#;
		next if $func =~ m#HTML::Mason::Component::run#;
		next if $func =~ m#HTML::Mason::ApacheHandler#;
		next if $func =~ m#FileBased=HASH#;
		next if $func =~ m#__ANON__#;
	    }

	    if (@callstack)   {
		my $last = $callstack[ scalar(@callstack) - 1 ];
		next if ($last && $last->{'line'} eq $linenum && defined $file && $last->{'file'} eq $file);
	    }

	    push @callstack, { "function" => $func || '', "file" => $file, "line" => $linenum };
	}

	# Sometimes the perl warnings span multiple lines. This should do the 
	# right thing by appending the rest of the error to the last message.
	elsif($line =~ /^\t/) {
	    if (@errors) {
		$errors[-1]->{'message'} .= $line;
	    }
	}

	# Handle information about what file Mason was trying to compile.
	# This error is redundant and we do not actually use the information,
	# but we need to handle the case so it doesn't get pushed into the
	# unparsable information structure.
	elsif($line =~ /loading/) {
	    my ($document) =
		($line =~ /loading '(\S*)' at/);
	    $error_info{'err_type'} = $conf{'runtime_error'};
	    $error_info{'err_descr'} = "while loading $document";
	}

	elsif($line =~ /while executing (\S*):/ and
	      (!defined($error_info{'err_descr'}) or $error_info{'err_descr'} !~ /while executing/)) {
	    $error_info{'err_type'} = $conf{'runtime_error'};
	    $error_info{'err_descr'} = "while executing $1";
	}

	# Handle perl's errors and 'die' statements.
	elsif($line =~ /at (\S*) line (\d*)/) {
	    my ($message, $file, $linenum) =
		($line =~ /(.*) at (\S*) line (\d*)/);
	    $message =~ s/,/ /g;  # hack for sake of error_parse
	    push @errors, { "message" => $message, "line" => $linenum };
	    $error_info{'err_file'} = $file;
	}

	# Handle undefined components.
	elsif($line =~ /^could not find component/) {
	    my ($component) = 
		($line =~ /^could not find component for path '(\S*)'/);
	    $error_info{'undef_component'} = $component;
	    $error_info{'err_type'} = $conf{'component_error'};
	}

	# Handle compilation errors.
	# Mason compile errors don't follow the same format as other errors, and the
	# format of the different errors are not necessarily the same, so we have to
	# handle them by checking if the error type is a compilation error.
	elsif($line =~ /during compilation/) {
	    my ($file) =
		($line =~ /compilation of (\S*):/);
	    $error_info{'err_file'} = $file;
	    $error_info{'err_type'} = $conf{'compile_error'};
	    $error_info{'err_descr'} = "during compilation of $file";
	}
	elsif(defined($error_info{'err_type'}) and $error_info{'err_type'} eq $conf{'compile_error'}) {
	    if($line =~ /\(line (\d*)\)/) {
		my ($message, $linenum) = 
		    ($line =~ /(.*) \(line (\d*)\)/);
		push @errors, { "message" => $message, "line" => $linenum };
	    } elsif ($line =~ /\S/) {
		push @errors, { "message" => $line };
	    }
	}

	# Put everything we can't handle into an extra array.
	else {
	    push @misc_info, $line;
	}
    }

    # If we have a component error, it is nice to have a context for the error, so
    # create an error from the call stack information.
    if($error_info{'err_type'} and ($error_info{'err_type'} eq $conf{'component_error'})) {
	my $ref = $callstack[0];
        $error_info{'err_file'} = $ref->{'file'};
        push @errors, { 'message' => qq(Unable to locate component "$error_info{'undef_component'}"), 'line' => $ref->{'line'} };
    }

    if(@errors==1) {
	my $err = $errors[0];
	if (!$callstack[0] or $callstack[0]->{"file"} ne $error_info{'err_file'}) {
	    unshift(@callstack,{"file"=>$error_info{'err_file'},line=>$errors[0]->{"line"}});
	}
    }

    # Save our results.
    $error_info{'callstack'} = [ @callstack ];
    $error_info{'errors'} =    [ @errors ];
    $error_info{'backtrace'} = [ @backtrace ];
    $error_info{'misc_info'} = [ @misc_info ];

    my @lines;

    if (my $descr = $error_info{'err_descr'}) {
	push(@lines,"Mason error ($descr)");
    } else {
	push(@lines,"Mason error");
    }

    if (my $file = $error_info{'err_file'}) {
	push(@lines,"File: $file");
    }

    if (@{$error_info{'errors'}}) {
	my $err = '';
	foreach my $ref (@{$error_info{'errors'}}) {
	    $err .= '[' . ($ref->{'line'} ? $ref->{'line'}.':' : '') . $ref->{'message'} . '], ';
	}
	$err =~ s/, $//;
	$err =~ s/\t//g;
	push(@lines,"Errors: $err");
    }

    if (@{$error_info{'backtrace'}}) {
	my $bt = '';
	foreach my $ref (@{$error_info{'backtrace'}}) {
	    $bt .= $ref->{'component'} . ', ';
	}
	$bt =~ s/, $//;
	$bt =~ s/\t//g;
	push(@lines,"Component stack: $bt");
    }

    if (@{$error_info{'callstack'}}) {
	my $cs = '';
	foreach my $ref (@{$error_info{'callstack'}}) {
	    $cs .= '[' . $ref->{'file'} . (defined($ref->{'line'}) ? ':'.$ref->{'line'} : '') . '], ';
	}
	$cs =~ s/, $//;
	$cs =~ s/\t//g;
	push(@lines,"Code stack: $cs");
    }

    if (@{$error_info{'misc_info'}}) {
	push(@lines,"Misc info: " . $error_info{'misc_info'}->[0]);
    }

    foreach my $line (@lines) {
	$line =~ s/[\t\n]//g;
    }
    my $new_error .= join("\n",@lines)."\n";

    return $new_error;
}

sub error_display_html {
    my ($error,$raw_error) = @_;

    my $out = '';

    my $conf = error_conf();
    my $title = "Mason error";
    my $error_info = error_parse($error);

    $raw_error =~ s/\n/<br>\n/g;
    $error_info->{raw_error} = $raw_error if $raw_error;

    $out .= qq{<html><body>\n<p align="center"><font face="$conf->{'font_face'}"><b>$title</b></font></p>\n};
    $out .= error_table_html($error_info, $conf, $error);
    $out .= qq{\n</body></html>\n};
}

sub error_table_html {
    my ($error_info, $conf, $error) = @_;
    my $out = '';

    my $notes      = error_notes_html     ($error_info,$conf);
    my $context    = error_context_html   ($error_info,$conf);
    my $comp_stack = error_compstack_html ($error_info,$conf);
    my $call_trace = error_calltrace_html ($error_info,$conf);

    $error =~ s/\t/<br>/g;
    my $processed_error_dump = qq{
	<font face="$conf->{font_face}" size="$conf->{font_size}"><b>processed mason error:</b></font><br>
	    <font size="-1"><code>$error</code></font><br>
	    };

    my %show = (
		error_type =>      $error_info->{'type'},
		file =>            $error_info->{'file'},
		notes =>           $notes,
		context =>         $context,
		component_stack => $comp_stack,
		call_trace =>      $call_trace
		);
    $show{misc_info} = $error_info->{misc_info} if $error_info->{misc_info};
    $show{debug_info} = $error_info->{debug_info} if $error_info->{debug_info};
    $show{raw_error} = "<br>" x 30 . "<a name=\"raw_error\">\n" . $error_info->{raw_error} if $error_info->{raw_error};

    $out .= qq{<table border="0" cellspacing="0" cellpadding="1">};
    foreach my $item (@{$conf->{'show'}}) {
	$out .= $conf->{table_entry}->($conf->{'labels'}{$item}, $show{$item}) if $show{$item};
    }
    $out .= qq{</table>};

    # Decided for now to avoid clutter and omit processed mason error
    # by default. We'll see whether there is a need for this.
    if (0) {
	$out .= qq{\n$processed_error_dump\n};
    }

    return $out;
}

sub error_calltrace_html {
    my ($error_info, $conf)= @_;

    # Create the call stack.
    my $call_trace = "";

    $call_trace .= qq(<table border="0" cellpadding="0" cellspacing="0">);
    foreach my $ref (@{$error_info->{'calltrace'}}) {
	$call_trace .= html_escape("$ref->{'file'}:$ref->{'line'}")."<br>";
	# $call_trace .= $conf->{'table_entry'}->("location:", html_escape("$ref->{'file'}:$ref->{'line'}"));

	# We stop when we reach sys_handler.pl because anything higher on the stack
	# is called regardless of what component we are in.
	last if $ref->{'file'} =~ /sys_handler.pl$/;
    }
    $call_trace .= qq(</table><br>);

    return $call_trace;
}

sub error_compstack_html {
    my ($error_info, $conf) = @_;

    my $comp_stack = "";

    foreach my $ref (@{$error_info->{'backtrace'}}) {
	$comp_stack .= html_escape($ref->{'component'})."<br>";
    }

    return $comp_stack;
}

sub error_conf {
    my $conf = {
		font_face        => "Verdana, Arial, Helvetica, sans-serif",
		font_size        => "-2",
		title_file       => "mason_errors.lst",

		# allowed values:
		# type component file notes context component_stack call_trace
		labels           => {
		    type            => "error type: ",
		    component       => "component: ",
		    file            => "error in file: ",
		    notes           => "",
		    context         => "context: ",
		    component_stack => "component stack: ",
		    call_trace      => "code stack: ",
		    debug_info      => "debug info: ",
		    raw_error       => "<br><a href=\"#raw_error\">raw_error</a>" . "<br>" x 30 . "raw error: ",
		},

		show             => [ "file",
				      "notes",
				      "context",
				      "component_stack",
				      "call_trace",
				      "debug_info",
				      "raw_error",
				    ],

		};

    $conf->{table_entry} = sub {
	my($t, $d) = @_;
	qq(<tr>\n\t<td nowrap align="left" valign="top"><font face="$conf->{'font_face'}" size="$conf->{'font_size'}"><b>$t</b>&nbsp;</font></td>\n\t<td align="left" valign="top"><font face="$conf->{'font_face'}" size="$conf->{'font_size'}">$d</font></td>\n</tr>\n);
    };

    return $conf;
}

#
# Create html context for $file and $line_nums.
#
sub create_context_html {
    my ($file,$line_nums,$conf) = @_;
    return '' unless $file;

    my $context .= qq(<table border="0" cellpadding="0" cellspacing="0">);

    my $fh = make_fh();
    open $fh, $file;
    unless($fh) {
	$context = "unable to open file";
    } else {
	# Put the file into a list, indexed at 1.
	my @file = map(html_escape($_),<$fh>);
	for (@file) { s/ /&nbsp;/g }
	chomp(@file);
	unshift(@file,undef);

	# Mark the important context lines.
	# We do this by going through the error lines and incrementing hash keys to
	# keep track of which lines we eventually need to print, and we color the
	# line which the error actually occured on in red.
	my (%marks,%red);
	my $delta = 4;
	foreach my $line_num (@$line_nums) {
	    foreach my $l (($line_num - $delta) .. ($line_num + $delta)) {
		$marks{$l}++;
	    }

	    unless ($red{$line_num}) {
		$red{$line_num}++;
		$file[$line_num] = qq(<font color="red">$file[$line_num]</font>);
	    }
	}

	# Create the context table.
	# By going through the keys of the %marks hash, we can tell which lines need
	# to be printed. We add a '...' line if we skip numbers in the context.
	my $last_num = 0;
	foreach (sort { $a <=> $b } keys %marks) {
	    next if($_ <= 0);
	    last if($_ > @file);
	    $context .= $conf->{table_entry}->("...", "") if($last_num != ($_ - 1));
	    $context .= $conf->{table_entry}->("$_:", $file[$_]);
	    $last_num = $_;
	}
	$context .= $conf->{table_entry}->("...", "");
	$context .= qq(</table>);
    }
    close $fh or HTML::Mason::Exception->throw( error => "can't close file: $file: $!" );

    return $context;
}

sub error_context_html {
    my ($error_info,$conf) = @_;

    my @line_nums;
    foreach my $ref (@{$error_info->{'errors'}}) {
	push(@line_nums,$ref->{'line'}) if $ref->{'line'};
    }
    return create_context_html($error_info->{'file'},\@line_nums,$conf);
}

sub error_notes_html {
    my ($error_info, $conf) = @_;

    my $notes = "";

    # Sort the errors by line number.
    my %notes;
    my $general_errors;
    foreach my $ref (@{$error_info->{'errors'}}) {
	if($ref->{'line'}) {
	    $notes{$ref->{'line'}} .= html_escape($ref->{'message'})."\n";
	} else {
	    $general_errors .= html_escape($ref->{'message'})."\n";
	}
    }

    # Assemble the table.
    $notes .= qq(<table border="0" cellpadding="0" cellspacing="0">);

    my @notes;
    if ($general_errors) {
	@notes = split("\n", $general_errors);
	foreach my $note (@notes) {
	    $notes .= $conf->{table_entry}->("general:", $note);
	}
    }

    foreach my $linenum (sort { $a <=> $b } keys %notes) {
	@notes = split("\n", $notes{$linenum});
	foreach my $note (@notes) {
	    $notes .= $conf->{table_entry}->("line $linenum:", $note);
	}
    }
    $notes .= qq(</table>);

    return $notes;
}

sub error_parse {
    my ($error) = @_;
    my $error_info = {};

    my @errors = ();

    my @error = split(/\n/, $error);
    foreach my $line (@error) {
	if($line =~ /^Mason error/) {
	    ($error_info->{'type'}) = ($line =~ /^Mason Error \((.*)\)/);

	} elsif($line =~ /^File:/) {
	    ($error_info->{'file'}) = ($line =~ /^File: (\S+)/);

	} elsif($line =~ /^Errors:/) {
	    my ($errors) = ($line =~ /^Errors: (.*)/);
	    my @entries = split(/, /, $errors);
	    foreach my $entry (@entries) {
		$entry =~ tr/[]//d;
		my ($line, $message) = split(/:/, $entry, 2);
		push @{$error_info->{'errors'}}, { line => $line, message => $message };
	    }

	} elsif($line =~ /^Component stack:/) {
	    my ($backtrace) = ($line =~ /^Component stack: (.*)/);
	    my @entries = split(/, /, $backtrace);
	    foreach my $entry (@entries) {
		my ($project, $component);
		if ($entry =~ /:/) {
		    ($project, $component) = ($entry =~ /\[(\S+):(\S+)\]/);
		} else {
		    ($project, $component) = (undef,$entry);
		}
		push @{$error_info->{'backtrace'}}, { project => $project, component => $component };
	    }

	} elsif($line =~ /^Code stack:/) {
	    my ($calltrace) = ($line =~ /^Code stack: (.*)/);
	    my @entries = split(/, /, $calltrace);
	    foreach my $entry (@entries) {
		my ($file, $line) = ($entry =~ /\[(\S+):(\d+)\]/);
		push @{$error_info->{'calltrace'}}, { file => $file, line => $line };
	    }
	} elsif($line =~ /^Debug info:/) {
	    my ($debug_info) = ($line =~ /^Debug info: (.*)/);
	    $error_info->{'debug_info'} = $debug_info;
	}
    }

    return $error_info;
}

1;
