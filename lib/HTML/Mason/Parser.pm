# Copyright (c) 1998 by Jonathan Swartz. All rights reserved.
# This program is free software; you can redistribute it and/or modify it
# under the same terms as Perl itself.

package HTML::Mason::Parser;
require 5.004;
require Exporter;
@ISA = qw(Exporter);
@EXPORT = qw();
@EXPORT_OK = qw();

use strict;
use Data::Dumper;
use File::Path;
use File::Basename;
use File::Find;
use HTML::Mason::Component;
use HTML::Mason::Request;
use HTML::Mason::Tools qw(read_file);
use vars qw($AUTOLOAD);

my %fields =
    (preamble => '',
     postamble => '',
     preprocess => undef,
     postprocess => undef,
     use_strict => 1,
     source_refer_predicate => sub { return ($_[1] >= 5000) },
     ignore_warnings_expr => 'Subroutine .* redefined',
     taint_check => 0,
     in_package => 'HTML::Mason::Commands',
     allow_globals => []
     );
# Minor speedup: create anon. subs to reduce AUTOLOAD calls
foreach my $f (keys %fields) {
    next if $f =~ /^allow_globals$/;  # don't overwrite real sub.
    no strict 'refs';
    *{$f} = sub {my $s=shift; return @_ ? ($s->{$f}=shift) : $s->{$f}};
}

#
# This version number, less than or equal to the Mason version, marks the
# points at which the Parser produces incompatible object files.
#
sub version
{
    return 0.7;
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
    my ($objectTextRef) = @options{qw(object_text)};

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
    my ($script,$scriptFile,$errorRef,$errposRef,$embedded,$fileBased) =
	@options{qw(script script_file error errpos embedded file_based)};
    my ($sub, $err, $errpos, $suberr, $suberrpos);
    $fileBased = 1 if !exists($options{file_based});
    my $pureTextFlag = 1;
    my $parseError = 1;
    my $parserVersion = version();

    #
    # If script_file option used, read script from file.
    #
    if (!defined($script)) {
	die "parse: must specify script or script_file\n" if (!defined($scriptFile));
	$script = read_file($scriptFile);
    }

    #
    # Eliminate DOS ctrl-M chars
    #
    $script =~ s/\cM//g;
    
    #
    # Preprocess the script.  The preprocessor routine is handed a
    # reference to the entire script.
    #
    if ($self->{preprocess}) {
        eval {$self->{preprocess}->(\$script)};
        if ($@) {
            $err = "error during custom preprocess step:\n$@";
            goto parse_error;
        }
    }
    
    #
    # Extract special sections. For backward compatibility, allow
    # names to be prefixed with perl_.  Record all other text ranges
    # in @textsegs.
    #
    # $startline keeps track of whether the next text range starts
    # at the beginning of a line. This becomes important later,
    # when looking for %-lines.
    #
    my %sectiontext = (map(($_,''),qw(args cleanup doc filter init once)));
    my $curpos = 0;
    my $startline = 1;
    my (@textsegs,%subcomps);
    my $scriptlength = length($script);
    while ($script =~ /(<%(?:perl_)?(args|cleanup|def[ \t]+([^>\n]+)|doc|filter|init|once|text)>)/ig) {
	my ($begintag,$beginfield,$beginarg) = ($1,lc($2),$3);
	$beginfield = 'def' if (substr($beginfield,0,3) eq 'def');
	my $beginmark = pos($script)-length($begintag);
	my $begintail = pos($script);
	push(@textsegs,{start=>$curpos,len=>$beginmark-$curpos,startline=>$startline}) if $curpos < $beginmark;
	if ($script =~ m/(<\/%(?:perl_)?$beginfield>\n?)/ig) {
	    my $endtag = $1;
	    my $endmark = pos($script)-length($endtag);
	    if ($beginfield eq 'text') {
		# Special case for <%text> sections: add a special
		# segment that won't get parsed
		push(@textsegs,{start=>$begintail,len=>$endmark-$begintail,startline=>0,noparse=>1});
	    } elsif ($beginfield eq 'def') {
		# Special case for <%def> sections: compile section as
		# component and put object text in subcomps hash,
		# keyed on def name
		my $name = $beginarg;
		if ($name !~ /^[\w\-\.]+$/) {
		    $err = "invalid subcomponent name '$name': valid characters are [A-Za-z0-9._-]";
		    $errpos = $begintail;
		} elsif (exists($subcomps{$name})) {
		    $err = "multiple definitions for subcomponent '$name'";
		    $errpos = $begintail;
		} else {
		    my $subtext = substr($script,$begintail,$endmark-$begintail);
		    if (my $objtext = $self->parse_component(script=>$subtext, embedded=>1, file_based=>0, error=>\$suberr, errpos=>\$suberrpos)) {
			$subcomps{$name} = $objtext;
		    } else {
			$err = "Error while parsing subcomponent '$name':\n$suberr";
			$errpos = $begintail+$suberrpos;
			$suberr =~ s/(line .*)\n$/($name line .*)\n/;
		    }
		}
		goto parse_error if ($err);
	    } else {
		$sectiontext{lc($beginfield)} .= substr($script,$begintail,$endmark-$begintail);
	    }
	    $curpos = pos($script);
	    $pureTextFlag = 0;
	    $startline = (substr($endtag,-1,1) eq "\n");
	} else {
	    $err = "<%$beginfield> with no matching </%$beginfield>";
	    $errpos = $beginmark;
	    goto parse_error;
	}
    }
    push(@textsegs,{start=>$curpos,len=>$scriptlength-$curpos,startline=>$startline}) if $curpos < $scriptlength;

    #
    # Start body of subroutine with user preamble and args declare.
    #
    my $body = $self->preamble();
    $body .= 'my (%ARGS) = @_;'."\n";
    $body .= 'my $_out = $REQ->sink;'."\n";

    #
    # Process args section.
    #
    my %declaredArgs = ();
    if ($sectiontext{args}) {
	my (@vars);
	my @decls = split("\n",$sectiontext{args});
	@decls = grep(/\S/,@decls);
	foreach my $decl (@decls) {
	    my ($var,$default);
	    my $split = index($decl,'=>');
            if ($split !=-1) {
		$var = substr($decl,0,$split);
		$default = substr($decl,$split+2);
	    } else {
		($var) = ($decl =~ /^\s*(\S+)/);
	    }
	    for ($var) { s/^\s+//; s/\s+$// }
	    # %ARGS is automatic, so ignore explicit declaration.
	    next if ($var eq '%ARGS');
	    push (@vars,$var);
	    my $type = substr($var,0,1);
	    my $name = substr($var,1);
	    $declaredArgs{$var} = {default=>$default};

	    my $defaultVal = defined($default) ? $default : 
		"die \"no value sent for required parameter '$name'\"";
	    $defaultVal .= "\n" if (defined($default) && $default =~ /\#/);   # allow comments
	    
	    # Scalar
	    if ($type eq "\$") {
		$body .= "my $var = (!exists \$ARGS{'$name'} ? $defaultVal : \$ARGS{'$name'});";
	    }
		
	    # Array
	    elsif ($type eq "\@") {
		$body .= "my $var = (!exists \$ARGS{'$name'} ? $defaultVal : ";
		$body .= "ref(\$ARGS{'$name'}) eq 'ARRAY' ? \@{\$ARGS{'$name'}} : (\$ARGS{'$name'}));";
	    }
	    
	    # Hash
	    elsif ($type eq "\%") {
		$body .= "my $var = (!exists \$ARGS{'$name'} ? $defaultVal : ";
		$body .= "ref \$ARGS{'$name'} eq 'ARRAY' ? \@{\$ARGS{'$name'}} : ";
		$body .= "ref \$ARGS{'$name'} eq 'HASH' ? \%{\$ARGS{'$name'}} : ";
		$body .= "die \"single value sent for hash parameter '$var'\");";
	    }

	    # None of the above
	    else {
		$err = "unknown type for argument '$var': first character must be \$, \@, or \%";
		goto parse_error;
	    }

	    $body .= "\n";
	}
    }

    #
    # Parse in-line insertions, which take one of four forms:
    #   - Lines beginning with %
    #   - Text delimited by <%perl> </%perl>
    #   - Text delimited by <% %> 
    #   - Text delimited by <& &>
    # All else is a string to be delimited by single quotes and output.
    #
    # $startline keeps track of whether the first character is the
    # start of a line.
    # $curpos keeps track of where we are in $text.
    # @alphasecs and @perltexts represent alternating plain text
    # and perl sections.
    #
    my $alphalength=0;
    my (@alphasecs, @perltexts);

    foreach my $textseg (@textsegs) {
	my ($segbegin, $textlength);
	($segbegin,$textlength,$startline) =
	    ($textseg->{start},$textseg->{len},$textseg->{startline});
	
	# Special case for <%text> sections
	if ($textseg->{noparse}) {
	    push(@alphasecs,[$segbegin,$textlength]);
	    push(@perltexts,'');
	    next;
	}
	my $text = substr($script,$segbegin,$textlength);
	my $curpos = 0;
	while ($curpos < $textlength) {
	    my ($alpha, $perl);
	    if ($startline && substr($text,$curpos,1) eq '%') {
		# Immediate line beginning with %
		my $endline = index($text,"\n",$curpos);
		$endline = $textlength if ($endline==-1);
		my $length = $endline-$curpos;
		push(@perltexts,substr($text,$curpos+1,$length));
		push(@alphasecs,[0,0]);
		$curpos += $length+1;
		$pureTextFlag = 0;
		next;
	    }
	    $startline=0;
	    my $a = index($text,"\n%",$curpos);
	    my $b = index($text,"<%",$curpos);
	    my $c = index($text,"<&",$curpos);
	    if ($a>-1 && ($b==-1 || $a<$b) && ($c==-1 || $a<$c)) {
		#
		# Line beginning with %
		#
		my $beginline = $a+1;
		$alpha = [$curpos,$beginline-$curpos];
		my $endline = index($text,"\n",$beginline);
		$endline = $textlength if ($endline==-1);
		my $length = $endline-$beginline;
		$perl = substr($text,$beginline+1,$length-1);
		$curpos = $endline+1;
		$startline = 1;
		$pureTextFlag = 0;
	    } elsif ($b>-1 && ($c==-1 || $b<$c)) {
		#
		# Tag beginning with <%
		#
		if (lc(substr($text,$b,7)) eq '<%perl>') {
		    #
		    # <%perl> section
		    #
		    $alpha = [$curpos,$b-$curpos];
		    if (!($text =~ m{</%perl>}ig)) {
			$err = "<%PERL> with no matching </%PERL>";
			$errpos = $segbegin + $b;
			goto parse_error;
		    }
		    my $i = pos($text)-8;
		    my $length = $i-($b+7);
		    $perl = substr($text,$b+7,$length);
		    $curpos = $b+7+$length+8;
		} else {
		    #
		    # <% %> section
		    #
		    $alpha = [$curpos,$b-$curpos];
		    # See if this is a mistaken <%xxx> command
		    if (substr($text,$b+2,20) =~ /^(\w+)>/) {
			$err = "unknown section <%$1>";
			$errpos = $segbegin + $b;
			goto parse_error;
		    }
		    my $i = index($text,"%>",$b+2);
		    if ($i==-1) {
			$err = "'<%' with no matching '%>'";
			$errpos = $segbegin + $b;
			goto parse_error;
		    }
		    my $length = $i-($b+2);
		    $perl = '$_out->('.substr($text,$b+2,$length).');';
		    $curpos = $b+2+$length+2;
		}
		$pureTextFlag = 0;
	    } elsif ($c>-1) {
		#
		# <& &> section
		#
		$alpha = [$curpos,$c-$curpos];
		my $i = index($text,"&>",$c+2);
		if ($i==-1) {
		    $err = "'<&' with no matching '&>'";
		    $errpos = $segbegin + $c;
		    goto parse_error;
		}
		my $length = $i-($c+2);
		my $call = substr($text,$c+2,$length);
		for ($call) { s/^\s+//; s/\s+$// }
		if (substr($call,0,1) =~ /[A-Za-z0-9\/_.]/) {
		    # Literal component path; put quotes around it
		    my $comma = index($call,',');
		    $comma = length($call) if ($comma==-1);
		    (my $comp = substr($call,0,$comma)) =~ s/\s+$//;
		    $call = "'$comp'".substr($call,$comma);
		}
		$perl = "\$REQ->call($call);";
		$curpos = $c+2+$length+2;
		$pureTextFlag = 0;
	    } else {
		# No more special characters, take the rest.
		$alpha = [$curpos,$textlength-$curpos];
		$perl = '';
		$curpos = $textlength;
	    }

	    #
	    # Escape newline preceded by backslash
	    #
	    if ($alpha->[1] >= 2) {
		my $c = $alpha->[0]+$alpha->[1]-2;
		if (substr($text,$c,2) eq "\\\n") {
		    $alpha->[1] -= 2;
		}
	    }

	    push(@alphasecs,[$segbegin+$alpha->[0],$alpha->[1]]);
	    $alphalength += $alpha->[1];
	    push(@perltexts,$perl);
	}
    }

    #
    # Use source_refer_predicate to determine whether to use source
    # references or directly embedded text.
    #
    my $useSourceReference = $fileBased && ($pureTextFlag || $self->source_refer_predicate->($scriptlength,$alphalength));
    my @alphatexts;
    my $endsec = '';
    if ($useSourceReference) {
	$body .= 'my $_srctext = $REQ->comp->source_ref_text;'."\n";
	my $cur = 0;
	foreach my $sec (@alphasecs) {
	    $endsec .= substr($script,$sec->[0],$sec->[1])."\n";
	    push(@alphatexts,sprintf('$_out->(substr($_srctext,%d,%d));',$cur,$sec->[1]));
	    $cur += $sec->[1] + 1;
	}
	$endsec =~ s/\n$//;
    } else {
	foreach (@alphasecs) {
	    my $alpha = substr($script,$_->[0],$_->[1]);
	    $alpha =~ s{([\\\'])} {\\$1}g;        # escape backslashes and single quotes
	    push(@alphatexts,sprintf('$_out->(\'%s\');',$alpha));
	}
    }

    #
    # Insert call to debug hook.
    #
    $body .= '$REQ->debug_hook($REQ->comp->path) if (%DB::);'."\n";
    
    #
    # Insert <%filter> section.
    #
    if ($sectiontext{filter}) {
	my $ftext = $sectiontext{filter};
	for ($ftext) { s/^\s+//g; s/\s+$//g }
	$body .= sprintf(join("\n",'{ my ($_c,$_r);','if (mc_call_self(\$_c,\$_r)) {'.'for ($_c) {',$ftext,'}','mc_out($_c);','return $_r }};'));
    }

    #
    # Insert <%init> section.
    #
    $body .= $sectiontext{init}."\n";

    #
    # Call start_primary hooks.
    #
    $body .= "\$REQ->call_hooks('start_primary');\n";
    
    #
    # Postprocess the alphabetical and Perl stuff separately
    #
    if ($self->{postprocess})
    {
        foreach my $a (@alphatexts)
        {
            next unless $a;
            $self->{postprocess}->(\$a, 'alpha');
        }
        foreach my $p (@perltexts)
        {
            next unless $p;
            $self->{postprocess}->(\$p, 'perl');
        }
    }

    #
    # Append text and perl sections to body.
    #
    for (my $i=0; $i<scalar(@alphatexts); $i++) {
	if ($alphasecs[$i]->[1] > 0) {
	    $body .= $alphatexts[$i]."\n";
	}
	$body .= $perltexts[$i]."\n";
    }

    #
    # Call end_primary hooks.
    #
    $body .= "\$REQ->call_hooks('end_primary');\n";
    
    #
    # Insert <%cleanup> section.
    #
    $body .= $sectiontext{cleanup}."\n";

    #
    # Insert user postamble and return undef by default.
    #
    $body .= $self->postamble();
    $body .= "return undef;\n";

    #
    # Wrap body in subroutine and add header, including <%once> section.
    #
    my $header = "";
    my $pkg = $self->{in_package};
    if (!$embedded) {
	$header .= "package $pkg;\n";
	$header .= "use strict;\n" if $self->use_strict;
	$header .= sprintf("use vars qw(%s);\n",join(" ","\$REQ",@{$self->{'allow_globals'}}))."\n";
    }
    $header .= $sectiontext{once}."\n" if $sectiontext{once};

    #
    # Add subcomponent (<%def>) sections.
    #
    if (%subcomps) {
	$header .= "my \%_subcomps = (\n";
	$header .= join(",\n",map("'$_' => do {\n$subcomps{$_}\n}",sort(keys(%subcomps))));
	$header .= "\n);\n";
    }

    #
    # Assemble parameters for component.
    #
    my @cparams = ("parser_version=>$parserVersion","create_time=>".time());
    push(@cparams,"source_ref_start=>0000000") if $useSourceReference;
    push(@cparams,"subcomps=>{%_subcomps}") if (%subcomps);
    if (%declaredArgs) {
	my $d = new Data::Dumper ([\%declaredArgs]);
	my $argsDump = $d->Dumpxs;
	for ($argsDump) { s/\$VAR1\s*=//g; s/;\s*$// }
	push(@cparams,"declared_args=>$argsDump");
    }
    if ($pureTextFlag) {
	push(@cparams,"code=>\\&HTML::Mason::Commands::pure_text_handler");
    } else {
	push(@cparams,"code=>sub {\n$body\n}");
    }
    my $cparamstr = join(",\n",@cparams);
    
    $body = "new HTML::Mason::Component (\n$cparamstr\n);\n";
    $body = $header . $body;

    #
    # If using source references, add the source text after the
    # __END__ tag, then calculate its position for
    # source_ref_start. This must occur after the component body
    # is fixed!
    #
    if ($useSourceReference) {
	$body .= "\n__END__\n";
	my $srcstart = sprintf("%7d",length($body));
	$body =~ s/source_ref_start=>0000000,/source_ref_start=>$srcstart,/;
	$body .= $endsec;
    }    
    
    #
    # Process parsing errors.
    #
    $parseError = 0;
    parse_error:
    my $success = 1;
    if ($err) {
	if (defined($errpos)) {
	    my $linenum = (substr($script,0,$errpos) =~ tr/\n//) + 1;
	    if ($suberr) {
		$err .= " (main script line $linenum)";
	    } else {
		$err .= " (line $linenum)";
	    }
	    $$errposRef = $errpos if defined($errposRef);
	}
	$err .= "\n";
	$success = 0;
	$$errorRef = $err if defined($errorRef);
    }
    
    return ($success) ? $body : undef;
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
	$comp->object_file($objectFile);
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

sub AUTOLOAD {
    my $self = shift;
    my $type = ref($self) or die "autoload error: bad function $AUTOLOAD";

    my $name = $AUTOLOAD;
    $name =~ s/.*://;   # strip fully-qualified portion
    return if $name eq 'DESTROY';

    die "No such function `$name' in class $type";
}
1;

__END__
