# Copyright (c) 1998 by Jonathan Swartz. All rights reserved.
# This program is free software; you can redistribute it and/or modify it
# under the same terms as Perl itself.

package HTML::Mason::Parser;
require 5.004;
require Exporter;
@ISA = qw(Exporter);
@EXPORT = qw();
@EXPORT_OK = qw(new parse);

use strict;
use File::Path;
use File::Basename;
use File::Find;
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

sub parse
{
    my ($self, %options) = @_;
    my $script = $options{script};
    my $scriptFile = $options{script_file};
    my $resultCodeRef = $options{result_code};
    my $resultTextRef = $options{result_text};
    my $pureTextFlagRef = $options{pure_text_flag};
    my $errorRef = $options{error};
    my $wrapErrors = $options{wrap_errors};
    my $saveTo = $options{save_to};
    my ($sub, $err, $errpos);
    my $pureTextFlag = ($self->{preprocess} || $self->{postprocess}) ? 0 : 1;
    my $parseError = 1;

    #
    # If script_file option used, read script from file.
    #
    if (!defined($script)) {
	die "parse: must specify script or script_file\n" if (!defined($scriptFile));
	$script = read_file($scriptFile);
    }

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
    my %sectiontext = (map(($_,''),qw(args cleanup doc init once)));
    my $curpos = 0;
    my @textsegs;
    my $scriptlength = length($script);
    while ($script =~ /(<%(?:perl_)?(args|cleanup|doc|init|once|text)>)/ig) {
	my ($begintag,$beginfield) = ($1,lc($2));
	my $beginmark = pos($script)-length($begintag);
	my $begintail = pos($script);
	push(@textsegs,[$curpos,$beginmark-$curpos]);
	if ($script =~ m/(<\/%(?:perl_)?$beginfield>\n?)/ig) {
	    my $endtag = $1;
	    my $endmark = pos($script)-length($endtag);
	    if ($beginfield eq 'text') {
		# Special case for <%text> sections: add a special
		# segment that won't get parsed
		push(@textsegs,[$begintail,$endmark-$begintail,'<%text>']);
	    } else {
		$sectiontext{lc($beginfield)} .= substr($script,$begintail,$endmark-$begintail);
	    }
	    $curpos = pos($script);
	    $pureTextFlag = 0;
	} else {
	    $err = "<%$beginfield> with no matching </%$beginfield>";
	    $errpos = $beginmark;
	    goto parse_error;
	}
    }
    push(@textsegs,[$curpos,$scriptlength-$curpos]) if $curpos < $scriptlength;

    #
    # Start body of subroutine with user preamble and args declare.
    #
    my $body = $self->preamble();
    $body .= 'my (%_args) = @_;'."\n".'my %ARGS = %_args;'."\n";
    $body .= 'my $_out = $INTERP->locals->{sink};'."\n";

    #
    # Process args section.
    #
    if ($sectiontext{args}) {
	my (@vars);
	my @decls = split("\n",$sectiontext{args});
	@decls = grep(/\S/,@decls);
	my $argsec = "\nmy (\$val);\n";
	foreach my $decl (@decls) {
	    my ($var,$default,$defaultClause);
	    my $split = index($decl,'=>');
	    if ($split !=-1) {
		$var = substr($decl,0,$split);
		$default = substr($decl,$split+2);
	    } else {
		$var = $decl;
	    }
	    $var =~ s/\s//g;
	    # %ARGS is automatic, so ignore explicit declaration.
	    next if ($var eq '%ARGS');
	    push (@vars,$var);
	    my $type = substr($var,0,1);
	    if ($type !~ /[\$\%\@]/) {
		$err = "unknown type for argument '$var': first character must be \$, \@, or \%";
		goto parse_error;
	    }
	    my $name = substr($var,1);

	    if (defined($default)) {
		$defaultClause = "$var = $default";
	    } else {
		$defaultClause = "die \"no value sent for required parameter '$name'\"";
	    }
	    
	    # Scalar
	    if ($type eq "\$") {
		my $tmpl = '$val = $_args{\'%s\'}; if (!exists($_args{\'%s\'})) { %s; } else { %s; }';
		$argsec .= sprintf($tmpl,$name,$name,$defaultClause,
				   "$var = \$val");
	    }
	    # Array
	    if ($type eq "\@") {
		my $tmpl = '$val = $_args{\'%s\'}; if (!exists($_args{\'%s\'})) { %s; } elsif (ref($val) eq \'ARRAY\') { %s; } else { %s; }';
		$argsec .= sprintf($tmpl,$name,$name,$defaultClause,
				   "$var = \@\$val",
				   "$var = (\$val)");
	    }
	    # Hash
	    if ($type eq "\%") {
		my $tmpl = '$val = $_args{\'%s\'}; if (!exists($_args{\'%s\'})) { %s; } elsif (ref($val) eq \'ARRAY\') { %s; } elsif (ref($val) eq \'HASH\') { %s } else { %s; }';
		$argsec .= sprintf($tmpl,$name,$name,$defaultClause,
				   "$var = \@\$val",
				   "$var = \%\$val",
				   "die \"single value sent for hash parameter '\%$name'\"");
	    }
	    $argsec .= "\n";
	}

	#
	# Declare the args as lexically scoped locals.
	#
	if (@vars) {
	    $body .= "my (".join(",",@vars).");\n{".$argsec."}\n";
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
    my $startline = 1;
    my (@alphasecs, @perltexts);

    foreach my $textseg (@textsegs) {
	# Special case for <%text> sections
	if (@{$textseg}>2 && $textseg->[2] eq '<%text>') {
	    push(@alphasecs,[$textseg->[0],$textseg->[1]]);
	    push(@perltexts,'');
	    next;
	}
	my $segbegin = $textseg->[0];
	my $textlength = $textseg->[1];
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
		if (substr($text,$b,7) eq '<%perl>') {
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
		(my $call = substr($text,$c+2,$length)) =~ s/^\s+//;
		my ($comp,$arglist);
		if ((my $quotemark = (substr($call,0,1))) =~ /[\'\"]/) {
		    # component path delimited by quotes
		    my $j = index($call,$quotemark,1);
		    if ($j==-1) {
			$err = "<& &> section starts with unmatched quote ($quotemark)";
			$errpos = $segbegin + $c;
			goto parse_error;
		    }
		    $comp = substr($call,1,$j-1);
		    $arglist = substr($call,$j+1);
		    $arglist =~ s/^\s*,//;
		} else {
		    # no quotes
		    ($comp,$arglist) = split(',',$call,2);
		    $comp =~ s/\s+$//;
		    if ($comp =~ /\s/) {
			$err = "comp path ($comp) cannot contain whitespace; did you forget a comma?";
			$errpos = $segbegin + $c;
			goto parse_error;
		    }
		}
		$perl = (defined($arglist) && $arglist =~ /\S/) ? "mc_comp('$comp',$arglist);" : "mc_comp('$comp');";
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

	    $alpha->[0] += $segbegin;
	    push(@alphasecs,$alpha);
	    push(@perltexts,$perl);
	}
    }

    #
    # Determine length of plain text.
    #
    my $alphalength=0;
    foreach (@alphasecs) {$alphalength += $_->[1]}

    #
    # Use source_refer_predicate to determine whether to use source
    # references or directly embedded text.
    #
    my $useSourceReference = $self->source_refer_predicate->($scriptlength,$alphalength) && !$self->{preprocess} && !$self->{postprocess};
    my @alphatexts;
    if ($useSourceReference) {
	$body .= 'my $_srctext = mc_file($INTERP->locals->{sourceFile});'."\n";
	@alphatexts = map(sprintf('$_out->(substr($_srctext,%d,%d));',$_->[0],$_->[1]),@alphasecs);
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
    $body .= '$INTERP->debug_hook($INTERP->locals->{truePath}) if (%DB::);'."\n";
    
    #
    # Insert <%init> section.
    #
    $body .= $sectiontext{init}."\n";

    #
    # Call start_primary hooks.
    #
    $body .= "\$INTERP->call_hooks('start_primary');\n";
    
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
    $body .= "\$INTERP->call_hooks('end_primary');\n";
    
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
    $header .= "package $pkg;\n";
    $header .= "use strict;\n" if $self->use_strict;
    $header .= sprintf("use vars qw(%s);\n",join(" ","\$INTERP",@{$self->{'allow_globals'}}));
    $header .= "\n".$sectiontext{once}."\n" if $sectiontext{once};
    $body = "$header\nsub {\n$body\n};\n";

    #
    # Eliminate DOS ctrl-M chars
    #
    $body =~ s/\cM//g;
    
    #
    # Check for errors and warnings.
    #
    $self->evaluate(script=>$body, code=>\$sub, error=>\$err);
    $parseError = 0;

    parse_error:
    my $success = 1;
    if ($err) {
	if (defined($errpos)) {
	    my $linenum = (substr($script,0,$errpos) =~ tr/\n//) + 1;
	    $err .= " (line $linenum)";
	}
	if ($wrapErrors) {
	    $err =~ s/\'/\\\'/g;
	    $err =~ s/\(eval [0-9]\) //g;
	    my $msg = sprintf("Error during compilation%s:",$scriptFile ? " of '$scriptFile'" : "");
	    my $errscript = "$header\nsub {\ndie \"$msg\\n\".'$err'.\"\\n\"\n}\n";
	    $sub = eval($errscript);
	} else {
	    $err .= "\n";
	    $success = 0;
	    $$errorRef = $err if defined($errorRef);
	}
    }
    
    if (!$err && defined($saveTo)) {
	File::Path::mkpath(File::Basename::dirname($saveTo));
	my $fh = new IO::File ">$saveTo" or die "Couldn't write object file $saveTo";
	print $fh $body if (!$pureTextFlag);
	$fh->close;
    }
    $$resultTextRef = $body if !$parseError && defined($resultTextRef);
    $$resultCodeRef = $sub if $success && defined($resultCodeRef);
    $$pureTextFlagRef = $pureTextFlag if defined($pureTextFlagRef);
    return $success;
}

sub evaluate
{
    my ($self, %options) = @_;
    my ($err, $sub);
    my $ignoreExpr = $self->ignore_warnings_expr;
    {
	my $warnstr;
	local $^W = 1;
	local $SIG{__WARN__} = $ignoreExpr ? sub { $warnstr .= $_[0] if $_[0] !~ /$ignoreExpr/ } : sub { $warnstr .= $_[0] };
	if ($options{script}) {
	    my $script = $options{script};
	    ($script) = ($script =~ /^(.*)$/s) if $self->taint_check;
	    $sub = eval($script);
	} elsif ($options{script_file}) {
	    my $file = $options{script_file};
	    ($file) = ($file =~ /^(.*)$/s) if $self->taint_check;
	    $sub = do($file);
	} else {
	    die "evaluate: must specify script or script_file";
	}
	$err = $warnstr . $@;
	# attempt to stem very long eval errors
	if ($err =~ /has too many errors\./) {
	    $err =~ s/has too many errors\..*/has too many errors./s;
	}
    }
    if ($err) {
	my $ref = $options{error};
	$$ref = $err if $ref;
	return 0;
    } else {
	my $ref = $options{code};
	$$ref = $sub if $ref;
	return 1;
    }
}

sub make {
    my ($self, %options) = @_;
    my $sourceDir = $options{source_dir} or die "make: must specify source_dir\n";
    my $objectDir = $options{object_dir} or die "make: must specify object_dir\n";
    my $errorDir = $options{error_dir} or die "make: must specify error_dir\n";
    die "make: object_dir must be different from source_dir\n" if $sourceDir eq $objectDir;
    die "make: source_dir '$sourceDir' does not exist\n" if (!-d $sourceDir);
    die "make: object_dir '$objectDir' does not exist\n" if (!-d $objectDir);
    my @paths = (exists($options{paths})) ? @{$options{paths}} : ('/');
    my $verbose = (exists($options{verbose})) ? $options{verbose} : 1;
    my $predicate = $options{predicate} || sub { $_[0] !~ /\~/ };
    my $dirCreateMode = $options{dir_create_mode} || 0775;
    my $reloadFile = $options{reload_file};
    my ($relfh);
    if (defined($reloadFile)) {
	$relfh = new IO::File ">>$reloadFile" or die "make: cannot open '$reloadFile' for writing\n";
	$relfh->autoflush(1);
    }
    
    my $compilesub = sub {
	my ($srcfile) = $File::Find::name;
	return if (!-f $srcfile);
	return if defined($predicate) && !($predicate->($srcfile));
	my $compPath = substr($srcfile,length($sourceDir));
 	my $objfile = $srcfile;
	$objfile =~ s@^$sourceDir@$objectDir@;
	my ($objfiledir) = dirname($objfile);
	if (!-d $objfiledir) {
	    if (defined($dirCreateMode)) {
		print "creating directory $objfiledir\n" if $verbose;
		mkpath($objfiledir,0,$dirCreateMode);
	    } else {
		die "make: no such directory '$objfiledir'\n";
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
	    my ($objtext,$errmsg,$pureTextFlag);
	    print "compiling $srcfile\n" if $verbose;
	    if (!$self->parse(script_file=>$srcfile, result_text=>\$objtext, error=>\$errmsg, pure_text_flag=>\$pureTextFlag)) {
		if ($verbose) {
		    print "error";
		    if ($errorDir) {
			my $errfile = $srcfile;
			$errfile =~ s@^$sourceDir@$errorDir@;
			my ($errfiledir) = dirname($errfile);
			mkpath($errfiledir,0,$dirCreateMode);
			my $outfh = new IO::File ">$errfile" or die "make: cannot open '$errfile' for writing\n";
			$outfh->print($objtext);
			print " in $errfile";
		    }
		    print ":\n$errmsg\n";
		}
	    } else {
		my $outfh = new IO::File ">$objfile" or die "make: cannot open '$objfile' for writing\n";
		$outfh->print($objtext) if (!$pureTextFlag);
		$relfh->print("$compPath\n") if (defined($relfh));
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
	    die "make: no such file or directory '$fullpath'\n";
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
