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
    my $script = $options{script};
    my $scriptFile = $options{script_file};
    my $errorRef = $options{error};
    my $objectFile = $options{object_file};
    my ($sub, $err, $errpos);
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
    my %sectiontext = (map(($_,''),qw(args cleanup doc filter init once)));
    my $curpos = 0;
    my @textsegs;
    my $scriptlength = length($script);
    while ($script =~ /(<%(?:perl_)?(args|cleanup|doc|filter|init|once|text)>)/ig) {
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
    $body .= 'my (%ARGS) = @_;'."\n";
    $body .= 'my $_out = $REQ->topstack->{sink};'."\n";

    #
    # Process args section.
    #
    my @declaredArgs = ();
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

	    push(@declaredArgs,{var=>$var,type=>$type,name=>$name,(defined($default) ? (default=>$default) : ())});

	    if (defined($default)) {
		$defaultClause = "$var = $default";
	    } else {
		$defaultClause = "die \"no value sent for required parameter '$name'\"";
	    }
	    
	    # Scalar
	    if ($type eq "\$") {
		my $tmpl = '$val = $ARGS{\'%s\'}; if (!exists($ARGS{\'%s\'})) { %s; } else { %s; }';
		$argsec .= sprintf($tmpl,$name,$name,$defaultClause,
				   "$var = \$val");
	    }
	    # Array
	    if ($type eq "\@") {
		my $tmpl = '$val = $ARGS{\'%s\'}; if (!exists($ARGS{\'%s\'})) { %s; } elsif (ref($val) eq \'ARRAY\') { %s; } else { %s; }';
		$argsec .= sprintf($tmpl,$name,$name,$defaultClause,
				   "$var = \@\$val",
				   "$var = (\$val)");
	    }
	    # Hash
	    if ($type eq "\%") {
		my $tmpl = '$val = $ARGS{\'%s\'}; if (!exists($ARGS{\'%s\'})) { %s; } elsif (ref($val) eq \'ARRAY\') { %s; } elsif (ref($val) eq \'HASH\') { %s } else { %s; }';
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
    my $alphalength=0;
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
		my $call = substr($text,$c+2,$length);
		for ($call) { s/^\s+//; s/\s+$// }
		if (substr($call,0,1) =~ /[A-Za-z0-9\/_.]/) {
		    # Literal component path; put quotes around it
		    my $comma = index($call,',');
		    $comma = length($call) if ($comma==-1);
		    (my $comp = substr($call,0,$comma)) =~ s/\s+$//;
		    $call = "'$comp'".substr($call,$comma);
		}
		$perl = "mc_comp($call);";
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
    my $useSourceReference = defined($objectFile) && ($pureTextFlag || $self->source_refer_predicate->($scriptlength,$alphalength));
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
	$body .= sprintf('{ my ($_c,$_r); if (mc_call_self(\$_c,\$_r)) { for ($_c) { %s } mc_out($_c); return $_r }};',$ftext);
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
    $header .= "package $pkg;\n";
    $header .= "use strict;\n" if $self->use_strict;
    $header .= sprintf("use vars qw(%s);\n",join(" ","\$REQ",@{$self->{'allow_globals'}}));
    $header .= "\n".$sectiontext{once}."\n" if $sectiontext{once};

    #
    # Assemble parameters for component.
    #
    my @cparams = ("parser_version=>$parserVersion","create_time=>".time());
    push(@cparams,"source_ref_start=>0000000") if $useSourceReference;
    if (@declaredArgs) {
	my $d = new Data::Dumper ([@declaredArgs]);
	my $argsDump = $d->Dumpxs;
	for ($argsDump) { s/\$VAR1\s*=//g; s/;\s*// }
	push(@cparams,"declared_args=>$argsDump");
    }
    if ($pureTextFlag) {
	push(@cparams,"code=>\\&HTML::Mason::Commands::pure_text_handler");
    } else {
	push(@cparams,"code=>sub {\n$body\n}");
    }
    push(@cparams,"path=>'$options{path}'") if (defined($options{path}));
    push(@cparams,"parent_path=>'$options{parent_path}'") if (defined($options{parent_path}));
    push(@cparams,"source_file=>'$options{script_file}'") if (defined($options{script_file}));
    my $cparamstr = join(",\n",@cparams);
    
    $body = "\nnew HTML::Mason::Component (\n$cparamstr\n);\n";
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
    # Load component, check for errors and warnings.
    #
    my $comp;
    my $ignoreExpr = $self->ignore_warnings_expr;
    {
	my $warnstr;
	local $^W = 1;
	local $SIG{__WARN__} = $ignoreExpr ? sub { $warnstr .= $_[0] if $_[0] !~ /$ignoreExpr/ } : sub { $warnstr .= $_[0] };
	($body) = ($body =~ /^(.*)$/s) if $self->taint_check;
	$comp = eval($body);
	$err = $warnstr . $@;
	# attempt to stem very long eval errors
	if ($err =~ /has too many errors\./) {
	    $err =~ s/has too many errors\..*/has too many errors./s;
	}
    }
    $parseError = 0;

    #
    # Process parsing and compilation errors.
    #
    parse_error:
    my $success = 1;
    if ($err) {
	if (defined($errpos)) {
	    my $linenum = (substr($script,0,$errpos) =~ tr/\n//) + 1;
	    $err .= " (line $linenum)";
	}
	$err .= "\n";
	$success = 0;
	$$errorRef = $err if defined($errorRef);
    }
    
    if (defined($objectFile)) {
	my @newfiles = ($objectFile);

	# Create object file.  We attempt to handle several cases
	# in which a file already exists and we wish to create a
	# directory, or vice versa.  However, not every case is
	# handled; to be complete, mkpath would have to unlink any
	# existing file in its way.
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
	print $fh $body;
	$fh->close;
	if (my $lref = $options{files_written}) {
	    @$lref = @newfiles;
	}
	
	$comp->object_file($objectFile) if $comp;
    }
    
    if ($success) {
	return $comp;
    } else {
	return undef;
    }
}

#
# Evaluate an object file.  Return a component object or undef if
# error. Optional second argument is a scalar reference which will be
# populated with the error message if any.
#
sub eval_object_file
{    
    my ($self, $objfile, $errref) = @_;
    my $ignoreExpr = $self->ignore_warnings_expr;
    ($objfile) = ($objfile =~ /^(.*)$/s) if $self->taint_check;
    my ($comp,$err);
    {
	my $warnstr;
	local $^W = 1;
	local $SIG{__WARN__} = $ignoreExpr ? sub { $warnstr .= $_[0] if $_[0] !~ /$ignoreExpr/ } : sub { $warnstr .= $_[0] };
	($objfile) = ($objfile =~ /^(.*)$/s) if $self->taint_check;
	$comp = do($objfile);
	$err = $warnstr . $@;
    }

    #
    # Detect various forms of older, incompatible object files:
    #  -- zero-sized files (previously signifying pure text components)
    #  -- pre-0.7 files that return code refs
    #  -- valid components but with an earlier parser_version
    #
    my $parserVersion = version();
    my $incompat = "Incompatible object file ($objfile);\nobject file was created by %s and you are running parser version $parserVersion.\nAsk your administrator to clear the object directory.\n";
    $err = sprintf($incompat,"a pre-0.7 parser") if (-z $objfile);
    if ($comp) {
	if (ref($comp) eq 'CODE') {
	    $err = sprintf($incompat,"a pre-0.7 parser");
	} elsif (ref($comp) !~ /HTML::Mason::Component/) {
	    $err = "object file ($objfile) did not return a component object!";
	} elsif ($comp->parser_version != $parserVersion) {
	    $err = sprintf($incompat,"parser version ".$comp->parser_version);
	}
    }
    if ($err) {
	# attempt to stem very long eval errors
	if ($err =~ /has too many errors\./) {
	    $err =~ s/has too many errors\..*/has too many errors./s;
	}
	$$errref = $err if defined($errref);
	return undef;
    } else {
	$comp->object_file($objfile);
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
 	my $objfile = $srcfile;
	$objfile =~ s@^$sourceDir@$objectDir@;
	my ($objfiledir) = dirname($objfile);
	if (!-d $objfiledir) {
	    if (defined($dirCreateMode)) {
		print "creating directory $objfiledir\n" if $verbose;
		mkpath($objfiledir,0,$dirCreateMode);
		die "make_dirs: cannot create directory '$objfiledir': $!";
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
			my $outfh = new IO::File ">$errfile" or die "make_dirs: cannot open '$errfile' for writing: $!";
			$outfh->print($objtext);
			print " in $errfile";
		    }
		    print ":\n$errmsg\n";
		}
	    } else {
		my $outfh = new IO::File ">$objfile" or die "make_dirs: cannot open '$objfile' for writing: $!";
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
