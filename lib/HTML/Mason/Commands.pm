# Copyright (c) 1998 by Jonathan Swartz. All rights reserved.
# This program is free software; you can redistribute it and/or modify it
# under the same terms as Perl itself.

package HTML::Mason::Commands;

use strict;
use File::Basename;
use HTML::Mason::Utils;
use HTML::Mason::Tools qw(read_file chop_slash);
use HTML::Mason::Config;
use IO;
use Time::Local;

use vars qw($REQ @ISA @EXPORT_OK @EXPORT);
 
require Exporter;
@ISA=qw(Exporter);
@EXPORT = qw
    (mc_abort
     mc_auto_comp
     mc_auto_next
     mc_cache
     mc_cache_self
     mc_caller 
     mc_comp
     mc_comp_exists
     mc_comp_source
     mc_comp_stack
     mc_date 
     mc_dhandler_arg
     mc_file 
     mc_file_root 
     mc_filter_self
     mc_out 
     mc_time
     );

@EXPORT_OK=@EXPORT;

sub pure_text_handler
{
    $REQ->call_hooks('start_primary');
    $REQ->sink->($REQ->comp->source_ref_text);
    $REQ->call_hooks('end_primary');
}

my $no_auto_error = "called when no autohandler invoked";

sub check_request
{
    if (!defined($REQ)) {
	my ($caller) = ((caller)[3] =~ /[^:]+$/);
	die "$caller called outside of request environment\n";
    }
}

sub mc_abort
{
    check_request;
    $REQ->abort(@_);
}

sub mc_auto_comp
{
    check_request;
    my $aref = $REQ->{autohandler_next} or die "mc_auto_comp $no_auto_error";
    my $path = $aref->[0]->path;
    
    # return relative path if possible
    my $curdir = $REQ->comp->parent_path;
    $path =~ s{^$curdir/}{};
    return $path;
}

sub mc_auto_next
{
    check_request;
    my $aref = $REQ->{autohandler_next} or die "mc_auto_next $no_auto_error";
    my ($comp, $argsref) = @$aref;
    my %args = (%$argsref,@_);
    return $REQ->call($comp, %args);
}

sub mc_cache
{
    check_request;
    my (%options) = @_;
    my $interp = $REQ->interp;
    return undef if !$interp->use_data_cache;
    $options{action} = $options{action} || 'retrieve';
    $options{key} = $options{key} || 'main';
    
    my $comp = $REQ->comp;
    if ($comp->is_file_based) {
	$options{cache_file} = $interp->data_cache_filename($comp->path);
    } elsif (my $p = $comp->parent_comp) {
	$options{cache_file} = $interp->data_cache_filename($p->path);
	$options{key} = "subcomp:" . $comp->name . ":" . $options{key};
    } else {
	die "mc_cache: no data cache for anonymous component ".$comp->title;
    }
    if ($options{keep_in_memory}) {
	$options{memory_cache} = $interp->{data_cache_store};
	delete($options{keep_in_memory});
    }
    
    my $results = HTML::Mason::Utils::access_data_cache(%options);
    if ($options{action} eq 'retrieve') {
	$interp->write_system_log('CACHE_READ',$comp->title,$options{key},
				  defined $results ? 1 : 0);
    } elsif ($options{action} eq 'store') {
	$interp->write_system_log('CACHE_WRITE',$comp->title,$options{key});
    }
    return $results;
}

sub mc_cache_self
{
    check_request;
    my (%options) = @_;
    
    my $interp = $REQ->interp;
    return 0 if !$interp->use_data_cache;
    return 0 if $REQ->stack->[0]->{in_cache_self_flag};
    my (%retrieveOptions,%storeOptions);
    foreach (qw(key expire_if keep_in_memory busy_lock)) {
	if (exists($options{$_})) {
	    $retrieveOptions{$_} = $options{$_};
	}
    }
    foreach (qw(key expire_at expire_next expire_in)) {
	if (exists($options{$_})) {
	    $storeOptions{$_} = $options{$_};
	}
    }
    my $result = mc_cache(action=>'retrieve',%retrieveOptions);
    if (!defined($result)) {
	#
	# Reinvoke the component and collect output in $result.
	#
	my $lref = $REQ->stack->[0];
	my %saveLocals = %$lref;
	$lref->{sink} = sub { $result .= $_[0] };
	$lref->{in_cache_self_flag} = 1;
	my $sub = $lref->{comp}->code;
	my %args = %{$lref->{args}};
	&$sub(%args);
	$REQ->stack->[0] = {%saveLocals};
	mc_cache(action=>'store',value=>$result,%storeOptions);
    } else {
	$REQ->call_hooks('start_primary');
	$REQ->call_hooks('end_primary');
    }
    mc_out($result);
    return 1;
}

sub mc_caller ()
{
    check_request;
    if ($REQ->depth <= 1) {
	return undef;
    } else {
	return $REQ->{stack}->[1]->{comp}->title;
    }
}

sub mc_call_self
{
    check_request;
    my ($cref,$rref) = @_;
    return 0 if $REQ->stack->[0]->{in_call_self_flag};
    
    #
    # Reinvoke the component with in_call_self_flag=1. Collect
    # output and return value in references provided.
    #
    my $content;
    my $lref = $REQ->stack->[0];
    my %saveLocals = %$lref;
    $lref->{sink} = sub { $content .= $_[0] };
    $lref->{in_call_self_flag} = 1;
    my $sub = $lref->{comp}->code;
    my %args = %{$lref->{args}};
    if (ref($rref) eq 'SCALAR') {
	$$rref = &$sub(%args);
    } elsif (ref($rref) eq 'ARRAY') {
	@$rref = &$sub(%args);
    } else {
	&$sub(%args);
    }
    $REQ->{stack}->[0] = {%saveLocals};
    $$cref = $content if ref($cref) eq 'SCALAR';

    return 1;
}

sub mc_call_stack ()
{
    check_request;
    return map($_->{comp}->title,@{$REQ->{stack}});
}

sub mc_comp
{
    check_request;
    return $REQ->call(@_);
}

sub mc_comp_exists
{
    check_request;
    return ($REQ->fetch_comp($_[0])) ? 1 : 0;
}

sub mc_comp_source
{
    check_request;
    my ($compPath) = @_;
    
    $compPath = $REQ->process_comp_path($compPath);
    return $REQ->interp->comp_root.$compPath;
}

sub mc_comp_stack ()
{
    check_request;
    return map($_->{comp}->title,@{$REQ->stack});
}

#
# Version of DateManip::UnixDate that uses interpreter's notion
# of current time and caches daily results.
#
sub mc_date ($)
{
    check_request;
    my ($format) = @_;

    my $interp = $REQ->interp;
    my $time = $interp->current_time();
    if ($format =~ /%[^yYmfbhBUWjdevaAwEDxQF]/ || $time ne 'real' || !$interp->use_data_cache) {
	if ($time eq 'real') {
	    return Date::Manip::UnixDate('now',$format);
	} else {
	    return Date::Manip::UnixDate("epoch $time",$format);
	}
    } else {
	my %cacheOptions = (cache_file=>($interp->data_cache_filename('_global')),key=>'mc_date_formats',memory_cache=>($interp->{data_cache_store}));
	my $href = HTML::Mason::Utils::access_data_cache(%cacheOptions);
	if (!$href) {
	    my %dateFormats;
	    my @formatChars = qw(y Y m f b h B U W j d e v a A w E D x Q F);
	    my @formatVals = split("\cA",Date::Manip::UnixDate('now',join("\cA",map("%$_",@formatChars))));
	    my $i;
	    for ($i=0; $i<@formatChars; $i++) {
		$dateFormats{$formatChars[$i]} = $formatVals[$i];
	    }
	    $href = {%dateFormats};
	    HTML::Mason::Utils::access_data_cache(%cacheOptions,action=>'store',value=>$href,expire_next=>'day');
	}
	$format =~ s/%(.)/$href->{$1}/g;
	return $format;
    }
}

sub mc_dhandler_arg ()
{
    return $REQ->dhandler_arg;
}

sub mc_file ($)
{
    check_request;
    my ($file) = @_;
    my $interp = $REQ->interp;
    # filenames beginning with / or a drive letter (e.g. C:/) are absolute
    unless ($file =~ /^([A-Za-z]:)?\//) {
	if ($interp->static_file_root) {
	    $file = $interp->static_file_root . "/" . $file;
	} else {
	    $file = $interp->comp_root . $REQ->comp->parent_path . "/" . $file;
	}
    }
    $REQ->call_hooks('start_file',$file);
    my $content = read_file($file);
    $REQ->call_hooks('end_file',$file);
    return $content;
}

sub mc_file_root ()
{
    check_request;
    return $REQ->interp->static_file_root;
}

sub mc_out ($)
{
    check_request;
    $REQ->sink->($_[0]) if defined($_[0]);
}

sub mc_time
{
    check_request;
    my $time = $REQ->interp->current_time;
    $time = time() if $time eq 'real';
    return $time;
}

1;

__END__
