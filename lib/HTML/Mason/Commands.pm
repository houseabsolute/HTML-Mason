# Copyright (c) 1998-99 by Jonathan Swartz. All rights reserved.
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
     mc_call_self
     mc_comp
     mc_comp_exists
     mc_comp_source
     mc_comp_stack
     mc_date 
     mc_dhandler_arg
     mc_file 
     mc_file_root 
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

sub mc_abort { check_request; $REQ->abort(@_) }

sub mc_auto_comp
{
    check_request;
    my $comp = $REQ->auto_comp or die "mc_auto_comp $no_auto_error";
    my $path = $comp->path;
    
    # return relative path if possible
    my $curdir = $REQ->comp->dir_path;
    $path =~ s{^$curdir/}{};
    return $path;
}

sub mc_auto_next { check_request; $REQ->auto_next(@_) }
sub mc_cache { check_request; $REQ->cache(@_) }
sub mc_cache_self { check_request; $REQ->cache_self(@_) }
sub mc_caller { check_request; $REQ->caller(@_) }
sub mc_call_self { check_request; $REQ->call_self(@_) }
sub mc_call_stack { check_request; map($_->title,$REQ->callers) }
sub mc_comp { check_request; $REQ->call(@_) }
sub mc_comp_exists { check_request; $REQ->comp_exists(@_) }
sub mc_comp_source { check_request; $m->fetch_comp(shift)->source_file }
sub mc_comp_stack { check_request; map($_->title,$REQ->callers) }
sub mc_dhandler_arg { check_request; $REQ->dhandler_arg }
sub mc_file { check_request; $REQ->file(@_) }
sub mc_file_root { check_request; $REQ->file_root }
sub mc_out { check_request; $REQ->out(@_) }
sub mc_time { check_request; $REQ->time(@_) } 

1;

__END__
