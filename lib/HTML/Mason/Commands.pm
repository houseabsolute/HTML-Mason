# Copyright (c) 1998-99 by Jonathan Swartz. All rights reserved.
# This program is free software; you can redistribute it and/or modify it
# under the same terms as Perl itself.

#
# Old-style mc_ commands left in for backwards compatibility. All are now
# implemented in terms of Request methods.
#

package HTML::Mason::Commands;

use strict;

use vars qw($m @ISA @EXPORT_OK @EXPORT);

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

my $no_auto_error = "called when no autohandler invoked";

sub check_request
{
    if (!defined($m)) {
	my ($caller) = ((caller)[3] =~ /[^:]+$/);
	die "$caller called outside of request environment\n";
    }
}

sub mc_abort { check_request; $m->abort(@_) }

sub mc_auto_comp
{
    check_request;
    my $comp = $m->fetch_next or die "mc_auto_comp $no_auto_error";
    my $path = $comp->path;
    
    # return relative path if possible
    my $curdir = $m->current_comp->dir_path;
    $path =~ s{^$curdir/}{};
    return $path;
}

sub mc_auto_next { check_request; $m->call_next(@_) }
sub mc_cache { check_request; $m->cache(@_) }
sub mc_cache_self { check_request; $m->cache_self(@_) }
sub mc_caller { check_request; $m->caller->path }
sub mc_call_self { check_request; $m->call_self(@_) }
sub mc_call_stack { check_request; map($_->title,$m->callers) }
sub mc_comp { check_request; $m->comp(@_) }
sub mc_comp_exists { check_request; $m->comp_exists(@_) }
sub mc_comp_source { check_request; $m->fetch_comp(shift)->source_file }
sub mc_comp_stack { check_request; map($_->title,$m->callers) }
sub mc_dhandler_arg { check_request; $m->dhandler_arg }
sub mc_file { check_request; $m->file(@_) }
sub mc_file_root { check_request; $m->file_root }
sub mc_out { check_request; $m->out(@_) }
sub mc_time { check_request; $m->time(@_) } 

1;

__END__
