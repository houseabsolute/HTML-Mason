#!/usr/bin/perl -w

use strict;

use File::Spec;
use HTML::Mason::Tests;

my $tests = make_tests();
$tests->run;

sub make_tests
{
    my $group = HTML::Mason::Tests->new( name => 'interp',
					 description => 'interp object functionality' );

    $group->add_support( path => '/autohandler_test/autohandler',
			 component => <<'EOF',
The recursive autohandler: <% $m->current_comp->path %>

<% $m->call_next %>
EOF
		       );

    $group->add_test( name => 'no recursive autohandlers',
		      description => 'tests turning off recursive autohandlers',
		      call_path => '/autohandler_test/subdir/hello',
		      interp_params => { allow_recursive_autohandlers => 0 },
		      component => <<'EOF',
Hello World!
EOF
		      expect => <<'EOF',
Hello World!
EOF
		    );

    $group->add_test( name => 'no recursive autohandlers',
		      description => 'tests turning off recursive autohandlers',
		      call_path => '/autohandler_test/subdir/hello',
		      component => <<'EOF',
Hello World!
EOF
		      expect => <<'EOF',
The recursive autohandler: /interp/autohandler_test/autohandler

Hello World!
EOF
		    );

    $group->add_support( path => '/autohandler_test/subdir/plainfile',
			 component => <<'EOF',
The local autohandler: <% $m->current_comp->path %>

<% $m->call_next %>
EOF
		       );

    $group->add_test( name => 'alternate autohandler name',
		      description => 'tests that providing an alternate name for autohandlers works',
		      call_path => '/autohandler_test/subdir/hello',
		      interp_params => { autohandler_name => 'plainfile' },
		      component => <<'EOF',
Hello World!
EOF
		      expect => <<'EOF',
The local autohandler: /interp/autohandler_test/subdir/plainfile

Hello World!
EOF
		    );

    my $alt_root = File::Spec->catdir( HTML::Mason::Tests->base_path, 'alt_comps' );
    my @roots = ( [ main => HTML::Mason::Tests->comp_root,],
		  [ alt => $alt_root ] );

    $group->add_support( path => '/comp_root_test/showcomp',
			 component => <<'EOF',
% my $comp = $m->callers(1);
<& /shared/display_comp_obj, comp=>$comp &>
EOF
		       );

    $group->add_test( name => 'shared',
		      description => '??',
		      call_path => '/comp_root_test/shared',
		      interp_params => { comp_root => \@roots },
		      component => <<'EOF',
shared in the main component root.
<& showcomp &>
EOF
		      expect => <<'EOF',
shared in the main component root.
Declared args:

This is my first time.
I am not a subcomponent.
I am file-based.
My short name is shared.
My directory is /interp/comp_root_test.
I have run 1 time(s).
I have 0 subcomponent(s).
My title is /interp/comp_root_test/shared [main].

My cache file is /.../cache/MAIN+2finterp+2fcomp_root_test+2fshared
My object file is /.../obj/MAIN/interp/comp_root_test/shared
My path is /interp/comp_root_test/shared.
My fq_path is /MAIN/interp/comp_root_test/shared.
My source file is /.../comps/interp/comp_root_test/shared
My source dir is /.../comps/interp/comp_root_test



EOF
		    );

    $group->add_test( name => 'shared',
		      description => '??',
		      call_path => '/comp_root_test/shared',
		      interp_params => { comp_root => \@roots },
		      component => <<'EOF',
shared in the main component root.
<& showcomp &>
EOF
		      expect => <<'EOF',
shared in the main component root.
Declared args:

This is my first time.
I am not a subcomponent.
I am file-based.
My short name is shared.
My directory is /interp/comp_root_test.
I have run 1 time(s).
I have 0 subcomponent(s).
My title is /interp/comp_root_test/shared [main].

My cache file is /.../cache/MAIN+2finterp+2fcomp_root_test+2fshared
My object file is /.../obj/MAIN/interp/comp_root_test/shared
My path is /interp/comp_root_test/shared.
My fq_path is /MAIN/interp/comp_root_test/shared.
My source file is /.../comps/interp/comp_root_test/shared
My source dir is /.../comps/interp/comp_root_test



EOF
		    );

    $group->add_test( name => 'private1',
		      description => '??',
		      call_path => '/comp_root_test/private1',
		      interp_params => { comp_root => \@roots },
		      component => <<'EOF',
private1 in the main component root.
<& showcomp &>
EOF
		      expect => <<'EOF',
private1 in the main component root.
Declared args:

This is my first time.
I am not a subcomponent.
I am file-based.
My short name is private1.
My directory is /interp/comp_root_test.
I have run 1 time(s).
I have 0 subcomponent(s).
My title is /interp/comp_root_test/private1 [main].

My cache file is /.../cache/MAIN+2finterp+2fcomp_root_test+2fprivate1
My object file is /.../obj/MAIN/interp/comp_root_test/private1
My path is /interp/comp_root_test/private1.
My fq_path is /MAIN/interp/comp_root_test/private1.
My source file is /.../comps/interp/comp_root_test/private1
My source dir is /.../comps/interp/comp_root_test



EOF
		    );

    #HACK!
    HTML::Mason::Tests->write_comp( '/alt_root/comp_root_test/private2', File::Spec->catdir( $alt_root, 'comp_root_test' ), 'private2', <<'EOF' );
private2 in the alternate component root.
<& showcomp &>
EOF

    HTML::Mason::Tests->write_comp( '/alt_root/comp_root_test/shared', File::Spec->catdir( $alt_root, 'comp_root_test' ), 'shared', <<'EOF' );
shared.html in the alternate component root.
<& showcomp &>
EOF
    $group->add_test( name => 'private1',
		      description => '??',
		      call_path => '/comp_root_test/private2',
		      path => '/foo', # its already written.  HACK!
		      interp_params => { comp_root => \@roots },
		      component => <<'EOF',
foo
EOF
		      expect => <<'EOF',
private2 in the alternate component root.
Declared args:

This is my first time.
I am not a subcomponent.
I am file-based.
My short name is private2.
My directory is /interp/comp_root_test.
I have run 1 time(s).
I have 0 subcomponent(s).
My title is /interp/comp_root_test/private2 [alt].

My cache file is /.../cache/ALT+2finterp+2fcomp_root_test+2fprivate2
My object file is /.../obj/ALT/interp/comp_root_test/private2
My path is /interp/comp_root_test/private2.
My fq_path is /ALT/interp/comp_root_test/private2.
My source file is /.../alt_root/interp/comp_root_test/private2
My source dir is /.../alt_root/interp/comp_root_test



EOF
		    );
    return $group;
}

__END__

# multiple comp_roots
{my @roots = ([main=>$comp_root],[alt=>"$root/test/alt_root/"]);
 try_exec_with_interp({comp_root=>\@roots},'comp_root_test/shared.html');
 try_exec_with_interp({comp_root=>\@roots},'comp_root_test/private1.html');
 try_exec_with_interp({comp_root=>\@roots},'comp_root_test/private2.html');}

# code cache
{my $interp = basic_interp();
 $interp->code_cache_max_size(5300);
 $interp->exec("/interp/code_cache_test/use1");
 try_exec($interp,"code_cache_test/show_code_cache",1);
 $interp->exec("/interp/code_cache_test/use2"); 
 try_exec($interp,"code_cache_test/show_code_cache",2);
 $interp->exec("/interp/code_cache_test/use3");
 try_exec($interp,"code_cache_test/show_code_cache",3);}

# current_time
try_exec_with_interp({current_time=>945526402},'current_time');

# dhandler_name
try_exec_with_interp({dhandler_name=>'plainfile'},'dhandler_test/foo/blag');

# max_recurse
try_exec_with_interp({},'max_recurse_8');
try_exec_with_interp({},'max_recurse_24',1);
try_exec_with_interp({max_recurse=>50},'max_recurse_24',2);

# out_mode
try_exec_with_interp({},'out_mode',1);
try_exec_with_interp({out_mode=>'stream'},'out_mode',2);

# preloads
try_exec_with_interp({},'preloads_test/show_code_cache',1);
try_exec_with_interp({preloads=>['/interp/preloads_test/hello']},'preloads_test/show_code_cache',2);
try_exec_with_interp({preloads=>['/interp/preloads_test/*']},'preloads_test/show_code_cache',3);

# try_exec_with_interp({use_data_cache=>0},'/request/cache');

# set_global
{my $interp = basic_interp();
 $interp->parser->allow_globals(qw($global));
 $interp->set_global(global=>'parsimmon');
 try_exec($interp,'set_global');}

exit;

# system_log_xxx
my $log_file = "$root/test/data/etc/system.log";
unlink($log_file);
{my $interp = basic_interp(system_log_events=>'COMP_LOAD',system_log_separator=>'||',out_method=>sub {});
 $interp->exec('/interp/autohandler_test/subdir/hello');
 $interp->exec('/interp/max_recurse_8');}
try_exec_with_interp({},'system_log');

1;
