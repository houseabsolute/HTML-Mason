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

#------------------------------------------------------------

    $group->add_support( path => '/autohandler_test/autohandler',
			 component => <<'EOF',
The recursive autohandler: <% $m->current_comp->path %>

<% $m->call_next %>
EOF
		       );


#------------------------------------------------------------

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


#------------------------------------------------------------

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


#------------------------------------------------------------

    $group->add_support( path => '/autohandler_test/subdir/plainfile',
			 component => <<'EOF',
The local autohandler: <% $m->current_comp->path %>

<% $m->call_next %>
EOF
		       );


#------------------------------------------------------------

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

    my $alt_root = File::Spec->catdir( HTML::Mason::Tests->base_path, 'alt_root' );
    my @roots = ( [ main => HTML::Mason::Tests->comp_root],
		  [ alt => $alt_root ] );


    #HACK!
    HTML::Mason::Tests->write_comp( '/alt_root/interp/comp_root_test/private2',
				    File::Spec->catdir( $alt_root, 'interp', 'comp_root_test' ),
				    'private2',
				    <<'EOF' );
private2 in the alternate component root.
<& showcomp &>
EOF

    HTML::Mason::Tests->write_comp( '/alt_root/interp/comp_root_test/shared',
				    File::Spec->catdir( $alt_root, 'interp', 'comp_root_test' ),
				    'shared',
				    <<'EOF' );
shared.html in the alternate component root.
<& showcomp &>
EOF


#------------------------------------------------------------

    $group->add_support( path => '/comp_root_test/showcomp',
			 component => <<'EOF',
% my $comp = $m->callers(1);
<& /shared/display_comp_obj, comp=>$comp &>
EOF
		       );


#------------------------------------------------------------

    $group->add_test( name => 'shared',
		      description => 'test that component in both comp_roots is called in first comp_root',
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


#------------------------------------------------------------

    $group->add_test( name => 'private1',
		      description => 'test that component in first comp_root is found',
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


#------------------------------------------------------------

    $group->add_test( name => 'private2',
		      description => 'test that component in second comp_root is found',
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


#------------------------------------------------------------

    $group->add_test( name => 'current_time',
		      description => 'test current_time interp param',
		      interp_params => { current_time => 945526402 },
		      component => <<'EOF',
<% $m->time %>
EOF
		      expect => <<'EOF',
945526402
EOF
		    );


#------------------------------------------------------------

    $group->add_support( path => 'support/recurse_test',
			 component => <<'EOF',
Entering <% $count %><p>
% if ($count < $max) {
<& recurse_test, count=>$count+1, max=>$max &>
% }
Exiting <% $count %><p>\
<%args>
$count=>0
$max
</%args>
EOF
		       );


#------------------------------------------------------------

    $group->add_test( name => 'max_recurse_1',
		      description => 'Test that recursion 8 levels deep is allowed',
		      component => <<'EOF',
% eval { $m->comp('support/recurse_test', max=>8) };
EOF
		      expect => <<'EOF',
Entering 0<p>
Entering 1<p>
Entering 2<p>
Entering 3<p>
Entering 4<p>
Entering 5<p>
Entering 6<p>
Entering 7<p>
Entering 8<p>
Exiting 8<p>
Exiting 7<p>
Exiting 6<p>
Exiting 5<p>
Exiting 4<p>
Exiting 3<p>
Exiting 2<p>
Exiting 1<p>
Exiting 0<p>
EOF
		    );


#------------------------------------------------------------

    $group->add_test( name => 'max_recurse_2',
		      description => 'Test that recursion is stopped after 32 levels',
		      interp_params => { out_mode => 'stream' },
		      component => <<'EOF',
% eval { $m->comp('support/recurse_test', max=>48) };
<& /shared/check_error, error=>$@ &>
EOF
		      expect => <<'EOF',
Entering 0<p>
Entering 1<p>
Entering 2<p>
Entering 3<p>
Entering 4<p>
Entering 5<p>
Entering 6<p>
Entering 7<p>
Entering 8<p>
Entering 9<p>
Entering 10<p>
Entering 11<p>
Entering 12<p>
Entering 13<p>
Entering 14<p>
Entering 15<p>
Entering 16<p>
Entering 17<p>
Entering 18<p>
Entering 19<p>
Entering 20<p>
Entering 21<p>
Entering 22<p>
Entering 23<p>
Entering 24<p>
Entering 25<p>
Entering 26<p>
Entering 27<p>
Entering 28<p>
Entering 29<p>
Entering 30<p>
Error: 32 levels deep in component stack (infinite recursive call?)

EOF
		    );


#------------------------------------------------------------

    $group->add_test( name => 'max_recurse_3',
		      description => 'Test interp max_recurse param',
		      interp_params => { max_recurse => 50 },
		      component => <<'EOF',
% eval { $m->comp('support/recurse_test', max=>48) };
<& /shared/check_error, error=>$@ &>
EOF
		      expect => <<'EOF',
Entering 0<p>
Entering 1<p>
Entering 2<p>
Entering 3<p>
Entering 4<p>
Entering 5<p>
Entering 6<p>
Entering 7<p>
Entering 8<p>
Entering 9<p>
Entering 10<p>
Entering 11<p>
Entering 12<p>
Entering 13<p>
Entering 14<p>
Entering 15<p>
Entering 16<p>
Entering 17<p>
Entering 18<p>
Entering 19<p>
Entering 20<p>
Entering 21<p>
Entering 22<p>
Entering 23<p>
Entering 24<p>
Entering 25<p>
Entering 26<p>
Entering 27<p>
Entering 28<p>
Entering 29<p>
Entering 30<p>
Entering 31<p>
Entering 32<p>
Entering 33<p>
Entering 34<p>
Entering 35<p>
Entering 36<p>
Entering 37<p>
Entering 38<p>
Entering 39<p>
Entering 40<p>
Entering 41<p>
Entering 42<p>
Entering 43<p>
Entering 44<p>
Entering 45<p>
Entering 46<p>
Entering 47<p>
Entering 48<p>
Exiting 48<p>
Exiting 47<p>
Exiting 46<p>
Exiting 45<p>
Exiting 44<p>
Exiting 43<p>
Exiting 42<p>
Exiting 41<p>
Exiting 40<p>
Exiting 39<p>
Exiting 38<p>
Exiting 37<p>
Exiting 36<p>
Exiting 35<p>
Exiting 34<p>
Exiting 33<p>
Exiting 32<p>
Exiting 31<p>
Exiting 30<p>
Exiting 29<p>
Exiting 28<p>
Exiting 27<p>
Exiting 26<p>
Exiting 25<p>
Exiting 24<p>
Exiting 23<p>
Exiting 22<p>
Exiting 21<p>
Exiting 20<p>
Exiting 19<p>
Exiting 18<p>
Exiting 17<p>
Exiting 16<p>
Exiting 15<p>
Exiting 14<p>
Exiting 13<p>
Exiting 12<p>
Exiting 11<p>
Exiting 10<p>
Exiting 9<p>
Exiting 8<p>
Exiting 7<p>
Exiting 6<p>
Exiting 5<p>
Exiting 4<p>
Exiting 3<p>
Exiting 2<p>
Exiting 1<p>
Exiting 0<p>No error!?

EOF
		    );


#------------------------------------------------------------

    $group->add_support( path => 'code_cache_test/show_code_cache',
			 component => <<'EOF',
Code cache contains these plain components:
% my %c = %{$m->interp->{code_cache}};
% foreach ( sort grep { /plain/ } keys %c ) {
<% $_ %>
% }
% if ( ! grep { /plain/ } keys %c ) {
Code cache contains no files matching /plain/
% }
EOF
		       );


#------------------------------------------------------------

    $group->add_support( path => 'code_cache_test/plain1',
			 component => <<'EOF'
plain1
%#AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
%#BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB
%#CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
%#DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD
EOF
		       );


#------------------------------------------------------------

    $group->add_support( path => 'code_cache_test/plain2',
			 component => <<'EOF'
plain2
%#AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
%#BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB
%#CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
%#DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD
EOF
		       );


#------------------------------------------------------------

    $group->add_support( path => 'code_cache_test/plain3',
			 component => <<'EOF'
plain3
%#AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
%#BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB
%#CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
%#DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD
EOF
		       );


#------------------------------------------------------------

    $group->add_support( path => 'code_cache_test/plain4',
			 component => <<'EOF'
plain4
%#AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
%#BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB
%#CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
%#DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD
%#AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
%#BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB
%#CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
%#DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD
%#AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
%#BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB
%#CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
%#DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD
%#AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
%#BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB
%#CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
%#DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD
EOF
		       );


#------------------------------------------------------------

    $group->add_support( path => 'code_cache_test/plain5',
			 component => <<'EOF'
plain5
%#AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
%#BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB
%#CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
%#DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD
EOF
		       );


#------------------------------------------------------------

    $group->add_support( path => 'code_cache_test/plain6',
			 component => <<'EOF'
plain6
%#AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
%#BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB
%#CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
%#DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD
EOF
		       );


#------------------------------------------------------------

    $group->add_support( path => 'code_cache_test/plain7',
			 component => <<'EOF'
plain7
%#AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
%#BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB
%#CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
%#DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD
EOF
		       );

#------------------------------------------------------------

    my $interp = HTML::Mason::Interp->new( data_dir => $group->data_dir,
					   comp_root => $group->comp_root,
					   code_cache_max_size => 9000 );

    $group->add_test( name => 'code_cache_test/code_cache_1',
		      description => 'Run in order to load up code cache',
		      interp => $interp,
		      component => <<'EOF',
<& plain1 &><& plain1 &><& plain1 &><& plain2 &><& plain2 &><& plain2 &><& plain3 &><& plain4 &><& plain2 &><& plain2 &><& plain1 &><& plain5 &><& plain2 &><& plain4 &><& plain4 &><& plain5 &><& plain5 &>

EOF
		      skip_expect => 1
		    );


#------------------------------------------------------------

    $group->add_test( name => 'code_cache_test/show_code_cache_1',
		      description => 'Show code cache size after first usage',
		      interp => $interp,
		      component => <<'EOF',
<& show_code_cache &>
EOF
		      expect => <<'EOF',
Code cache contains these plain components:
/interp/code_cache_test/plain1
/interp/code_cache_test/plain2
/interp/code_cache_test/plain3
/interp/code_cache_test/plain5
EOF
		    );


#------------------------------------------------------------

    $group->add_test( name => 'code_cache_test/code_cache_2',
		      description => 'Run in order to load up code cache',
		      interp => $interp,
		      component => <<'EOF',
<& plain6 &><& plain6 &><& plain6 &><& plain6 &>
EOF
		      skip_expect => 1
		    );


#------------------------------------------------------------

    $group->add_test( name => 'code_cache_test/show_code_cache_2',
		      description => 'Show code cache size after second usage',
		      interp => $interp,
		      component => <<'EOF',
<& show_code_cache &>
EOF
		      expect => <<'EOF',
Code cache contains these plain components:
/interp/code_cache_test/plain1
/interp/code_cache_test/plain2
/interp/code_cache_test/plain3
/interp/code_cache_test/plain5
/interp/code_cache_test/plain6
EOF
		    );


#------------------------------------------------------------

    $group->add_test( name => 'code_cache_test/code_cache_3',
		      description => 'Run in order to load up code cache',
		      interp => $interp,
		      component => <<'EOF',
<& plain7 &><& plain7 &><& plain7 &>
EOF
		      skip_expect => 1
		    );


#------------------------------------------------------------

    $group->add_test( name => 'code_cache_test/show_code_cache_3',
		      description => 'Show code cache size after second usage',
		      interp => $interp,
		      component => <<'EOF',
<& show_code_cache &>
EOF
		      expect => <<'EOF',
Code cache contains these plain components:
/interp/code_cache_test/plain1
/interp/code_cache_test/plain2
/interp/code_cache_test/plain5
/interp/code_cache_test/plain6
/interp/code_cache_test/plain7
EOF
		    );


#------------------------------------------------------------

    $group->add_test( name => 'dhandler_name',
		      description => 'Test that providing an alternate name for dhandlers works',
		      path => 'dhandler_test/plainfile',
		      call_path => 'dhandler_test/foo/blag',
		      interp_params => { dhandler_name => 'plainfile' },
		      component => <<'EOF',
dhandler arg = <% $m->dhandler_arg %>
EOF
		      expect => <<'EOF',
dhandler arg = foo/blag
EOF
		    );


#------------------------------------------------------------

    $group->add_support( path => 'mode_test',
			 component => <<'EOF',
First of all I'd
% $m->clear_buffer;
No what I really wanted to say was
% $m->clear_buffer;
Oh never mind.
EOF
		       );


#------------------------------------------------------------

    $group->add_test( name => 'batch_mode',
		      description => 'Test that batch mode setting works',
		      component => <<'EOF',
<& mode_test &>
EOF
		      expect => <<'EOF',
Oh never mind.
EOF
		    );


#------------------------------------------------------------

    $group->add_test( name => 'stream_mode',
		      description => 'Test that stream mode setting works',
		      interp_params => { out_mode => 'stream' },
		      component => <<'EOF',
<& mode_test &>
EOF
		      expect => <<'EOF',
First of all I'd
No what I really wanted to say was
Oh never mind.
EOF
		    );


#------------------------------------------------------------

    $group->add_support( path => 'preloads_test/show_code_cache',
			 component => <<'EOF',
Code cache contains:
% my %c = %{$m->interp->{code_cache}};
<% join("\n",sort(keys(%c))) %>
EOF
		    );



#------------------------------------------------------------

    $group->add_support( path => 'preloads_test/hello',
			 component => 'hello',
		       );


#------------------------------------------------------------

    $group->add_support( path => 'preloads_test/goodbye',
			 component => 'goodbye',
		       );


#------------------------------------------------------------

    $group->add_support( path => 'preloads_test/howareyou',
			 component => 'howareyou',
		       );


#------------------------------------------------------------

    $group->add_support( path => 'preloads_test/subdir/in_a_subdir',
			 component => 'howareyou',
		       );

#------------------------------------------------------------

    $group->add_test( name => 'preload_1',
		      description => 'Make sure no preloading is done by default',
		      component => <<'EOF',
<& preloads_test/show_code_cache &>
EOF
		      expect => <<'EOF',
Code cache contains:
/interp/preload_1
/interp/preloads_test/show_code_cache
EOF
		    );


#------------------------------------------------------------

    $group->add_test( name => 'preload_2',
		      description => 'Preload a single component by filename',
		      interp_params => { preloads => [ '/interp/preloads_test/hello' ] },
		      component => <<'EOF',
<& preloads_test/show_code_cache &>
EOF
		      expect => <<'EOF',
Code cache contains:
/interp/preload_2
/interp/preloads_test/hello
/interp/preloads_test/show_code_cache
EOF
		    );


#------------------------------------------------------------

    $group->add_test( name => 'preload_3',
		      description => 'Preload all components (including subdirectory) by glob pattern',
		      interp_params => { preloads => [ '/interp/preloads_test/*', '/interp/preloads_test/*/*' ] },
		      component => <<'EOF',
<& preloads_test/show_code_cache &>
EOF
		      expect => <<'EOF',
Code cache contains:
/interp/preload_3
/interp/preloads_test/goodbye
/interp/preloads_test/hello
/interp/preloads_test/howareyou
/interp/preloads_test/show_code_cache
/interp/preloads_test/subdir/in_a_subdir
EOF
		    );

#------------------------------------------------------------

    $interp = HTML::Mason::Interp->new( data_dir => $group->data_dir,
					comp_root => $group->comp_root,
					 );
    $interp->compiler->set_allowed_globals( qw($global) );
    $interp->set_global( global => 'parsimmon' );


    $group->add_test( name => 'globals',
		      description => 'Test setting a global in interp & compiler objects',
		      interp => $interp,
		      component => <<'EOF',
<% $global %>
EOF
		      expect => <<'EOF',
parsimmon
EOF
		    );

#------------------------------------------------------------

    my $log_file = File::Spec->catfile( $group->base_path, 'system.log' );
    unlink $log_file;

    $interp = HTML::Mason::Interp->new( data_dir => $group->data_dir,
					comp_root => $group->comp_root,
					system_log_file => $log_file,
					system_log_events => 'COMP_LOAD',
					system_log_separator => ':::',
				      );


    $group->add_test( name => 'system_log',
		      description => 'Test system log COMP_LOAD event',
		      interp => $interp,
		      component => <<"EOF",
<%perl>
local \*F;
open F, '$log_file';
my \@f = <F>;
my \@files = map { chomp; my \@i = split /:::/; \$i[3] } \@f;
</\%perl>
Number of files: <% scalar \@files %>
Filename: <% \$files[0] %>
EOF
		      expect => <<'EOF',
Number of files: 1
Filename: /interp/system_log
EOF
		    );

#------------------------------------------------------------

    $group->add_support( path => '/comp_path_test/a/b/c/foo',
			 component => <<'EOF',
I am foo!
EOF
		       );

#------------------------------------------------------------

    $group->add_test( name => 'process_comp_path',
		      description => 'Test that component paths cannot be resolved outside the comp root',
		      component => <<'EOF',
<& ../../../../../interp/comp_path_test/a/b/c/../c/foo &>
EOF
		      expect => <<'EOF'
I am foo!

EOF
		    );

#------------------------------------------------------------

    $group->add_test( name => 'process_comp_path2',
		      description => 'Test that component paths containing /../ work as long they stay in the comp root',
		      path => '/comp_path_test/a/b/d/process',
		      call_path => '/comp_path_test/a/b/d/process',
		      component => <<'EOF',
<& ../c/foo &>
EOF
		      expect => <<'EOF'
I am foo!

EOF
		    );

#------------------------------------------------------------

    $group->add_test( name => 'overriding_die_handler',
		      description => 'Test overriding the $SIG{__DIE__} handler',
		      interp_params => { die_handler => sub { die "BAR\n" } },
		      component => <<'EOF',
<% die 'foo' %>
EOF
		      expect_error => '^\\s*BAR\\s*$',
		    );

#------------------------------------------------------------

    $group->add_test( name => 'normal_die_handler',
		      description => 'Test normal $SIG{__DIE__} handler',
		      component => <<'EOF',
<% die 'foo' %>
EOF
		      expect_error => 'while executing /interp/normal_die_handler',
		    );

#------------------------------------------------------------

    return $group;
}
