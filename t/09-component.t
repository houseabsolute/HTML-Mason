#!/usr/bin/perl -w

use strict;
use HTML::Mason::Tests;

my $tests = make_tests();
$tests->run;

sub make_tests
{
    my $group = HTML::Mason::Tests->new( name => 'component',
					 description => 'Component object functionality' );


#------------------------------------------------------------

    $group->add_test( name => 'comp_obj',
		      path => 'comp_obj_test/comp_obj',
		      call_path => 'comp_obj_test/comp_obj',
		      description => 'Tests several component object methods',
		      component => <<'EOF',
<%def .subcomp>
% my $adj = 'happy';
I am a <% $adj %> subcomp.
<%args>
$crucial
$useless=>17
</%args>
</%def>

% my $anon = $m->interp->make_component(comp_text=>join("\n",'% my $adj = "flummoxed";','I am a <% $adj %> anonymous component.'),name=>'anonymous');

<% '-' x 60 %>

File-based:
<& /shared/display_comp_obj, comp=>$m->current_comp &>

<% '-' x 60 %>

Subcomponent:
<& /shared/display_comp_obj, comp=>$m->fetch_comp('.subcomp')  &>

<% '-' x 60 %>

Anonymous component:
<& $anon &>
<& $anon &>
<& /shared/display_comp_obj, comp=>$anon &>

<%args>
@animals=>('lions','tigers')
</%args>
EOF
		      expect => <<'EOF',


------------------------------------------------------------

File-based:
Declared args:
@animals=>('lions','tigers')

I am not a subcomponent.
I am file-based.
My short name is comp_obj.
My directory is /component/comp_obj_test.
I have 1 subcomponent(s).
Including one called .subcomp.
My title is /component/comp_obj_test/comp_obj.

My object file is /.../obj/component/comp_obj_test/comp_obj
My path is /component/comp_obj_test/comp_obj.
My comp_id is /component/comp_obj_test/comp_obj.
My source file is /.../comps/component/comp_obj_test/comp_obj
My source dir is /.../comps/component/comp_obj_test



------------------------------------------------------------

Subcomponent:
Declared args:
$crucial
$useless=>17

I am a subcomponent.
I am not file-based.
My short name is .subcomp.
My parent component is /component/comp_obj_test/comp_obj.
My directory is /component/comp_obj_test.
I have 0 subcomponent(s).
My title is /component/comp_obj_test/comp_obj:.subcomp.

My object file is /.../obj/component/comp_obj_test/comp_obj
My path is /component/comp_obj_test/comp_obj:.subcomp.



------------------------------------------------------------

Anonymous component:
I am a flummoxed anonymous component.
I am a flummoxed anonymous component.
Declared args:

I am not a subcomponent.
I am not file-based.
My short name is [anon something].
I have 0 subcomponent(s).
My title is [anon something].




EOF
		     );


#------------------------------------------------------------

    $group->add_test( name => 'context',
		      description => 'Tests list/scalar context propogation in comp calls',
		      component => <<'EOF',
Context checking:

List:\
% my $discard = [$m->comp('.subcomp')];


Scalar:\
% scalar $m->comp('.subcomp');


Scalar:\
<& .subcomp &>

<%def .subcomp>
% $m->out( wantarray ? ('an','array') : 'scalar' );
</%def>
EOF
		      expect => <<'EOF',
Context checking:

List:
anarray

Scalar:
scalar

Scalar:
scalar

EOF
		    );


#------------------------------------------------------------

    $group->add_test( name => 'scomp',
		      description => 'Test scomp Request method',
		      component => <<'EOF',

% my $text = $m->scomp('.subcomp', 1,2,3);
-----
<% $text %>

<%def .subcomp>
 Hello, you say <% join '', @_ %>.
</%def>
EOF
		      expect => <<'EOF',

-----

 Hello, you say 123.


EOF
		    );


#------------------------------------------------------------

    $group->add_test( name => 'mfu_count',
		      description => 'Test mfu_count component method',
		      component => <<'EOF',
<% $m->current_comp->mfu_count %>
% $m->current_comp->mfu_count(75);
<% $m->current_comp->mfu_count %>
EOF
		      expect => <<'EOF',
1
75
EOF
		    );

#------------------------------------------------------------

    $group->add_test( name => 'store',
                      description => 'Test store parameter to component call',
                      component => <<'EOF',

% my $buffy;
% my $rtn;
% $rtn = $m->comp({store => \$buffy}, '.subcomp', 1,2,3,4);
-----
<% $buffy %>
returned <% $rtn %>

<%def .subcomp>
 Hello, you say <% join '', @_ %>.
% return 'foo';
</%def>
EOF
                      expect => <<'EOF',

-----

 Hello, you say 1234.

returned foo

EOF
                    );

#------------------------------------------------------------

    $group->add_test( name => 'flush_clear',
		      description => 'Flush then clear',
		      component => <<'EOF',
Foo
% $m->flush_buffer;
Bar
% $m->clear_buffer;
Baz
EOF
		      expect => <<'EOF',
Foo
Baz
EOF
		    );

#------------------------------------------------------------

    $group->add_test( name => 'flush_clear_scomp',
		      description => 'Flush then clear inside scomp',
		      component => <<'EOF',
<%method s>
Foo
% $m->flush_buffer;
Bar
% $m->clear_buffer;
Baz
</%method>
This is me
----------
This is scomp-ed output:
<% $m->scomp('SELF:s') %>
----------
This is me again
EOF
		      expect => <<'EOF',
This is me
----------
This is scomp-ed output:

Foo
Baz

----------
This is me again
EOF
		    );

#------------------------------------------------------------

    $group->add_support( path => 'flush_clear_filter_comp',
			 component => <<'EOF',
Foo
% $m->flush_buffer;
Bar
% $m->clear_buffer;
Baz
<%filter>
s/^/-/gm;
</%filter>
EOF
		       );

#------------------------------------------------------------

    $group->add_test( name => 'flush_clear_filter',
		      description => 'Flush then clear with filter section',
		      component => <<'EOF',
before
<& flush_clear_filter_comp &>
after
EOF
		      expect => <<'EOF',
before
-Foo
-Baz

after
EOF
		    );

#------------------------------------------------------------

    $group->add_test( name => 'attr_if_exists',
		      description => 'Test attr_if_exists method',
		      component => <<'EOF',
have it: <% $m->base_comp->attr_if_exists('have_it') %>
don't have it: <% $m->base_comp->attr_if_exists('don\'t have_it') %>
<%attr>
have_it => 1
</%attr>
EOF
		      expect => <<'EOF',
have it: 1
don't have it: 0
EOF
		    );

#------------------------------------------------------------

    return $group;
}

