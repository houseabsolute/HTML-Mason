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

% my $anon = $m->interp->make_anonymous_component(comp=>join("\n",'% my $adj = "flummoxed";','I am a <% $adj %> anonymous component.'),name=>'anonymous');

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

This is my first time.
I am not a subcomponent.
I am file-based.
My short name is comp_obj.
My directory is /component/comp_obj_test.
I have run 1 time(s).
I have 1 subcomponent(s).
Including one called .subcomp.
My title is /component/comp_obj_test/comp_obj.

My cache file is /.../cache/component+2fcomp_obj_test+2fcomp_obj
My object file is /.../obj/component/comp_obj_test/comp_obj
My path is /component/comp_obj_test/comp_obj.
My fq_path is /component/comp_obj_test/comp_obj.
My source file is /.../comps/component/comp_obj_test/comp_obj
My source dir is /.../comps/component/comp_obj_test



------------------------------------------------------------

Subcomponent:
Declared args:
$crucial
$useless=>17

This is my first time.
I am a subcomponent.
I am not file-based.
My short name is .subcomp.
My parent component is /component/comp_obj_test/comp_obj.
My directory is /component/comp_obj_test.
I have run 0 time(s).
I have 0 subcomponent(s).
My title is /component/comp_obj_test/comp_obj:.subcomp.

My cache file is /.../cache/component+2fcomp_obj_test+2fcomp_obj
My object file is /.../obj/component/comp_obj_test/comp_obj
My path is /component/comp_obj_test/comp_obj:.subcomp.



------------------------------------------------------------

Anonymous component:
I am a flummoxed anonymous component.
I am a flummoxed anonymous component.
Declared args:

This is not my first time.
I am not a subcomponent.
I am not file-based.
My short name is [anon something].
I have run 2 time(s).
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

    return $group;
}

