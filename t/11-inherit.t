#!/usr/bin/perl -w

use strict;

use HTML::Mason::Tests;

my $tests = make_tests();
$tests->run;

sub make_tests
{
    my $group = HTML::Mason::Tests->new( name => 'inherit',
					 description => 'Test inheritance' );


#------------------------------------------------------------

    $group->add_support( path => 'autohandler',
			 component => <<'EOF',
<%method m1>m1 from level 1</%method>
<%method m12>m12 from level 1</%method>
<%method m13>m13 from level 1</%method>
<%method m123>m123 from level 1</%method>

<%attr>
a1=>'a1 from level 1'
a12=>'a12 from level 1'
a13=>'a13 from level 1'
a123=>'a123 from level 1'
</%attr>

<& variants &>

<% $m->call_next %>

EOF
		       );


#------------------------------------------------------------

    $group->add_support( path => 'report_parent',
			 component => <<'EOF',
% my $comp = $m->callers(1);
My name is <% $comp->path %> and <% $comp->parent ? "my parent is ".$comp->parent->path : "I have no parent" %>.
EOF
		       );


#------------------------------------------------------------

    $group->add_support( path => 'variants',
			 component => <<'EOF',
% my @variants = qw(1 2 3 12 13 23 123);

Methods (called from <% $m->callers(1)->title %>)
% foreach my $v (@variants) {
%   if ($self->method_exists("m$v")) {
m<% $v %>: <& "SELF:m$v" &>
%   } else {
m<% $v %>: does not exist
%   }
% }

Attributes (referenced from <% $m->callers(1)->title %>)
% foreach my $v (@variants) {
%   if ($self->attr_exists("a$v")) {
a<% $v %>: <% $self->attr("a$v") %>
%   } else {
a<% $v %>: does not exist
%   }
% }

<%init>
my $self = $m->base_comp;
</%init>
EOF
		       );


#------------------------------------------------------------

    $group->add_support( path => 'subdir/autohandler',
			 component => <<'EOF',
<%method m2>m2 from level 2</%method>
<%method m12>m12 from level 2</%method>
<%method m23>m23 from level 2</%method>
<%method m123>m123 from level 2</%method>

<%attr>
a2=>'a2 from level 2'
a12=>'a12 from level 2'
a23=>'a23 from level 2'
a123=>'a123 from level 2'
</%attr>

<& ../variants &>

<% $m->call_next %>

<%init>
my $self = $m->base_comp;
</%init>
EOF
		       );


#------------------------------------------------------------

    $group->add_test( name => 'bypass',
		      description => 'test inheritance that skips one autohandler',
		      path => 'subdir/bypass',
		      call_path => 'subdir/bypass',
		      component => <<'EOF',
<%method m3>m3 from level 3</%method>
<%method m13>m13 from level 3</%method>
<%method m23>m23 from level 3</%method>
<%method m123>m123 from level 3</%method>

<%attr>
a3=>'a3 from level 3'
a13=>'a13 from level 3'
a23=>'a23 from level 3'
a123=>'a123 from level 3'
</%attr>

<& ../report_parent &>

<%flags>
inherit=>'../autohandler'
</%flags>
EOF
		      expect => <<'EOF',



Methods (called from /inherit/autohandler)
m1: m1 from level 1
m2: does not exist
m3: m3 from level 3
m12: m12 from level 1
m13: m13 from level 3
m23: m23 from level 3
m123: m123 from level 3

Attributes (referenced from /inherit/autohandler)
a1: a1 from level 1
a2: does not exist
a3: a3 from level 3
a12: a12 from level 1
a13: a13 from level 3
a23: a23 from level 3
a123: a123 from level 3





My name is /inherit/subdir/bypass and my parent is /inherit/autohandler.




EOF
		    );


#------------------------------------------------------------

    $group->add_test( name => 'ignore',
		      description => 'turning off inheritance',
		      path => 'subdir/ignore',
		      call_path => 'subdir/ignore',
		      component => <<'EOF',
<%method m3>m3 from level 3</%method>
<%method m13>m13 from level 3</%method>
<%method m23>m23 from level 3</%method>
<%method m123>m123 from level 3</%method>

<%attr>
a3=>'a3 from level 3'
a13=>'a13 from level 3'
a23=>'a23 from level 3'
a123=>'a123 from level 3'
</%attr>

<& ../variants &>

<& ../report_parent &>

<%flags>
inherit=>undef
</%flags>
EOF
		      expect => <<'EOF',



Methods (called from /inherit/subdir/ignore)
m1: does not exist
m2: does not exist
m3: m3 from level 3
m12: does not exist
m13: m13 from level 3
m23: m23 from level 3
m123: m123 from level 3

Attributes (referenced from /inherit/subdir/ignore)
a1: does not exist
a2: does not exist
a3: a3 from level 3
a12: does not exist
a13: a13 from level 3
a23: a23 from level 3
a123: a123 from level 3



My name is /inherit/subdir/ignore and I have no parent.


EOF
		    );


#------------------------------------------------------------

    $group->add_test( name => 'normal',
		      description => 'normal inheritance path',
		      path => 'subdir/normal',
		      call_path => 'subdir/normal',
		      component => <<'EOF',
<%method m3>m3 from level 3</%method>
<%method m13>m13 from level 3</%method>
<%method m23>m23 from level 3</%method>
<%method m123>m123 from level 3</%method>

<%attr>
a3=>'a3 from level 3'
a13=>'a13 from level 3'
a23=>'a23 from level 3'
a123=>'a123 from level 3'
</%attr>

<& ../report_parent &>
EOF
		      expect => <<'EOF',



Methods (called from /inherit/autohandler)
m1: m1 from level 1
m2: m2 from level 2
m3: m3 from level 3
m12: m12 from level 2
m13: m13 from level 3
m23: m23 from level 3
m123: m123 from level 3

Attributes (referenced from /inherit/autohandler)
a1: a1 from level 1
a2: a2 from level 2
a3: a3 from level 3
a12: a12 from level 2
a13: a13 from level 3
a23: a23 from level 3
a123: a123 from level 3






Methods (called from /inherit/subdir/autohandler)
m1: m1 from level 1
m2: m2 from level 2
m3: m3 from level 3
m12: m12 from level 2
m13: m13 from level 3
m23: m23 from level 3
m123: m123 from level 3

Attributes (referenced from /inherit/subdir/autohandler)
a1: a1 from level 1
a2: a2 from level 2
a3: a3 from level 3
a12: a12 from level 2
a13: a13 from level 3
a23: a23 from level 3
a123: a123 from level 3





My name is /inherit/subdir/normal and my parent is /inherit/subdir/autohandler.





EOF
		    );


#------------------------------------------------------------

    return $group;
}

