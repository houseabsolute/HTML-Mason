#!/usr/bin/perl -w

use strict;
use HTML::Mason::Tests;

my $tests = make_tests();
$tests->run;

sub make_tests
{
    my $group = HTML::Mason::Tests->new( name => 'subrequest',
					 description => 'subrequest-related features' );

#------------------------------------------------------------

    $group->add_support( path => '/support/subrequest_error_test',
			 component => <<'EOF',
<& /shared/display_req_obj &>
% die "whoops!";
EOF
		       );

#------------------------------------------------------------


    $group->add_support( path => '/support/dir/autohandler',
			 component => <<'EOF',
I am the autohandler.
EOF
		       );

#------------------------------------------------------------

    $group->add_support( path => '/support/dir/comp',
			 component => <<'EOF',
I am the called comp (no autohandler).
EOF
		       );

#------------------------------------------------------------

    $group->add_test( name => 'subrequest',
		      description => 'tests the official subrequest mechanism',
		      component => <<'EOF',
<%def .helper>
Executing subrequest
% my $buf;
% my $req = $m->make_subrequest(comp=>'/shared/display_req_obj', out_method => \$buf);
% $req->exec();
<% $buf %>
</%def>

Calling helper
<& .helper &>
EOF
		      expect => <<'EOF',

Calling helper

Executing subrequest
My depth is 1.

The top-level component is /shared/display_req_obj.

My stack looks like:
-----
/shared/display_req_obj
-----



EOF
		    );


#------------------------------------------------------------

    $group->add_test( name => 'subrequest_with_autohandler',
		      description => 'tests the subrequest mechanism with an autohandler',
		      component => <<'EOF',
Executing subrequest
% my $buf;
% my $req = $m->make_subrequest(comp=>'/subrequest/support/dir/comp', out_method => \$buf);
% $req->exec();
<% $buf %>
EOF
		      expect => <<'EOF',
Executing subrequest
I am the autohandler.
EOF
		    );


#------------------------------------------------------------

    $group->add_support( path => '/subrequest2/autohandler',
			 component => <<'EOF',
I am the autohandler for <% $m->base_comp->name %>.
% $m->call_next;
<%flags>
inherit => undef
</%flags>
EOF
		       );

#------------------------------------------------------------

    $group->add_support( path => '/subrequest2/bar',
			 component => <<'EOF',
I am bar.
EOF
		       );

#------------------------------------------------------------

    $group->add_test( name => 'subreq_exec_order',
		      path => '/subrequest2/subreq_exec_order',
		      call_path => '/subrequest2/subreq_exec_order',
		      description => 'Test that output from a subrequest comes out when we expect it to.',
		      component => <<'EOF',
% $m->subexec('/subrequest/subrequest2/bar');
I am subreq_exec_order.
EOF
		      expect => <<'EOF',
I am the autohandler for subreq_exec_order.
I am the autohandler for bar.
I am bar.
I am subreq_exec_order.
EOF
		    );

#------------------------------------------------------------

    $group->add_support( path => '/support/autoflush_subrequest',
			 component => <<'EOF',
here is the child
% $m->clear_buffer;
EOF
		       );

#------------------------------------------------------------

    $group->add_test( name => 'autoflush_subrequest',
		      description => 'make sure that a subrequest respects its parent autoflush setting',
		      interp_params => { autoflush => 1 },
		      component => <<'EOF',
My child says:
% $m->subexec('/subrequest/support/autoflush_subrequest');
EOF
		      expect => <<'EOF',
My child says:
here is the child
EOF
		    );

#------------------------------------------------------------

    $group->add_test( name => 'no_autoflush_subrequest',
		      description => 'make sure that a subrequest respects its parent autoflush setting',
		      interp_params => { autoflush => 0 },
		      component => <<'EOF',
My child says:
% $m->subexec('/subrequest/support/autoflush_subrequest');
EOF
		      expect => <<'EOF',
My child says:
EOF
		    );

#------------------------------------------------------------

    $group->add_support( path => '/support/return/scalar',
			 component => <<'EOF',
% die "wantarray should be false" unless defined(wantarray) and !wantarray;
% return 'foo';
EOF
		       );

#------------------------------------------------------------

    $group->add_test( name => 'return_scalar',
		      description => 'tests that exec returns scalar return value of top component',
		      component => <<'EOF',
% my $req = $m->make_subrequest(comp=>'/subrequest/support/return/scalar');
% my $value = $req->exec();
return value is <% $value %>
EOF
		      expect => <<'EOF',
return value is foo
EOF
		    );


#------------------------------------------------------------

    $group->add_support( path => '/support/return/list',
			 component => <<'EOF',
% die "wantarray should be true" unless wantarray;
% return (1, 2, 3);
EOF
		       );

#------------------------------------------------------------

    $group->add_test( name => 'return_list',
		      description => 'tests that exec returns list return value of top component',
		      component => <<'EOF',
% my $req = $m->make_subrequest(comp=>'/subrequest/support/return/list');
% my @value = $req->exec();
return value is <% join(",", @value) %>
EOF
		      expect => <<'EOF',
return value is 1,2,3
EOF
		    );


#------------------------------------------------------------

    $group->add_support( path => '/support/return/nothing',
			 component => <<'EOF',
wantarray is <% defined(wantarray) ? "defined" : "undefined" %>
EOF
		       );

#------------------------------------------------------------

    $group->add_test( name => 'return_nothing',
		      description => 'tests exec in non-return context',
		      component => <<'EOF',
% my $req = $m->make_subrequest(comp=>'/subrequest/support/return/nothing');
% $req->exec();
EOF
		      expect => <<'EOF',
wantarray is undefined
EOF
		    );


#------------------------------------------------------------

    $group->add_support( path => '/support/output',
			 component => <<'EOF',
More output
EOF
		       );

#------------------------------------------------------------

    $group->add_test( name => 'kwindla',
		      description => 'tests bug report from Kwindla Kramer',
		      component => <<'EOF',
Some output
% $m->clear_buffer;
% my $req = $m->make_subrequest( comp => '/subrequest/support/output' );
% $req->exec();
% $m->flush_buffer;
% $m->abort;
EOF
		      expect => <<'EOF',
More output
EOF
		    );


#------------------------------------------------------------

    return $group;
}
