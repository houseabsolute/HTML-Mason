#!/usr/bin/perl -w

use strict;

use HTML::Mason::Tests;

my $tests = make_tests();
$tests->run;

sub make_tests
{
    my $group = HTML::Mason::Tests->new( name => 'request',
					 description => 'request object functionality' );

    $group->add_support( path => '/support/abort_test',
			 component => <<'EOF',
Some more text

% $m->abort(50);

But this will never be seen
EOF
		       );

    $group->add_support( path => '/support/display_req_obj',
			 component => <<'EOF',
My depth is <% $m->depth %>.

The top-level component is <% $m->callers(-1)->title %>.

My stack looks like:
-----
% foreach my $comp ($m->callers) {
<% $comp->title %>
% }
-----

EOF
		       );

    $group->add_support( path => '/support/subrequest_error_test',
			 component => <<'EOF',
<& display_req_obj &>
% die "whoops!";
EOF
		       );

    $group->add_support( path => '/sections/perl',
			 component => <<'EOF',
foo
EOF
		       );

    $group->add_support( path => '/support/various_test',
			 component => <<'EOF',
Caller is <% $m->caller->title %> or <% $m->callers(1)->title %>.
The top level component is <% $m->callers(-1)->title %>.
The full component stack is <% join(",",map($_->title,$m->callers)) %>.
My argument list is (<% join(",",$m->caller_args(0)) %>).

% foreach my $path (qw(various_test /request/sections/perl foobar /shared)) {
%   my $full_path = $m->process_comp_path($path);
Trying to fetch <% $path %> (full path <% $full_path %>):
%   if ($m->comp_exists($path)) {
%     if (my $comp = $m->fetch_comp($path)) {
<% $path %> exists with title <% $comp->title %>.
%     } else {
<% $path %> exists but could not fetch object!
%     }
%   } else {
<% $path %> does not exist.
%   }
% }

% $m->out("Output via the out function.");

/request/file outputs <% int(length($m->scomp("/request/file"))/10) %>0+ characters.

% my $diff = time-($m->time);
% if ($diff <= 2) {
No time difference.
% } else {
Time difference!
% }



EOF
		       );

    $group->add_test( name => 'abort',
		      description => 'test $m->abort method',
		      component => <<'EOF',
Some text

% eval {$m->comp('support/abort_test')};
% if (my $err = $@) {
%   if ($m->aborted) {
Component aborted with value <% $m->aborted_value %>
%   } else {
Got error
%   }
% }
EOF
		      expect => <<'EOF',
Some text

Some more text

Component aborted with value 50
EOF
		    );

    $group->add_test( name => 'file',
		      description => 'tests $m->file method',
		      component => <<'EOF',
Now I will print myself:

<% $m->file("file") %>
EOF
		      expect => <<'EOF',
Now I will print myself:

Now I will print myself:

<% $m->file("file") %>

EOF
		    );

    $group->add_test( name => 'list_out',
		      description => 'tests that $m->out can handle a list of arguments',
		      component => <<'EOF',
Sending list of arguments:

<% 'blah','boom','bah' %>

<%perl>
 $m->out(3,4,5);
</%perl>
EOF
		      expect => <<'EOF',
Sending list of arguments:

blahboombah

345
EOF
		    );

    $group->add_test( name => 'req_obj',
		      description => 'tests various operations such as $m->out, comp calls, $m->current_comp',
		      component => <<'EOF',
<%def .subcomp>
% if ($count < 5) {
<& $m->current_comp, count=>$count+1 &>
% } else {
<& support/display_req_obj &>
% }
<%args>
$count
</%args>
</%def>

<% '-' x 60 %>

One level request:
<& support/display_req_obj &>

<% '-' x 60 %>

Many level request:
<& .subcomp, count=>0 &>

<% '-' x 60 %>
EOF
		      expect => <<'EOF',

------------------------------------------------------------

One level request:
My depth is 2.

The top-level component is /request/req_obj.

My stack looks like:
-----
/request/support/display_req_obj
/request/req_obj
-----



------------------------------------------------------------

Many level request:






My depth is 8.

The top-level component is /request/req_obj.

My stack looks like:
-----
/request/support/display_req_obj
/request/req_obj:.subcomp
/request/req_obj:.subcomp
/request/req_obj:.subcomp
/request/req_obj:.subcomp
/request/req_obj:.subcomp
/request/req_obj:.subcomp
/request/req_obj
-----









------------------------------------------------------------
EOF
		    );

    $group->add_test( name => 'subrequest',
		      description => 'tests a provisional subrequest mechanism (Jon will explain)',
		      component => <<'EOF',
<%def .helper>
% my $interp = $m->interp;
Executing subrequest
% # This is still unsupported but will likely be official later
% my $buf;
% my $req = new HTML::Mason::Request (interp=>$interp, out_method=>\$buf);
% $req->exec('/request/support/display_req_obj');
<% $buf %>
</%def>

Calling helper
<& .helper &>
EOF
		      expect => <<'EOF',

Calling helper

Executing subrequest
My depth is 1.

The top-level component is /request/support/display_req_obj.

My stack looks like:
-----
/request/support/display_req_obj
-----



EOF
		    );

    $group->add_test( name => 'subrequest_error',
		      description => 'check error handling for provision subrequest mechanism',
		      component => <<'EOF',
<%def .helper>
% my $interp = $m->interp;
% $interp->exec('/request/support/subrequest_error_test');
</%def>

Calling helper
% eval {$m->comp('.helper')};
% my $error = $@;
<& /shared/check_error, error=>$error, lines=>2 &>

% if ($error) {
Back from error, checking request state:
<& support/display_req_obj &>
% }
EOF
		      expect => <<'EOF',

Calling helper

Error: error while executing /request/support/subrequest_error_test:
whoops! 


Back from error, checking request state:
My depth is 2.

The top-level component is /request/subrequest_error.

My stack looks like:
-----
/request/support/display_req_obj
/request/subrequest_error
-----


EOF
		    );

    $group->add_test( name => 'various',
		      description => 'tests caller, callers, fetch_comp, process_comp_path, comp_exists and scomp',
		      component => <<'EOF',
<& support/various_test, junk=>6 &>
EOF
		      expect => <<'EOF',
Caller is /request/various or /request/various.
The top level component is /request/various.
The full component stack is /request/support/various_test,/request/various.
My argument list is (junk,6).

Trying to fetch various_test (full path /request/support/various_test):
various_test exists with title /request/support/various_test.
Trying to fetch /request/sections/perl (full path /request/sections/perl):
/request/sections/perl exists with title /request/sections/perl.
Trying to fetch foobar (full path /request/support/foobar):
foobar does not exist.
Trying to fetch /shared (full path /shared):
/shared does not exist.

Output via the out function.
/request/file outputs 70+ characters.

No time difference.




EOF
		    );

    return $group;
}
