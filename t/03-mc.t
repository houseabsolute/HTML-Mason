#!/usr/bin/perl -w

use strict;

use HTML::Mason::Tests;

my $tests = make_tests();
$tests->run;

sub make_tests
{
    my $group = HTML::Mason::Tests->new( name => 'mc',
					 description => 'test mc_* style command compatibility' );


#------------------------------------------------------------

    $group->add_support( path => 'support/mc_various_test',
			 component => <<'EOF',
Caller is <% mc_caller %>.
The full component stack is <% join(",",mc_comp_stack) %>.

% foreach my $comp (qw(mc_various_test /mc/mc_various foobar /shared)) {
%   if (mc_comp_exists($comp)) {
%     my $content = mc_file(mc_comp_source($comp));
%     if (length($content) > 20) {
Component <% $comp %> exists and has content.
%     } else {
Component <% $comp %> exists but has no content!!!
%     }
%   } else {
Component <% $comp %> does not exist.
%   }
% }

<%perl>
my $t1 = mc_time();
sleep(2);
my $t2 = mc_time();
my $diff = $t2-$t1;
</%perl>
% if ($diff >= 1 and $diff <= 3) {
Time difference is approximately 2 seconds.
% } else {
Time difference is <% $diff %> seconds!
% }
</%perl>
EOF
		       );


#------------------------------------------------------------

    $group->add_test( name => 'mc_various',
		      description => 'mc_* commands',
		      component => <<'EOF',
<& support/mc_various_test &>
EOF
		      expect => <<'EOF'
Caller is /mc/mc_various.
The full component stack is /mc/support/mc_various_test,/mc/mc_various.

Component mc_various_test exists and has content.
Component /mc/mc_various exists and has content.
Component foobar does not exist.
Component /shared does not exist.


Time difference is approximately 2 seconds.
</%perl>

EOF
		    );


#------------------------------------------------------------

    return $group;
}
