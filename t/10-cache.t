#!/usr/bin/perl -w

use strict;

use HTML::Mason::Tests;

# Skip if flock not implemented.
eval { my $fh = do { local *FH; *FH; }; open $fh, $0; flock $fh,1; };
if ($@)
{
    print "1..0\n";
    exit;
}

my $tests = make_tests();
$tests->run;

sub make_tests
{
    my $group = HTML::Mason::Tests->new( name => 'cache',
					 description => 'Test caching' );

    $group->add_support( path => 'support/cache_test',
			 component => <<'EOF',
<% $result %>
This was<% $cached ? '' : ' not' %> cached.
Return value: <% $return %>

<%init>
my $cached = 0;
my $result;
my $return;
unless ($result = $m->cache(key=>'fandango')) {
    $result = "Hello Dolly.";
    $return = $m->cache(action=>'store', key=>'fandango', value=>$result) || '';
} else {
    $cached = 1;
}
</%init>
EOF
		       );

    $group->add_test( name => 'cache',
		      description => 'basic caching functionality',
		      component => <<'EOF',
% for (my $i=0; $i<3; $i++) {
<& support/cache_test &>
% }
EOF
		      expect => <<'EOF',
Hello Dolly.
This was not cached.
Return value: Hello Dolly.


Hello Dolly.
This was cached.
Return value: 


Hello Dolly.
This was cached.
Return value: 


EOF
		    );

    $group->add_support( path => 'support/cache_self_test',
			 component => <<'EOF',
Hello World! var = <% $var %>
<%init>
return if $m->cache_self(key=>'fandango');
</%init>
<%args>
$var
</%args>

EOF
		       );

    $group->add_test( name => 'cache_self',
		      description => 'cache_self functionality',
		      component => <<'EOF',
% my $var = 1;
% for (my $i=0; $i<3; $i++) {
<% $m->comp('support/cache_self_test',var=>$var) %>
% $var++;
% }
EOF
		      expect => <<'EOF',
Hello World! var = 1


Hello World! var = 1


Hello World! var = 1


EOF
		    );

    $group->add_test( name => 'keys',
		      description => q|test $m->cache( action => 'keys' )|,
		      component => <<'EOF',
<%init>
foreach my $key (qw(foo bar baz)) {
    $m->cache(action=>'store',key=>$key,value=>$key);
}
my @keys = sort $m->cache(action=>'keys');
$m->out("keys in cache: ".join(",",@keys)."\n");
foreach my $key (qw(foo bar baz)) {
    my $value = $m->cache(key=>$key) || "undefined";
    $m->out("value for $key is $value\n");
}
$m->cache(action=>'expire', key=>[qw(foo bar)]);
$m->out("expiring foo and bar...\n");
foreach my $key (qw(foo bar baz)) {
    my $value = $m->cache(key=>$key) || "undefined";
    $m->out("value for $key is $value\n");
}
</%init>
EOF
		      expect => <<'EOF',
keys in cache: bar,baz,foo
value for foo is foo
value for bar is bar
value for baz is baz
expiring foo and bar...
value for foo is undefined
value for bar is undefined
value for baz is baz
EOF
		    );

    return $group;
}
