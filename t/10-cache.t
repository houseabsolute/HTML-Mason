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

eval { require Cache::FileCache };
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


#------------------------------------------------------------

    $group->add_support( path => 'support/cache_test',
			 component => <<'EOF',
<% $result %>
This was<% $cached ? '' : ' not' %> cached.

<%init>
my $cached = 0;
my $result;
my $return;
unless ($result = $m->cache->get('fandango')) {
    $result = "Hello Dolly.";
    $return = $m->cache->set('fandango', $result) || '';
} else {
    $cached = 1;
}
</%init>
EOF
		       );


#------------------------------------------------------------

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


Hello Dolly.
This was cached.


Hello Dolly.
This was cached.


EOF
		    );


#------------------------------------------------------------

    $group->add_test( name => 'keys',
		      description => q|test multiple keys and $m->cache->get_keys|,
		      component => <<'EOF',
<%init>
foreach my $key (qw(foo bar baz)) {
    $m->cache->set($key, $key);
}
my @keys = sort $m->cache->get_keys;
$m->print("keys in cache: ".join(",",@keys)."\n");
foreach my $key (qw(foo bar baz)) {
    my $value = $m->cache->get($key) || "undefined";
    $m->print("value for $key is $value\n");
}
$m->cache->remove('foo');
$m->cache->remove('bar');
$m->print("expiring foo and bar...\n");
foreach my $key (qw(foo bar baz)) {
    my $value = $m->cache->get($key) || "undefined";
    $m->print("value for $key is $value\n");
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

#------------------------------------------------------------

    $group->add_support ( path => 'support/cache_self',
			  component => <<'EOF',
x is <% $x %>
<%args>
$x
</%args>
<%init>
return if $m->cache_self;
</%init>
EOF
			);

#------------------------------------------------------------

    $group->add_test( name => 'cache_self',
		      description => 'test $m->cache_self',
		      component => <<'EOF',
<& support/cache_self, x => 1 &>
<& support/cache_self, x => 99 &>
EOF
		      expect => <<'EOF',
x is 1

x is 1
EOF
		    );

#------------------------------------------------------------

    $group->add_support ( path => 'support/cache_self_expires',
			  component => <<'EOF',
x is <% $x %>
<%args>
$x
</%args>
<%init>
return if $m->cache_self( expires_in => '1s' );
</%init>
EOF
			);

#------------------------------------------------------------

    $group->add_test( name => 'cache_self_expiration',
		      description => 'test that $m->cache_self respects expires_in parameter',
		      component => <<'EOF',
<& support/cache_self_expires, x => 1 &>
% sleep 3;
<& support/cache_self_expires, x => 99 &>
EOF
		      expect => <<'EOF',
x is 1

x is 99
EOF
		    );

#------------------------------------------------------------

    $group->add_support ( path => 'support/cache_self_with_key',
			  component => <<'EOF',
x is <% $x %>
<%args>
$x
$key
</%args>
<%init>
return if $m->cache_self( key => $key );
</%init>
EOF
			);

#------------------------------------------------------------

    $group->add_test( name => 'cache_self_key',
		      description => 'test $m->cache_self with a key',
		      component => <<'EOF',
<& support/cache_self_with_key, x => 1, key => 1 &>
<& support/cache_self_with_key, x => 99, key => 99 &>
<& support/cache_self_with_key, x => 1000, key => 1 &>
EOF
		      expect => <<'EOF',
x is 1

x is 99

x is 1
EOF
		    );

#------------------------------------------------------------

    $group->add_support ( path => 'support/cache_self_and_die',
			  component => <<'EOF',
<%init>
return if $m->cache_self;
die "argh!";
</%init>
EOF
			);

#------------------------------------------------------------

    $group->add_test( name => 'cache_self_error',
		      description => 'test $m->cache_self with an error to make sure errors are propogated',
		      component => <<'EOF',
<& support/cache_self_and_die, x => 1, key => 1 &>
EOF
		      expect_error => qr/argh! at .*/,
		    );

#------------------------------------------------------------

    $group->add_test( name => 'cache_self_scomp',
                      description => 'make sure that $m->cache_self cooperates with $m->scomp',
                      component => <<'EOF',
<% $m->scomp( 'support/cache_self', x => 1 ) %>
<% $m->scomp( 'support/cache_self', x => 99 ) %>
EOF
                      expect => <<'EOF',
x is 1

x is 1
EOF
                    );

#------------------------------------------------------------

    $group->add_support ( path => 'support/cache_self_filtered',
			  component => <<'EOF',
x is <% $x %>
<%args>
$x
$key => 1
</%args>
<%init>
return if $m->cache_self( key => $key );
</%init>
<%filter>
$_ = uc $_;
</%filter>
EOF
			);

#------------------------------------------------------------

    $group->add_test( name => 'cache_self_filtered',
		      description => 'test $m->cache_self with a filter block',
		      component => <<'EOF',
<& support/cache_self_filtered, x => 1 &>
<& support/cache_self_filtered, x => 99 &>
EOF
		      expect => <<'EOF',
X IS 1

X IS 1
EOF
		    );

#------------------------------------------------------------

    $group->add_test( name => 'cache_self_filtered_scomp',
		      description => 'test $m->cache_self with a filter block callled via $m->scomp',
		      component => <<'EOF',
<% $m->scomp( 'support/cache_self_filtered', key => 2, x => 1 ) %>
<% $m->scomp( 'support/cache_self_filtered', key => 2, x => 99 ) %>
EOF
		      expect => <<'EOF',
X IS 1

X IS 1
EOF
		    );

#------------------------------------------------------------

    return $group;
}

