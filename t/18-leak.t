#!/usr/bin/perl -w
use HTML::Mason::Tests;
use strict;

BEGIN
{
    if ($] < 5.006)
    {
        print "1..0\n";
        exit;
    }
    else
    {
        require Scalar::Util;
        unless ( defined &Scalar::Util::weaken )
        {
            print "Your installation of Scalar::Util does not include the weaken subroutine\n";
            print "1..0\n";
            exit;
        }
    }
}

my $tests = make_tests();
$tests->run;

{
    package InterpWatcher;
    my $destroy_count = 0;
    
    use base qw(HTML::Mason::Interp);
    sub DESTROY { $destroy_count++ }
    sub destroy_count   { $destroy_count   }
}

{
    package RequestWatcher;
    my $destroy_count = 0;
    
    use base qw(HTML::Mason::Request);
    sub DESTROY { $destroy_count++ }
    sub destroy_count   { $destroy_count   }
}

sub make_tests
{
    my $group = HTML::Mason::Tests->tests_class->new( name => '18-leak.t',
						      description => 'Tests that various memory leaks are no longer with us' );

    $group->add_test( name => 'interp_destroy',
                      description => 'Test that interps with components in cache still get destroyed',
		      component => <<'EOF',
<%perl>
{ 
    my $interp = InterpWatcher->new();
    my $comp = $interp->make_component( comp_source => 'foo' );
}
$m->print("destroy_count = " . InterpWatcher->destroy_count . "\n");

{
    my $interp = InterpWatcher->new();
    my $comp = $interp->make_component( comp_source => 'foo' );
}
$m->print("destroy_count = " . InterpWatcher->destroy_count . "\n");
</%perl>
EOF
                      expect => <<'EOF',
destroy_count = 1
destroy_count = 2
EOF
                    );

#------------------------------------------------------------

    $group->add_support( path => '/support/no_error_comp',
			 component => <<'EOF',
No error here.
EOF
		       );

#------------------------------------------------------------

    $group->add_support( path => '/support/error_comp',
			 component => <<'EOF',
<%
EOF
		       );

#------------------------------------------------------------

    $group->add_test( name => 'request_destroy',
                      description => 'Test that requests get destroyed after top-level component error',
                      interp_params => { request_class => 'RequestWatcher' },
		      component => <<'EOF',
<%perl>
eval { $m->subexec('support/no_error_comp') };
$m->print("destroy_count = " . RequestWatcher->destroy_count . "\n");
eval { $m->subexec('support/error_comp') };
$m->print("destroy_count = " . RequestWatcher->destroy_count . "\n");
eval { $m->subexec('support/not_found_comp') };
$m->print("destroy_count = " . RequestWatcher->destroy_count . "\n");
</%perl>
EOF
                      expect => <<'EOF',
No error here.
destroy_count = 1
destroy_count = 2
destroy_count = 3
EOF
                    );

#------------------------------------------------------------

    return $group;
}
