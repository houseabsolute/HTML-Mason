#!/usr/bin/perl -w

use strict;

use HTML::Mason::Tests;

my $tests = make_tests();
$tests->run;

sub make_tests
{
    my $group = HTML::Mason::Tests->new( name => 'syntax',
					 description => 'Basic component syntax tests' );


#------------------------------------------------------------

    $group->add_support( path => '/support/amper_test',
			 component => <<'EOF',
amper_test.<p>
% if (%ARGS) {
Arguments:<p>
%   foreach my $key (sort keys %ARGS) {
<b><% $key %></b>: <% $ARGS{$key} %><br>
%   }
% }
EOF
		       );


#------------------------------------------------------------

    $group->add_test( name => 'ampersand syntax',
		      description => 'tests all variations of component call path syntax and arg passing',
		      component => <<'EOF',
<HTML>
<HEAD>
<TITLE>
amper Test
</TITLE>
</HEAD>
<BODY>
<& support/amper_test &>
<& /syntax/support/amper_test, message=>'Hello World!'  &>
<& support/amper_test, message=>'Hello World!', to=>'Joe' &>
<& "support/amper_test" &>
% my $dir = "support";
% my %args = (a=>17, b=>32);
<& $dir . "/amper_test", %args &>
</BODY>
</HTML>
EOF
		      expect => <<'EOF',
<HTML>
<HEAD>
<TITLE>
amper Test
</TITLE>
</HEAD>
<BODY>
amper_test.<p>

amper_test.<p>
Arguments:<p>
<b>message</b>: Hello World!<br>

amper_test.<p>
Arguments:<p>
<b>message</b>: Hello World!<br>
<b>to</b>: Joe<br>

amper_test.<p>

amper_test.<p>
Arguments:<p>
<b>a</b>: 17<br>
<b>b</b>: 32<br>

</BODY>
</HTML>
EOF
		 );


#------------------------------------------------------------

    $group->add_test( name => 'replace',
		      description => 'tests <% %> tag',
		      component => <<'EOF',
<HTML>
<HEAD>
<TITLE>
Replacement Test
</TITLE>
</HEAD>
<BODY>
<% "Hello World!" %>
</BODY>
</HTML>
EOF
		      expect => <<'EOF',
<HTML>
<HEAD>
<TITLE>
Replacement Test
</TITLE>
</HEAD>
<BODY>
Hello World!
</BODY>
</HTML>
EOF
		    );


#------------------------------------------------------------

	$group->add_test( name => 'percent',
			  description => 'tests %-line syntax',
			  component => <<'EOF',
<HTML>
<HEAD>
<TITLE>
Percent Test
</TITLE>
</HEAD>
<BODY>
% my $message = "Hello World!";
<% $message %>
</BODY>
</HTML>
EOF
			  expect => <<'EOF',
<HTML>
<HEAD>
<TITLE>
Percent Test
</TITLE>
</HEAD>
<BODY>
Hello World!
</BODY>
</HTML>
EOF
			);

#------------------------------------------------------------

	$group->add_test( name => 'fake_percent',
			  description => 'tests % in text section',
			  component => 'some text, a %, and some text',
			  expect =>    'some text, a %, and some text',
			);

    return $group;
}
