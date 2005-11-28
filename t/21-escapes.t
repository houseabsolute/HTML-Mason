#!/usr/bin/perl -w

use strict;

use Test::More tests => 3;

use HTML::Mason::Escapes;

my $html = qq|<>"& \x{2202}|;
HTML::Mason::Escapes::basic_html_escape( \$html );
is( $html, "&lt;&gt;&quot;&amp; \x{2202}",
    'test basic HTML escape' );

my $html2 = qq|<>"& \x{2202}\x{20a5}|;
HTML::Mason::Escapes::html_entities_escape( \$html2 );
is( $html2, "&lt;&gt;&quot;&amp; &part;&#x20A5;",
    'test HTML::Entities escape' );

my $url = qq|"=\x{2202}|;
HTML::Mason::Escapes::url_escape( \$url );
is( $url, '%22%3D%E2%88%82',
    'test url escape' );
