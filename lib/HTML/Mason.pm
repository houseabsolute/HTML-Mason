package HTML::Mason;
# Copyright (c) 1998-2002 by Jonathan Swartz. All rights reserved.
# This program is free software; you can redistribute it and/or modify it
# under the same terms as Perl itself.

use 5.005;

$HTML::Mason::VERSION = '1.09_01';
$HTML::Mason::VERSION = eval $HTML::Mason::VERSION

use HTML::Mason::Interp;

sub version
{
    return $HTML::Mason::VERSION;
}

1;

__END__

=head1 NAME

Mason - High-performance, dynamic web site authoring system

=head1 SYNOPSIS

    PerlModule HTML::Mason::ApacheHandler

    <FilesMatch "\.html$">
        SetHandler perl-script
        PerlHandler HTML::Mason::ApacheHandler
    </FilesMatch>

=head1 DESCRIPTION

Mason is a tool for building, serving and managing large web
sites. Its features make it an ideal backend for high load sites
serving dynamic content, such as online newspapers or database driven
e-commerce sites.

Actually, Mason can be used to generate any sort of text, whether for
a web site or not.  But it was originally built for web sites and
since that's why most people are interested in it, that is the focus
of this documentation.

Mason's various pieces revolve around the notion of "components''. A
component is a mix of HTML, Perl, and special Mason commands, one
component per file. So-called "top-level" components represent entire
web-pages, while smaller components typically return HTML snippets for
embedding in top-level components. This object-like architecture
greatly simplifies site maintenance: change a shared component, and
you instantly changed all dependant pages that refer to it across a
site (or across many virtual sites).

Mason's component syntax lets designers separate a web page into
programmatic and design elements. This means the esoteric Perl bits
can be hidden near the bottom of a component, preloading simple
variables for use above in the HTML. In our own experience, this frees
content managers (i.e., non-programmers) to work on the layout without
getting mired in programming details. Techies, however, still enjoy
the full power of Perl.

One of the best ways to learn about Mason is to explore the samples/
directory created during installation. There you will find a
collection of components, simple to complex, illustrating most of
Mason's component syntax.

Other Mason features include:

=over

=item Caching

Mason caches components after compilation, and offers an internal,
shared, expireable data cache for use by components themselves.

=item Templating

The autohandler and dhandler features make it easy to apply a common
header, footer, or filtering function to an entire directory or
hierarchy of pages.

=item Previewer

Mason includes a powerful previewing utility which acts as a proxy
between Mason and incoming requests. Using a web interface, users
create "port profiles" that massage requests in all kinds of
interesting ways: different browsers may be simulated, different
request ports, different times of day, etc.

=item Standalone Use

Mason is optimized for use with mod_perl, but can also be used from
CGI or as a standalone tool to generate other types of dynamic text
files (similar to Text::Template, ePerl, etc.). To learn more about
this option see the STANDALONE MODE section in
B<L<HTML::Mason::Interp|HTML::Mason::Interp>>.

=back

Mason works by intercepting innocent-looking requests (say,
http://www.yoursite.com/index.html) and mapping them to requests for
Mason components.  Mason then compiles the component, runs it, and
feeds the output back to the client.

Consider this simple Mason component:

    % my $noun = 'World';
    Hello <% $noun %>!
    How are ya?

The output of this component is:

    Hello World!
    How are ya?

In this component you see a mix of standard HTML and Mason
elements. The bare '%' prefixing the first line tells Mason that this
is a line of Perl code. One line below, the embedded S<E<lt>%
... %E<gt>> tag gets replaced with the return value of its contents,
evaluated as a Perl expression.

Beyond this trivial example, components can also embed serious chunks
of Perl code (say, to pull records from a database). They can also
call other components, cache results for later reuse, and perform all
the tricks you expect from a regular Perl program. See
B<L<HTML::Mason::Devel|HTML::Mason::Devel>> for a full tutorial on building, using and
debugging Mason components.

=head1 INSTALLATION

Mason has been tested under Linux, FreeBSD, Solaris, HPUX, and
Win32. As an all-Perl solution, it should work on any machine that has
working versions of Perl 5.00503+, mod_perl, and the required CPAN
modules.

Mason has a standard MakeMaker-driven installation. See the README
file for details.

=head1 CONFIGURING MASON

This section assumes that you are able to install and configure a
mod_perl server. Relevant documentation is available at
http://www.apache.org (Apache) and http://perl.apache.org
(mod_perl). The mod_perl mailing list, archive, and guide are also
great resources.

The simplest configuration of Mason requires a few lines in your
httpd.conf. You can find an annotated version of these lines in
C<eg/httpd.conf> in your distribution.

    PerlModule HTML::Mason::ApacheHandler

    <FilesMatch "\.html$">
        SetHandler perl-script
        PerlHandler HTML::Mason::ApacheHandler
    </FilesMatch>

The PerlModule directive simply ensures that the Mason code is loaded
in the parent process before forking, which can save some memory when
running mod_perl.

The <FilesMatch> section routes all "\.html$" requests to the Mason
handler. If you want all Mason pages to have a distinct extension like
".mcomp", use that instead. If you wish to restrict Mason to one
subdirectory at first, do this:

    <Directory /path/to/subdirectory>
        <FilesMatch "\.html$">
            SetHandler perl-script
            PerlHandler HTML::Mason::ApacheHandler
        </FilesMatch>
    </Directory>

Once you have added the configuration directives, restart the
server. First, go to a standard URL on your site to make sure you
haven't broken anything. If all goes well you should see the same page
as before. If not, recheck your Apache config files and also tail your
server's error log.

If you are getting "404 Not Found" errors even when the files clearly
exist, Mason may be having trouble with your document root. One
situation that will unfortunately confuse Mason is if your document
root goes through a symbolic link. Try expressing your document root
in terms of the true filesystem path.

Next, try adding the tag <% 2+2 %> at the top of some HTML file. If you
reload this page and see a "4", Mason is working!

=head1 DOCUMENTATION ROADMAP

Once Mason is on its feet, the next step is to write a component or
two. The I<Mason Developer's Manual>
(B<L<HTML::Mason::Devel|HTML::Mason::Devel>>) is a complete tutorial
for writing, using, and debugging components. A reference companion to
the Developer's Manual is the Request API documentation,
B<L<HTML::Mason::Request|HTML::Mason::Request>>.

Whoever is responsible for setting up and tuning Mason should read the
I<Administrator's Manual>
(B<L<HTML::Mason::Admin|HTML::Mason::Admin>>), though developers might
also benefit from reading it as well. This document covers more
advanced configuration scenarios and performance
optimization. Reference companions to the Administrator's Manual
include B<L<HTML::Mason::Component|HTML::Mason::Component>>,
B<L<HTML::Mason::Interp|HTML::Mason::Interp>>, and
B<L<HTML::Mason::ApacheHandler|HTML::Mason::ApacheHandler>>.

Most of this documentation assumes that you're running Mason on top of
mod_perl, since that is the most common configuration.  If you are
using Mason outside of mod_perl, the documentation is still valid;
you'll just have to ignore mod_perl specific references like $r and
the ApacheHandler object, and you'll want to read the STANDALONE MODE section in
B<L<HTML::Mason::Interp|HTML::Mason::Interp>>.

=head1 AUTHORS

Jonathan Swartz <swartz@pobox.com>, Dave Rolsky <autarch@urth.org>, Ken Williams <ken@mathforum.org>

=head1 SEE ALSO

L<HTML::Mason::Devel|HTML::Mason::Devel>,
L<HTML::Mason::Request|HTML::Mason::Request>,
L<HTML::Mason::Admin|HTML::Mason::Admin>

=cut
