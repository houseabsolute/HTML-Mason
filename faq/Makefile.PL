use ExtUtils::MakeMaker;
require 5.004;

# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.
WriteMakefile(
    'NAME'    => 'HTML::Mason::faq',
    'SKIP'    => [qw(all pure_all static static_lib dynamic dynamic_lib test)],
    'clean'   => {'FILES' => 'html_mason_faq.html'},
);

sub MY::top_targets {
	'
pure_all all :: faq
faq ::	html_mason_faq.html
test:

html_mason_faq.html: html_mason_faq.pod
	pod2html --htmlroot=. --podpath=. --infile=html_mason_faq.pod \
			--outfile=html_mason_faq.html && \
	rm -f pod2html-itemcache pod2html-dircache
';
}
