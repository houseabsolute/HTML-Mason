package HTML::Mason::CGIHandler;

use strict;
use 5.004;
use HTML::Mason;
use CGI;
use Carp;
use Params::Validate qw(:all);

use base qw(HTML::Mason::Container HTML::Mason::Request);

__PACKAGE__->valid_params
    (
     dev_dirs => {type => ARRAYREF, optional => 1},
     interp   => {isa => 'HTML::Mason::Interp'},
    );

__PACKAGE__->contained_objects
    (
     interp => 'HTML::Mason::Interp',
    );


sub new {
    my ($package, %args) = @_;
    
    # If no comp_root given, use DOCUMENT_ROOT
    my @my_args = $package->create_contained_objects(comp_root => $ENV{DOCUMENT_ROOT}, %args);

    my $self = bless { validate @my_args, $package->validation_spec };
    $self->{interp}->out_method(\$self->{output});
    $self->{interp}->compiler->set_allowed_globals('$r');
    
    return $self;
}

sub handle_request {
    my $self = shift;

    my ($local_root, $local_datadir);
    if ($self->{dev_dirs}) {
	foreach my $dir (@{$self->{dev_dirs}}) {
	    if ($ENV{PATH_INFO} =~ s/^\Q$dir//) {
		$local_root    = "$self->{interp}{comp_root}$dir";
		$local_datadir = "$self->{interp}{data_dir}$dir";
		last;
	    }
	}
    }

    
    # Encapsulation be damned!
    # Actually, the 'dev_dirs' stuff won't work properly unless comp_root and data_dir can
    # be changed on the fly.
    local $self->{interp}{comp_root} = $local_root    if $local_root;
    local $self->{interp}{data_dir}  = $local_datadir if $local_datadir;

    my $r = 'HTML::Mason::CGIRequest'->new();
    $self->{interp}->set_global('$r', $r);
    
    $self->{output} = '';
    $self->{interp}->exec($ENV{PATH_INFO}, $r->params);

    if (@_) {
	# This is a secret feature, and should stay secret because it's just a hack for the test suite.
	$_[0] .= $r->http_header . $self->{output};
    } else {
	print $r->http_header;
	print $self->{output};
    }
}

package HTML::Mason::CGIRequest;

sub new {
    my $class = shift;
    return bless {
		  query   => new CGI(),
		  headers => {},
		 }, $class;
}

sub header_out {
    my ($self, $header) = (shift, shift);

    return $self->{headers}{$header} = shift if @_;
    return $self->{headers}{$header};
}

sub content_type {
    my $self = shift;

    return $self->{headers}{'Content-type'} = shift if @_;
    return $self->{headers}{'Content-type'};
}

sub http_header {
    my $self = shift;
    my $method = exists $self->{headers}{'-location'} ? 'redirect' : 'header';
    return $self->{query}->$method(%{$self->{headers}});
}

sub params {
    my $self = shift;
    my @input = map {$_, [$self->{query}->param($_)]} $self->{query}->param;
    foreach (@input) {$_ = $_->[0] if ref($_) and @$_==1}  # Unwrap single-element array refs
    return @input;
}

1;

__END__

=head1 NAME HTML::Mason::CGIHandler - Use Mason in a CGI environment

=head1 SYNOPSIS

In httpd.conf or .htaccess:

   Action html-mason /cgi-bin/mason_handler.cgi
   <FilesMatch "\.html$">
    SetHandler html-mason
   </FilesMatch>

A script at /cgi-bin/mason_handler.pl :

   #!/usr/bin/perl
   use HTML::Mason::CGIHandler;
   
   my $h = new HTML::Mason::CGIHandler
    (
     dev_dirs  => [qw(/cleetus /zeke)],
     data_dir  => '/home/jethro/code/mason_data',
     allow_globals => [qw(%session $u)],
    );
   
   $h->handle_request;

A .html component somewhere in the web server's document root:

   <%args>
    $mood => 'satisfied'
   </%args>
   % $r->header_out(Location => "http://blahblahblah.com/moodring/$mood.html");
   ...

=head1 DESCRIPTION

This module lets you execute Mason components in a CGI environment.
It lets you keep your top-level components in the web server's
document root, using regular component syntax and without worrying
about the particular details of invoking Mason on each request.

If you want to use Mason components from I<within> a regular CGI
script (or any other Perl program, for that matter), then you don't
need this module.  You can simply follow the directions in
L<HTML::Mason::Interp/"STANDALONE MODE">.

This module also provides an C<$r> request object for use inside
components, similar to the Apache request object under
C<HTML::Mason::ApacheHandler>, but limited in functionality.

=head2 C<HTML::Mason::CGIHandler> Methods

=over 4

=item * new()

Creates a new handler.  Accepts any parameter that the Interpreter
accepts, and also accepts a C<dev_dirs> parameter.  If a C<dev_dirs>
parameter is passed, it should contain an array reference of
directories that contain alternate copies of the entire site.  This
helps facilitate seperate development copies when you can't afford to
run a seperate development server.  Using C<dev_dirs> will have the
effect of temporarily appending the given C<dev_dir> to the component
root and data directory if the request is for a component in a
C<dev_dir>.

If no C<comp_root> parameter is passed to C<new()>, the component root
will be C<$ENV{DOCUMENT_ROOT}>.

=item * handle_request()

Handles the current request, reading input from C<$ENV{QUERY_STRING}>
or C<STDIN> and sending output to C<STDOUT>.  This method doesn't
accept any parameters.

=back

=head2 C<$r> Methods

=over 4

=item * header_out()

This works much like the C<Apache> method of the same name.  When
passed the name of a header, returns the value of the given outgoing
header.  When passed a name and a value, sets the value of the header.

The headers are eventually passed to the C<CGI> module's C<header()>
method.

One header currently gets special treatment - if you set a C<Location>
header, you'll cause the C<CGI> module's C<redirect()> method to be
used instead of the C<header()> method.  This means that in order to
do a redirect, all you need to do is:

 $r->header_out(Location => 'http://redirect.to/here');


=item * content_type()

When passed an argument, sets the content type of the current request
to the value of the argument.  Use this method instead of setting a
C<Content-Type> header directly with C<header_out()>.

When called without arguments, returns the value set by a previous
call to C<content_type()>.  The behavior when C<content_type()> hasn't
already been set is undefined - currently it returns C<undef>.

=back

=head1 AUTHOR

Ken Williams, ken@mathforum.org

Jonathan Swartz, swartz@pobox.com

=head1 SEE ALSO

L<HTML::Mason>,
L<HTML::Mason::ApacheHandler>,

=cut

