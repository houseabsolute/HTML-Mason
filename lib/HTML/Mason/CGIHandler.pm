package HTML::Mason::CGIHandler;

use strict;

use HTML::Mason;
use HTML::Mason::Utils;
use CGI;
use File::Spec;
use Params::Validate qw(:all);
use HTML::Mason::Exceptions;

use Class::Container;
use base qw(Class::Container);

use HTML::Mason::MethodMaker
    ( read_write => [ qw( interp ) ] );

use vars qw($VERSION);

# Why do we have a version?  I'm glad you asked.  See, dummy me
# stupidly referenced it in the Subclassing docs _and_ the book.  It's
# needed in order to dynamically have a request subclass change its
# parent properly to work with CGIHandler or ApacheHandler.  It
# doesn't really matter what the version is, as long as it's a true
# value.  - dave
$VERSION = '1.00';


__PACKAGE__->valid_params
    (
     interp => { isa => 'HTML::Mason::Interp' },
    );

__PACKAGE__->contained_objects
    (
     interp => 'HTML::Mason::Interp',
     cgi_request => { class   => 'HTML::Mason::FakeApache', # $r
		      delayed => 1 },
    );


sub new {
    my $package = shift;

    my %p = @_;
    my $self = $package->SUPER::new(comp_root => $ENV{DOCUMENT_ROOT},
				    request_class => 'HTML::Mason::Request::CGI',
				    error_mode => 'output',
				    error_format => 'html',
				    %p);

    $self->interp->out_method(\$self->{output})
        unless exists $p{out_method};
    $self->interp->compiler->add_allowed_globals('$r');
    
    return $self;
}

sub handle_request {
    my $self = shift;
    $self->_handler( { comp => $ENV{PATH_INFO} }, @_ );
}

sub handle_comp {
    my ($self, $comp) = (shift, shift);
    $self->_handler( { comp => $comp }, @_ );
}

sub handle_cgi_object {
    my ($self, $cgi) = (shift, shift);
    $self->_handler( { comp => $cgi->path_info,
		       cgi       => $cgi },
		     @_);
}

sub _handler {
    my ($self, $p) = (shift, shift);

    my $r = $self->create_delayed_object('cgi_request', cgi => $p->{cgi});
    $self->interp->set_global('$r', $r);

    $self->{output} = '';

    $self->interp->delayed_object_params('request', cgi_request => $r);

    my %args = $self->request_args($r);

    eval { $self->interp->exec($p->{comp}, %args) };
    if (my $err = $@) {
        rethrow_exception($err)
          unless isa_mason_exception($err, 'Abort')
          or isa_mason_exception($err, 'Decline');
    }

    if (@_) {
	# This is a secret feature, and should stay secret (or go away) because it's just a hack for the test suite.
	$_[0] .= $r->http_header . $self->{output};
    } else {
        $r->send_http_header;
	print $self->{output};
    }
}

# This is broken out in order to make subclassing easier.
sub request_args {
    my ($self, $r) = @_;

    return $r->params;
}

###########################################################
package HTML::Mason::Request::CGI;
# Subclass for HTML::Mason::Request object $m

use HTML::Mason::Request;
use base qw(HTML::Mason::Request);

use HTML::Mason::MethodMaker
    ( read_only => [ 'cgi_request' ] );

__PACKAGE__->valid_params( cgi_request => {isa => 'HTML::Mason::FakeApache'} );

sub cgi_object {
    my $self = shift;
    return $self->{cgi_request}->query(@_);
}

sub redirect {
    my $self = shift;
    my $url = shift;
    my $status = shift || 302;

    $self->clear_buffer;

    $self->{cgi_request}->header_out( Location => $url );
    $self->{cgi_request}->header_out( Status => $status );

    $self->abort;
}

###########################################################
package HTML::Mason::FakeApache;
@HTML::Mason::FakeApache::ISA = qw(Apache);
# Analogous to Apache request object $r (but not an actual Apache subclass)
# In the future we'll probably want to switch this to Apache::Fake or similar

use HTML::Mason::MethodMaker(read_write => [qw(query)]);

sub new {
    my $class = shift;
    my %p = @_;
    return bless {
		  query           => $p{cgi} || CGI->new,
		  headers_out     => HTML::Mason::FakeTable->new,
		  err_headers_out => HTML::Mason::FakeTable->new,
		  pnotes          => {},
		 }, $class;
}

# CGI request are _always_ main, and there is never a previous or a next
# internal request.
sub main {}
sub prev {}
sub next {}
sub is_main {1}
sub is_initial_req {1}

# What to do with this?
sub allowed {}

sub method {
    shift->query->request_method;
#    return $ENV{REQUEST_METHOD};
}

# There mut be a mapping for this.
sub method_number {}

# Can CGI.pm tell us this?
sub bytes_sent {0}

# The request line sent by the client." Poached from Apache::Emulator.
sub the_request {
    my $self = shift;
    $self->{the_request} ||= join ' ', $self->method,
      ($ENV{QUERY_STRING} ? $self->uri . "?$ENV{QUERY_STRING}" : $self->uri),
      $ENV{SERVER_PROTOCOL} || 'HTTP/1.0';
}

# Is CGI ever a proxy request?
sub proxy_req {}

sub header_only { $ENV{REQUEST_METHOD} eq 'HEAD' }

sub protocol { $ENV{SERVER_PROTOCOL} || 'HTTP/1.0' }

sub hostname { $ENV{HTTP_HOST} }

# Fake it by just giving the current time.
sub request_time { time }

sub uri {
    shift->{uri} ||= $ENV{SCRIPT_NAME} . $ENV{PATH_INFO} || '';
}

# Is this available in CGI?
sub filename {}

# "The $r->location method will return the path of the
# <Location> section from which the current "Perl*Handler"
# is being called." This is irrelevant, I think.
sub location {}

sub path_info { $ENV{PATH_INFO} }

sub args {
    my $self = shift;
    if (@_) {
        # Assign args here.
    }
    return $ENV{QUERY_STRING} unless wantarray;
    # Do more here to return key => arg values.
}

sub headers_in {
    my $self = shift;

    # Create the headers table if necessary. Decided how to build it based on
    # information here:
    # http://cgi-spec.golux.com/draft-coar-cgi-v11-03-clean.html#6.1
    $self->{headers_in} ||= HTML::Mason::FakeTable->new
      ( 'Authorization'       => $ENV{AUTH_TYPE}, # No credentials though.
        'Content-Length'      => $ENV{CONTENT_LENGTH},
        'Content-Type'        => $ENV{CONTENT_TYPE},
        # Convert HTTP environment variables back into their header names.
        map {
            my $k = ucfirst lc;
            $k =~ s/_(.)/-\u$1/g;
            ( $k => $ENV{"HTTP_$_"} )
        } grep { s/^HTTP_// } keys %ENV
      );

    # Give 'em the hash list of the hash table.
    return wantarray ? %{$self->{headers_in}} : $self->{headers_in};
}

sub header_in {
    my ($self, $header) = (shift, shift);
    my $h = $self->headers_in;
    return @_ ? $h->set($header, shift) : $h->get($header);
}


#           The $r->content method will return the entity body
#           read from the client, but only if the request content
#           type is "application/x-www-form-urlencoded".  When
#           called in a scalar context, the entire string is
#           returned.  When called in a list context, a list of
#           parsed key => value pairs are returned.  *NOTE*: you
#           can only ask for this once, as the entire body is read
#           from the client.
# Not sure what to do with this one.
sub content {}

# I think this may be irrelevant under CGI.
sub read {}

# Use LWP?
sub get_remote_host {}
sub get_remote_logname {}

sub http_header {
    my $self = shift;
    my $h = $self->headers_out;
    my $e = $self->err_headers_out;
    my $method = exists $h->{Location} || exists $e->{Location} ?
      'redirect' : 'header';
    return $self->query->$method(tied(%$h)->cgi_headers,
                                 tied(%$e)->cgi_headers);
}

sub send_http_header {
    print shift->http_header;
}

# How do we know this under CGI?
sub get_basic_auth_pw {}
sub note_basic_auth_failure {}

# I think that this just has to be empty.
sub handler {}

sub notes {
    my ($self, $key) = (shift, shift);
    $self->{notes} ||= HTML::Mason::FakeTable->new;
    return wantarray ? %{$self->{notes}} : $self->{notes}
      unless defined $key;
    return $self->{notes}{$key} = "$_[0]" if @_;
    return $self->{notes}{$key};
}

sub pnotes {
    my ($self, $key) = (shift, shift);
    return wantarray ? %{$self->{pnotes}} : $self->{pnotes}
      unless defined $key;
    return $self->{pnotes}{$key} = $_[0] if @_;
    return $self->{pnotes}{$key};
}

sub subprocess_env {
    my ($self, $key) = (shift, shift);
    unless (defined $key) {
        $self->{subprocess_env} = HTML::Mason::FakeTable->new(%ENV);
        return wantarray ? %{$self->{subprocess_env}} :
          $self->{subprocess_env};

    }
    $self->{subprocess_env} ||= HTML::Mason::FakeTable->new(%ENV);
    return $self->{subprocess_env}{$key} = "$_[0]" if @_;
    return $self->{subprocess_env}{$key};
}

sub content_type {
    shift->header_out('Content-Type', @_);
}

sub content_encoding {
    shift->header_out('Content-Encoding', @_);
}

sub content_languages {
    my ($self, $langs) = @_;
    return unless $langs;
    my $h = shift->headers_out;
    for my $l (@$langs) {
        $h->add('Content-Language', $l);
    }
}

sub status {
    shift->header_out('Status', @_);
}

sub status_line {
    # What to do here? Should it be managed differently than status?
    my $self = shift;
    if (@_) {
        my $status = shift =~ /^(\d+)/;
        return $self->header_out('Status', $status);
    }
    return $self->header_out('Status');
}

sub headers_out {
    my $self = shift;
    return wantarray ? %{$self->{headers_out}} : $self->{headers_out};
}

sub header_out {
    my ($self, $header) = (shift, shift);
    my $h = $self->headers_out;
    return @_ ? $h->set($header, shift) : $h->get($header);
}

sub err_headers_out {
    my $self = shift;
    return wantarray ? %{$self->{err_headers_out}} : $self->{err_headers_out};
}

sub err_header_out {
    my ($self, $err_header) = (shift, shift);
    my $h = $self->err_headers_out;
    return @_ ? $h->set($err_header, shift) : $h->get($err_header);
}

sub no_cache {
    my $self = shift;
    $self->header_out(Pragma => 'no-cache');
    $self->header_out('Cache-Control' => 'no-cache');
}

sub print {
    shift->query->print(@_);
}

# "Send the contents of a file to the client." Do we really want to do this?
sub send_fd {}

# Should this perhaps throw an exception?
sub internal_redirect {}
sub internal_redirect_handler {}

# Do something with ErrorDocument?
sub custom_response {}

# I think we'ev made this essentially the same thing.
BEGIN {
    local $^W;
    *send_cgi_header = \&send_http_header;
}

# Does CGI support logging?
sub log_reason {}
sub log_error {}
sub warn {}

sub params {
    my $self = shift;
    return HTML::Mason::Utils::cgi_request_args($self->query,
                                                $self->query->request_method);
}

1;

###########################################################
package HTML::Mason::FakeTable;
# Analogous to Apache::Table.
use strict;

sub new {
    my $class = shift;
    my $self = {};
    tie %{$self}, 'HTML::Mason::FakeTableHash';
    %$self = @_ if @_;
    return bless $self, ref $class || $class;
}

sub set {
    my ($self, $header, $value) = @_;
    defined $value ? $self->{$header} = $value : delete $self->{$header};
}

sub unset {
    my $self = shift;
    delete $self->{shift()};
}

sub add {
    tied(%{shift()})->add(@_);
}

sub clear {
    %{shift()} = ();
}

sub get {
    tied(%{shift()})->get(@_);
}

sub merge {
    my ($self, $key, $value) = @_;
    if (defined $self->{$key}) {
        $self->{$key} .= ',' . $value;
    } else {
        $self->{$key} = "$value";
    }
}

sub do {
    my ($self, $code) = @_;
    while (my ($k, $val) = each %$self) {
        for my $v (ref $val ? @$val : $val) {
            return unless $code->($k => $v);
        }
    }
}

###########################################################
package HTML::Mason::FakeTableHash;
# Used by HTML::Mason::FakeTable.
use strict;

sub TIEHASH {
    my $class = shift;
    return bless {}, ref $class || $class;
}

sub _canonical_key {
    my $key = lc shift;
    # CGI really wants a - before each header
    return substr( $key, 0, 1 ) eq '-' ? $key : "-$key";
}

sub STORE {
    my ($self, $key, $value) = @_;
    $self->{_canonical_key $key} = [ $key => ref $value ? "$value" : $value ];
}

sub add {
    my ($self, $key) = (shift, shift);
    return unless defined $_[0];
    my $value = ref $_[0] ? "$_[0]" : $_[0];
    my $ckey = _canonical_key $key;
    if (exists $self->{$ckey}) {
        if (ref $self->{$ckey}[1]) {
            push @{$self->{$ckey}[1]}, $value;
        } else {
            $self->{$ckey}[1] = [ $self->{$ckey}[1], $value ];
        }
    } else {
        $self->{$ckey} = [ $key => $value ];
    }
}

sub DELETE {
    my ($self, $key) = @_;
    my $ret = delete $self->{_canonical_key $key};
    return $ret->[1];
}

sub FETCH {
    my ($self, $key) = @_;
    # Grab the values first so that we don't autovivicate the key.
    my $val = $self->{_canonical_key $key} or return;
    if (my $ref = ref $val->[1]) {
        return unless $val->[1][0];
        # Return the first value only.
        return $val->[1][0];
    }
    return $val->[1];
}

sub get {
    my ($self, $key) = @_;
    my $ckey = _canonical_key $key;
    return unless exists $self->{$ckey};
    return $self->{$ckey}[1] unless ref $self->{$ckey}[1];
    return wantarray ? @{$self->{$ckey}[1]} : $self->{$ckey}[1][0];
}

sub CLEAR {
    %{shift()} = ();
}

sub EXISTS {
    my ($self, $key)= @_;
    return exists $self->{_canonical_key $key};
}

sub FIRSTKEY {
    my $self = shift;
    # Reset perl's iterator.
    keys %$self;
    # Get the first key via perl's iterator.
    my $first_key = each %$self;
    return undef unless defined $first_key;
    return $self->{$first_key}[0];
}

sub NEXTKEY {
    my ($self, $nextkey) = @_;
    # Get the next key via perl's iterator.
    my $next_key = each %$self;
    return undef unless defined $next_key;
    return $self->{$next_key}[0];
}

sub cgi_headers {
    my $self = shift;
    map { $_ => $self->{$_}[1] } keys %$self;
}

1;
__END__

=head1 NAME

HTML::Mason::CGIHandler - Use Mason in a CGI environment

=head1 SYNOPSIS

In httpd.conf or .htaccess:

   Action html-mason /cgi-bin/mason_handler.cgi
   <LocationMatch "\.html$">
    SetHandler html-mason
   </LocationMatch>

A script at /cgi-bin/mason_handler.pl :

   #!/usr/bin/perl
   use HTML::Mason::CGIHandler;

   my $h = HTML::Mason::CGIHandler->new
    (
     data_dir  => '/home/jethro/code/mason_data',
     allow_globals => [qw(%session $u)],
    );

   $h->handle_request;

A .html component somewhere in the web server's document root:

   <%args>
    $mood => 'satisfied'
   </%args>
   % $r->err_header_out(Location => "http://blahblahblah.com/moodring/$mood.html");
   ...

=head1 DESCRIPTION

This module lets you execute Mason components in a CGI environment.
It lets you keep your top-level components in the web server's
document root, using regular component syntax and without worrying
about the particular details of invoking Mason on each request.

If you want to use Mason components from I<within> a regular CGI
script (or any other Perl program, for that matter), then you don't
need this module.  You can simply follow the directions in
ADMIN<Using Mason from a standalone script>.

This module also provides an C<$r> request object for use inside
components, similar to the Apache request object under
C<HTML::Mason::ApacheHandler>, but limited in functionality.  Please
note that we aim to replicate the C<mod_perl> functionality as closely
as possible - if you find differences, do I<not> depend on them to
stay different.  We may fix them in a future release.  Also, if you
need some missing functionality in C<$r>, let us know, we might be
able to provide it.

Finally, this module alters the C<HTML::Mason::Request> object C<$m> to
provide direct access to the CGI query, should such access be necessary.

=head2 C<HTML::Mason::CGIHandler> Methods

=over 4

=item * new()

Creates a new handler.  Accepts any parameter that the Interpreter
accepts.

If no C<comp_root> parameter is passed to C<new()>, the component root
will be C<$ENV{DOCUMENT_ROOT}>.

=item * handle_request()

Handles the current request, reading input from C<$ENV{QUERY_STRING}>
or C<STDIN> and sending headers and component output to C<STDOUT>.
This method doesn't accept any parameters.  The initial component
will be the one specified in C<$ENV{PATH_INFO}>.

=item * handle_comp()

Like C<handle_request()>, but the first (only) parameter is a
component path or component object.  This is useful within a
traditional CGI environment, in which you're essentially using Mason
as a templating language but not an application server.

C<handle_component()> will create a CGI query object, parse the query
parameters, and send the HTTP header and component output to STDOUT.
If you want to handle those parts yourself, see
ADMIN<Using Mason from a standalone script>.

=item * handle_cgi_object()

Also like C<handle_request()>, but this method takes only a CGI object
as its parameter.  This can be quite useful if you want to use this
module with CGI::Fast.

The component path will be the value of the CGI object's
C<path_info()> method.

=item * request_args()

Given an C<HTML::Mason::FakeApache> object, this method is expected to
return a hash containing the arguments to be passed to the component.
It is a separate method in order to make it easily overrideable in a
subclass.

=item * interp()

Returns the Mason Interpreter associated with this handler.  The
Interpreter lasts for the entire lifetime of the handler.

=back

=head2 $r Methods

=over 4

=item * headers_in()

This works much like the C<Apache> method of the same name. In an array
context, it will return a C<%hash> of response headers. In a scalar context,
it will return a reference to the case-insensitive hash blessed into the
C<HTML::Mason::FakeTable> class. The values initially populated in this hash are
extracted from the CGI environment variables as best as possible. The pattern
is to merely reverse the conversion from HTTP headers to CGI variables as
documented here: L<http://cgi-spec.golux.com/draft-coar-cgi-v11-03-clean.html#6.1>.

=item * header_in()

This works much like the C<Apache> method of the same name. When passed the
name of a header, returns the value of the given incoming header. When passed
a name and a value, sets the value of the header. Setting the header to
C<undef> will actually I<unset> the header (instead of setting its value to
C<undef>), removing it from the table of headers returned from future calls to
C<headers_in()> or C<header_in()>.

=item * headers_out()

This works much like the C<Apache> method of the same name. In an array
context, it will return a C<%hash> of response headers. In a scalar context,
it will return a reference to the case-insensitive hash blessed into the
C<HTML::Mason::FakeTable> class. Changes made to this hash will be made to the
headers that will eventually be passed to the C<CGI> module's C<header()>
method.

=item * header_out()

This works much like the C<Apache> method of the same name.  When
passed the name of a header, returns the value of the given outgoing
header.  When passed a name and a value, sets the value of the header.
Setting the header to C<undef> will actually I<unset> the header
(instead of setting its value to C<undef>), removing it from the table
of headers that will be sent to the client.

The headers are eventually passed to the C<CGI> module's C<header()>
method.

=item * err_headers_out()

This works much like the C<Apache> method of the same name. In an array
context, it will return a C<%hash> of error response headers. In a scalar
context, it will return a reference to the case-insensitive hash blessed into
the C<HTML::Mason::FakeTable> class. Changes made to this hash will be made to
the error headers that will eventually be passed to the C<CGI> module's
C<header()> method.

=item * err_header_out()

This works much like the C<Apache> method of the same name. When passed the
name of a header, returns the value of the given outgoing error header. When
passed a name and a value, sets the value of the error header. Setting the
header to C<undef> will actually I<unset> the header (instead of setting its
value to C<undef>), removing it from the table of headers that will be sent to
the client.

The headers are eventually passed to the C<CGI> module's C<header()> method.

One header currently gets special treatment - if you set a C<Location>
header, you'll cause the C<CGI> module's C<redirect()> method to be
used instead of the C<header()> method.  This means that in order to
do a redirect, all you need to do is:

 $r->err_header_out(Location => 'http://redirect.to/here');

You may be happier using the C<< $m->redirect >> method, though,
because it hides most of the complexities of sending headers and
getting the status code right.

=item * content_type()

When passed an argument, sets the content type of the current request
to the value of the argument.  Use this method instead of setting a
C<Content-Type> header directly with C<header_out()>.  Like
C<header_out()>, setting the content type to C<undef> will remove any
content type set previously.

When called without arguments, returns the value set by a previous
call to C<content_type()>.  The behavior when C<content_type()> hasn't
already been set is undefined - currently it returns C<undef>.

If no content type is set during the request, the default MIME type
C<text/html> will be used.

=item * method()

Returns the request method used for the current request, e.g., "GET", "POST",
etc.

=item * http_header()

This method returns the outgoing headers as a string, suitable for
sending to the client.

=item * send_http_header()

Sends the outgoing headers to the client.

=item * notes()

This works much like the C<Apache> method of the same name. When passed
a C<$key> argument, it returns the value of the note for that key. When
passed a C<$value> argument, it stores that value under the key. Keys are
case-insensitive, and both the key and the value must be strings. When
called in a scalar context with no C<$key> argument, it returns a hash
reference blessed into the C<HTML::Mason::FakeTable> class.

=item * pnotes()

Like C<notes()>, but takes any scalar as an value, and stores the
values in a case-sensitive hash.

=item * subprocess_env()

Works like the C<Apache> method of the same name, but is simply populated with
the current values of the environment. Still, it's useful, because values can
be changed and then seen by later components, but the environment itself
remains unchanged. Like the C<Apache> method, it will reset all of its values
to the current environment again if it's called without a C<$key> argument.

=item * params()

This method returns a hash containing the parameters sent by the
client.  Multiple parameters of the same name are represented by array
references.  If both POST and query string arguments were submitted,
these will be merged together.

=back

=head2 Added C<$m> methods

The C<$m> object provided in components has all the functionality of
the regular C<HTML::Mason::Request> object C<$m>, and the following:

=over 4

=item * cgi_object()

Returns the current C<CGI> request object.  This is handy for
processing cookies or perhaps even doing HTML generation (but is that
I<really> what you want to do?).  If you pass an argument to this
method, you can set the request object to the argument passed.  Use
this with care, as it may affect components called after the current
one (they may check the content length of the request, for example).

Note that the ApacheHandler class (for using Mason under mod_perl)
also provides a C<cgi_object()> method that does the same thing as
this one.  This makes it easier to write components that function
equally well under CGIHandler and ApacheHandler.

=item * cgi_request()

Returns the object that is used to emulate Apache's request object.
In other words, this is the object that C<$r> is set to when you use
this class.

=back

=head2 C<HTML::Mason::FakeTable> Methods

This class emulates the behavior of the C<Apache::Table> class, and is
used to store manage the tables of values for the following attributes
of <$r>:

=over 4

=item headers_in

=item headers_out

=item err_headers_out

=item notes

=item subprocess_env

=back

C<HTML::Mason::FakeTable> is designed to behave exactly like C<Apache::Table>,
and differs in only one respect. When a given key has multiple values in an
C<Apache::Table> object, one can fetch each of the values for that key using
Perl's C<each> operator:

  while (my ($k, $v) = each %{$r->headers_out}) {
      push @cookies, $v if lc $k eq 'set-cookie';
  }

If anyone knows how Apache::Table does this, let us know! In the meantime, use
C<get()> or C<do()> to get at all of the values for a given key (C<get()> is
much more efficient, anyway).

Since the methods named for these attributes return an
C<HTML::Mason::FakeTable> object hash in a scalar reference, it seemed only
fair to document its interface.

=over 4

=item * new()

Returns a new C<HTML::Mason::FakeTable> object. Any parameters passed
to C<new()> will be added to the table as initial values.

=item * add()

Adds a new value to the table. If the value did not previously exist under the
given key, it will be created. Otherwise, it will be added as a new value to
the key.

=item * clear()

Clears the table of all values.

=item * do()

Pass a code reference to this method to have it iterate over all of the
key/value pairs in the table. Keys will multiple values will trigger the
execution of the code reference multiple times for each value. The code
reference should expect two arguments: a key and a value. Iteration terminates
when the code reference returns false, to be sure to have it return a true
value if you wan it to iterate over every value in the table.

=item * get()

Gets the value stored for a given key in the table. If a key has multiple
values, all will be returned when C<get()> is called in an array context, and
only the first value when it is called in a scalar context.

=item * merge()

Merges a new value with an existing value by concatenating the new value onto
the existing. The result is a comma-separated list of all of the values merged
for a given key.

=item * set()

Takes key and value arguments and sets the value for that key. Previous values
for that key will be discarded. The value must be a string, or C<set()> will
turn it into one. A value of C<undef> will have the same behavior as
C<unset()>.

=item * unset()

Takes a single key argument and deletes that key from the table, so that none
of its values will be in the table any longer.

=back

=head1 SEE ALSO

L<HTML::Mason|HTML::Mason>,
L<HTML::Mason::Admin|HTML::Mason::Admin>,
L<HTML::Mason::ApacheHandler|HTML::Mason::ApacheHandler>

=cut
