package HTML::Mason::Plugin;

use strict;

sub new {
  my $class = shift;
  bless { @_ }, $class;
}

sub start_request {
  # my $self = shift;
  # my $context = shift;
}

sub end_request {
  # my $self = shift;
  # my $context = shift;
}

sub start_component {
  # my $self = shift;
  # my $context = shift;
}

sub end_component {
  # my $self = shift;
  # my $context = shift;
}

1;

__END__

=head1 NAME

HTML::Mason::Plugin - Plugin Base class for Mason

=head1 SYNOPIS

   package HTML::Mason::Plugin::Timer;
   use base qw(HTML::Mason::Plugin);
   use Time::HiRes;

   sub start_component {
      my $self = shift;
      push @{$self->{ timers }}, Time::HiRes::time;
   }

   sub end_component {
      my $self = shift;
      my $context = shift;
      my $comp = $context->{comp};
      my $elapsed = Time::HiRes::time - pop @{$self->{ timers }};
      print STDERR "Component $comp took $elapsed seconds\n";
   }

   1;

=head1 DESCRIPTION

Use a Mason plugin to have actions occur at the beginning or end of
requests or components.  If you configure the plugin with a class
name, the plugin object is created once for each request; if you give
an actual object, the same object will be used for all requests.

If your plugin can be configured, place the configuration in class
variables, which can be set either from httpd.conf via PerlSetVar
directives, or in perl directly from a handler.pl file.

=head1 PLUGIN METHODS

B<WARNING>: do not keep a reference to the request object in your
plugin object, or you will create a nasty circular reference.

=over

=item start_request

Receives a hashref containing:

    request => $request,    # the current request
    args    => \@args,      # arguments the request was called with
    wantarray => $wantarray # value of wantarray that request was called with

This is called before the Mason request begins execution.  Note that
subrequests (see L<HTML::Mason::Request|HTML::Mason::Request> will
create a new plugin object and execute this code again; you can
prevent this by calling C<is_subrequest()> on the request object.

B<WARNING>: do not keep a reference to the request object in your
plugin object.  Doing so creates a circular reference that will be a
memory leak.

=item end_request

Receives a hashref containing:

    request => $request,    # the current request
    args    => \@args,      # arguments the request was called with
    wantarray => $wantarray # value of wantarray that request was called with
    return_value => \@rv    # return value that request returned

This is called after all components have executed.  The "return_value"
parameter always contains an array ref; if the component returns only
a single value, it will be the first element of that array.

=item start_component

Receives a hashref containing:

   request => $request,    # the current request
   comp => $comp,          # the component object
   args => \@args,         # the arguments the component was called with
   wantarray => $wantarray # value of wantarray that component was called with

The C<start_component()> method is called before a component begins
executing.  If you modify the array that "args" points to, it will
take effect in the arguments the component receives.

=item end_component

Receives a hashref containing:

   request => $request,    # the current request
   comp => $comp,          # the component object
   args => \@args,         # the arguments the component was called with
   wantarray => $wantarray # value of wantarray that component was called with
   error =>  $error        # error message, if any, when component was executed
   return_value => \@rv    # return value the component returned

The C<end_component()> method is called after a component has
completed and before the next component begins.  The "return_value"
parameter always contains an array ref; if the component returns only
a single value, it will be the element of that array.

=back

=head1 AUTHOR

Doug Treder

=head1 SEE ALSO

L<HTML::Mason::Request|HTML::Mason::Request>

=cut
