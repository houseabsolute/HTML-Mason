package HTML::Mason::Plugin;

use strict;

sub new {
  my $class = shift;
  bless { @_ }, $class;
}

sub start_request {
  # my ($self, $request, $request_args) = @_;
}

sub end_request {
  # my ($self, $request, $request_args, $wantarray, $result, $error) = @_;
}

sub start_component {
  # my ($self, $request, $comp, $args) = @_;
}

sub end_component {
  # my ($self, $request, $comp, $args, $wantarray, $result, $error) = @_;
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
      my ($self, $comp, $request, $args) = @_;
      push @{$self->{ timers }}, Time::HiRes::time;
   }

   sub end_component {
      my ($self, $comp, $request, $args, $wantarray, $result, $error) = shift;
      my $elapsed = Time::HiRes::time - pop @{$self->{ timers }};
      printf STDERR "Component %s took %.1f seconds\n", $comp, $elapsed;
   }

   1;

=head1 DESCRIPTION

Use a Mason plugin to have actions occur at the beginning or end of
requests or components. Plugins are activated by passing P<plugins> in
the interpreter or request object. Each plugin in the list can be
specified as a class name (in which case the plugin object is created
once for each request) or as an actual object of the plugin class.

If your plugin can be configured, place the configuration in class
variables, which can be set either from httpd.conf via PerlSetVar
directives, or in perl directly from a handler.pl file.

B<WARNING>: do not keep an unweakened reference to a request or
component object in your plugin object, or you will create a nasty
circular reference.

=head1 PLUGIN HOOKS

A plugin class defines one or more of the following hooks (methods):
I<start_request>, I<end_request>, I<start_component>, and
I<end_component>.

=over

=item start_request

The C<start_request> hook is called before the Mason request begins
execution. It receives the following arguments:

    $self         # plugin object or class
    $request      # the current request
    $request_args # arrayref of arguments the request was called with

The hook may modify I<$request_args>. Note that subrequests (see
L<HTML::Mason::Request|HTML::Mason::Request> will create a new plugin
object and execute this code again; you can skip your code for
subrequests by checking C<is_subrequest> on I<$request>.

=item end_request

The C<end_request> hook is called before the Mason request
exits. It receives the following arguments:

    $self        # plugin object or class
    $request     # the current request
    $args        # arguments the request was called with
    $wantarray   # value of wantarray the request was called with
    $result      # arrayref of value(s) that the request is about to return
    $error       # reference to error, if any, that the request is about to throw

I<$result> always contains an array ref; if I<$wantarray> is 0, the
return value is the the first element of that array. The plugin may
modify both I<$result> and I<$error> to affect how the request returns.

=item start_component

The C<start_component()> hook is called before a component begins
executing. It receives the following arguments:

    $self        # plugin object or class
    $request     # the current request
    $comp        # the component object
    $args        # arrayref of arguments the component was called with

The plugin may NOT modify I<$args> currently.

=item end_component

The C<end_component()> hook is called after a component has
completed. It receives the following arguments:

    $self        # plugin object or class
    $request     # the current request
    $comp        # the component object
    $args        # arrayref of arguments the component was called with
    $wantarray   # value of wantarray the component was called with
    $result      # arrayref of value(s) that the component is about to return
    $error       # reference to error, if any, that the component is about to throw

I<$result> always contains an array ref; if I<$wantarray>
is 0, the return value is the first element of that array.  The plugin
may modify both I<$result> and I<$error> to affect how the request
returns.

It would be desirable for this hook to have access to the component's
output as well as its return value, but this is currently impossible
because output from all components is generally appended to a single
buffer.

=back

=head1 AUTHORS

Doug Treder, Jonathan Swartz, Dave Rolsky

=head1 SEE ALSO

L<HTML::Mason::Request|HTML::Mason::Request>

=cut
