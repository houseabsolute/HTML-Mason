# Copyright (c) 1998 by Jonathan Swartz. All rights reserved.
# This program is free software; you can redistribute it and/or modify it
# under the same terms as Perl itself.

package HTML::Mason::FakeApache;
require 5.003;
require Exporter;
@ISA = qw(Exporter);
@EXPORT = ();
@EXPORT_OK = ();

sub new {
    my $class = shift;
    my $self = {
	headers_in=>{},
	headers_out=>{},
	dir_config=>{},
	cgi_env=>{}
    };
    $self->{connection} = new HTML::Mason::FakeApache::Connection;
    $self->{server} = new HTML::Mason::FakeApache::Server;
    bless $self, $class;
    return $self;
}

sub read_scalar {
    my ($self, $field) = @_;
    return $self->{$field};
}

sub access_scalar {
    my ($self,$field,$value) = @_;
    if (@_ >= 3) {
	$self->{$field} = $value;
    }
    return $self->{$field};
}

sub access_hash {
    my ($self,$field,$key,$value) = @_;
    if (@_ >= 4) {
	$self->{$field}->{$key} = $value;
    }
    return $self->{$field}->{$key};
}

sub headers_in {
    return %{$self->{headers_in}};
}

sub headers_out {
    return %{$self->{headers_out}};
}

sub cgi_env {
    if (@_ >= 2) {
	return access_hash(@_);
    } else {
	return %{$self->{cgi_env}};
    }
}

sub print {
    my $self = shift;
    print (@_);
}

sub rflush {
}

sub send_http_header {
    my $self = shift;
    $self->content_type('text/plain') if !$self->content_type;
    print "Server: ".$self->cgi_var('SERVER_SOFTWARE')."\n" if $self->cgi_var('SERVER_SOFTWARE');
    print "Content-type: ".$self->content_type."\n";
    my %headers = %{$self->headers_out};
    while (my ($key,$value) = each(%headers)) {
	print "$key: $value\n";
    }
    print "\n";
}

sub sent_header { return 1 }

sub AUTOLOAD {
    my $self = shift;
    my $type = ref($self) or die "autoload error: bad function $AUTOLOAD";

    my $name = $AUTOLOAD;
    $name =~ s/.*://;   # strip fully-qualified portion
    return if $name eq 'DESTROY';

    if ($name =~ /^(method|method_number|bytes_sent|the_request|proxyreq|header_only|protocol|uri|filename|path_info|content|requires|auth_type|auth_name|document_root|allow_options|content_type|content_encoding|content_language|status|status_line|args)$/) {
	$self->access_scalar($name,@_);
    } elsif ($name =~ /^(connection|server)$/) {
	$self->read_scalar($name,@_);
    } elsif ($name =~ /^(header_in|header_out|dir_config|cgi_var)$/) {
	my $h = {header_in=>'headers_in',header_out=>'headers_out',dir_config=>'dir_config','cgi_env'=>'cgi_env','cgi_var'=>'cgi_env'}->{$name};
	$self->access_hash($h,@_);
    } else {
        die "No such function `$name' in class $type";
    }
}

package HTML::Mason::FakeApache::Server;
sub new {
    my $class = shift;
    my $self = {
    };
    bless $self, $class;
    return $self;
}

sub access_scalar {
    my ($self,$field,$value) = @_;
    if (@_ >= 3) {
	$self->{$field} = $value;
    }
    return $self->{$field};
}

sub AUTOLOAD {
    my $self = shift;
    my $type = ref($self) or die "autoload error: bad function $AUTOLOAD";

    my $name = $AUTOLOAD;
    $name =~ s/.*://;   # strip fully-qualified portion
    return if $name eq 'DESTROY';

    if ($name =~ /^(server_admin|server_hostname|port|is_virtual|names)$/) {
	$self->access_scalar($name,@_);
    } else {
        die "No such function `$name' in class $type";
    }
}

package HTML::Mason::FakeApache::Connection;
sub new {
    my $class = shift;
    my $self = {
    };
    bless $self, $class;
    return $self;
}

sub access_scalar {
    my ($self,$field,$value) = @_;
    if (@_ >= 3) {
	$self->{$field} = $value;
    }
    return $self->{$field};
}

sub AUTOLOAD {
    my $self = shift;
    my $type = ref($self) or die "autoload error: bad function $AUTOLOAD";

    my $name = $AUTOLOAD;
    $name =~ s/.*://;   # strip fully-qualified portion
    return if $name eq 'DESTROY';

    if ($name =~ /^(remote_host|remote_ip|local_addr|remote_addr|remote_logname|user|auth_type|aborted)$/) {
	$self->access_scalar($name,@_);
    } else {
        die "No such function `$name' in class $type";
    }
}

1;
