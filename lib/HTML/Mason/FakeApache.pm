# Copyright (c) 1998 by Jonathan Swartz. All rights reserved.
# This program is free software; you can redistribute it and/or modify it
# under the same terms as Perl itself.

package HTML::Mason::FakeApache;
require 5.003;
require Exporter;
@ISA = qw(Exporter);
@EXPORT = ();
@EXPORT_OK = ();

use mod_perl;
use strict;
use vars (qw($AUTOLOAD));

sub new {
    my $class = shift;
    my $self = {};
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

sub access_table {
    my ($self,$field,$key,$value) = @_;
    if (wantarray) {
	return %{$self->{$field}};
    } elsif (@_==2) {
	return $self->{$field};
	# die "FakeApache is unable to simulate the Apache::Table class for the `$field' method called in scalar context.  You'll need to temporarily remove or comment out that method call to use debug files.\n";
    } elsif (@_==3) {
	return $self->{$field}->{$key};
    } else {
	return ($self->{$field}->{$key} = $value);
    }
}

sub args {
    my $self = shift;
    return (wantarray) ? @{$self->{'args@'}} : $self->{'args$'};
}

sub get_basic_auth_pw {
    my $self = shift;
    return @{$self->{get_basic_auth_pw}};
}

sub print {
    my $self = shift;
    print (@_);
}

sub write_client {
    my $self = shift;
    print (@_);
}

sub send_fd
{
    local (*FD) = @_;
    print <FD>;
}

sub warn {
    my ($self, $msg) = @_;
    print "warn: $msg\n";
}

sub send_http_header {
    my $self = shift;
    $self->content_type('text/plain') if !$self->content_type;
    print "Server: ".$ENV{'SERVER_SOFTWARE'}."\n" if $ENV{'SERVER_SOFTWARE'};
    print "Content-type: ".$self->content_type."\n";
    my $href = $self->headers_out;
    if (ref($href) eq 'HASH') {
	while (my ($key,$value) = each(%{$href})) {
	    print "$key: $value\n";
	}
    }
    print "\n";
}

sub sent_header { return 1 }

sub module { return 0 }

sub AUTOLOAD {
    my $self = shift;
    my $type = ref($self) or die "autoload error: bad function $AUTOLOAD";

    my $name = $AUTOLOAD;
    $name =~ s/.*://;   # strip fully-qualified portion
    return if $name eq 'DESTROY';

    if ($name =~ /^(allow_options|auth_name|auth_type|bytes_sent|no_cache|content|content_encoding|content_languages|content_type|document_root|filename|header_only|method|method_number|path_info|protocol|proxyreq|requires|status|status_line|the_request|uri)$/) {
	$self->access_scalar($name,@_);
    } elsif ($name =~ /^(as_string|connection|get_remote_host|get_remote_logname|get_server_port|is_initial_req|is_main|server)$/) {
	$self->read_scalar($name,@_);
    } elsif ($name =~ /^(dir_config|headers_in|headers_out|err_headers_out|notes|subprocess_env)$/) {
	$self->access_table($name,@_);	
    } elsif ($name =~ /^(header_in|header_out|err_header_out|notes)$/) {
	my $h = {header_in=>'headers_in',header_out=>'headers_out',err_header_out=>'err_headers_out'}->{$name};
	$self->access_hash($h,@_);
    } elsif ($name =~ /^(log_error|log_reason|post_connection|note_basic_auth_failure|register_cleanup|send_cgi_header|custom_response|rflush|cgi_header_out)$/) {
	# do nothing and hope it's okay
    } elsif ($name =~ /^(last|soft_timeout|kill_timeout|lookup_file|lookup_uri|main|prev|next|handler|hard_timeout|reset_timeout|internal_redirect|internal_redirect_handler)$/) {
	die "FakeApache is unable to simulate the `$name' method.  You'll need to temporarily remove or comment out that method call to use debug files.\n";
    } else {
        CORE::warn("FakeApache does not know about the `$name' method; returning undef.\n");
	return undef;
    }
}

package HTML::Mason::FakeApache::Server;
use vars (qw($AUTOLOAD));
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
    } elsif ($name =~ /^(warn|log_error)$/) {
	# do nothing and hope it's okay
    } else {
        warn "FakeApache::Server does not know about the `$name' method; returning undef.\n";
	return undef;
    }
}

package HTML::Mason::FakeApache::Connection;
use vars (qw($AUTOLOAD));
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
        warn "FakeApache::Connection does not know about the `$name' method; returning undef.\n";
	return undef;
    }
}

1;
