# -*- cperl-indent-level: 4; cperl-continued-brace-offset: -4; cperl-continued-statement-offset: 4 -*-

# Copyright (c) 1998-2003 by Jonathan Swartz. All rights reserved.
# This program is free software; you can redistribute it and/or modify it
# under the same terms as Perl itself.


#
# A note about the internals:
#
# Because request is the single most intensively used piece of the
# Mason architecture, this module is often the best target for
# optimization.
#
# By far, the two methods called most often are comp() and print().
# We have attempted to optimize the parts of these methods that handle
# the _normal_ path through the code.
#
# Code paths that are followed less frequently (like the path that
# handles the $mods{store} parameter in comp, for example) are
# intentionally not optimized because doing so would clutter the code
# while providing a minimal benefit.
#
# Many of the optimizations consist of ignoring defined interfaces for
# accessing parts of the request object's internal data structure, and
# instead accessing it directly.
#
# We have attempted to comment these various optimizations
# appropriately, so that future hackers understand that we did indeed
# mean to not use the relevant interface in that particular spot.
#

package HTML::Mason::Request;

use strict;

use File::Spec;
use HTML::Mason::Tools qw(read_file compress_path load_pkg pkg_loaded absolute_comp_path);
use HTML::Mason::Utils;
use HTML::Mason::Buffer;

use Class::Container;
use base qw(Class::Container);

# HTML::Mason::Exceptions always exports rethrow_exception() and isa_mason_exception()
use HTML::Mason::Exceptions( abbr => [qw(param_error syntax_error
					 top_level_not_found_error error)] );

use Params::Validate qw(:all);
Params::Validate::validation_options( on_fail => sub { param_error( join '', @_ ) } );

BEGIN
{
    __PACKAGE__->valid_params
	(
	 args =>
         { type => ARRAYREF, default => [],
           descr => "Array of arguments to initial component",
           public => 0 },

	 autoflush =>
         { parse => 'boolean', default => 0, type => SCALAR,
           descr => "Whether output should be buffered or sent immediately" },

	 comp =>
         { type => SCALAR | OBJECT, optional => 0,
           descr => "Initial component, either an absolute path or a component object",
           public => 0 },

         data_cache_api =>
         { parse => 'string', default => '1.1', type => SCALAR,
           regex => qr/^(?:1\.0|1\.1)$/,
           descr => "Data cache API to use: 1.0 or 1.1" },

	 data_cache_defaults =>
         { parse => 'hash_list', type => HASHREF|UNDEF, optional => 1,
           descr => "A hash of default parameters for Cache::Cache" },

	 declined_comps =>
         { type => HASHREF, optional => 1,
           descr => "Hash of components that have been declined in previous parent requests",
           public => 0 },

	 dhandler_name =>
         { parse => 'string', default => 'dhandler', type => SCALAR,
           descr => "The filename to use for Mason's 'dhandler' capability" },

	 interp =>
         { isa => 'HTML::Mason::Interp',
           descr => "An interpreter for Mason control functions",
           public => 0 },

	 error_format =>
         { parse => 'string', type => SCALAR, default => 'text',
           callbacks => { "HTML::Mason::Exception->can( method )'" =>
                          sub { HTML::Mason::Exception->can("as_$_[0]"); } },
           descr => "How error conditions are returned to the caller (brief, text, line or html)" },

	 error_mode =>
         { parse => 'string', type => SCALAR, default => 'fatal',
           regex => qr/^(?:output|fatal)$/,
           descr => "How error conditions are manifest (output or fatal)" },

	 max_recurse =>
         { parse => 'string', default => 32, type => SCALAR,
           descr => "The maximum recursion depth for component, inheritance, and request stack" },

	 out_method =>
         { parse => 'code',type => CODEREF|SCALARREF,
           default => sub { print STDOUT grep {defined} @_ },
           descr => "A subroutine or scalar reference through which all output will pass" },

 	 plugins => { type => ARRAYREF, default => [], parse => 'list',
 		      descr => "Classes or objects to run event hooks around Mason actions",
 		    },

        );

    __PACKAGE__->contained_objects
	(
	 buffer     => { class => 'HTML::Mason::Buffer',
			 delayed => 1,
			 descr => "This class receives component output and dispatches it appropriately" },
	);
}

my @read_write_params;
BEGIN { @read_write_params = qw( autoflush
				 data_cache_api
				 data_cache_defaults
				 dhandler_name
				 error_format
				 error_mode
                                 max_recurse
                                 out_method ); }
use HTML::Mason::MethodMaker
    ( read_only => [ qw( count
			 dhandler_arg
			 interp
			 parent_request
                         plugin_objects
			 request_depth
			 request_comp ) ],

      read_write => [ map { [ $_ => __PACKAGE__->validation_spec->{$_} ] }
                      @read_write_params ]
    );

sub _properties { @read_write_params }

sub new
{
    my $class = shift;
    my $self = $class->SUPER::new(@_);

    %$self = (%$self, buffer_stack => undef,
		      count => 0,
		      dhandler_arg => undef,
	              execd => 0,
		      parent_request => undef,
		      request_depth => 0,
		      stack => undef,
		      wrapper_chain => undef,
		      wrapper_index => undef,
		      notes => {},
	     );

    $self->{request_comp} = delete($self->{comp});
    $self->{request_args} = delete($self->{args});
    if (UNIVERSAL::isa($self->{request_args}, 'HASH')) {
	$self->{request_args} = [%{$self->{request_args}}];
    }
    $self->{count} = ++$self->{interp}{request_count};
    $self->_initialize;
    return $self;
}

# in the future this method may do something completely different but
# for now this works just fine.
sub instance {
    return $HTML::Mason::Commands::m; #; this comment fixes a parsing bug in Emacs cperl-mode
}

sub _initialize {
    my ($self) = @_;
    my $interp = $self->interp;

    local $SIG{'__DIE__'} = \&rethrow_exception;

    eval {
	$self->{buffer_stack} = [];
	$self->{stack} = [];

	# request_comp can be an absolute path or component object.  If a path,
	# load into object.
	my $request_comp = $self->{request_comp};
	my ($path);
	if (!ref($request_comp)) {
	    $request_comp =~ s{/+}{/}g;
	    $self->{top_path} = $path = $request_comp;

	    search: {
		$request_comp = $self->interp->load($path);

		last search unless $self->use_dhandlers;

		# If path was not found, check for dhandler.
		unless ($request_comp) {
		    if ( $request_comp = $interp->find_comp_upwards($path, $self->dhandler_name) ) {
			my $parent_path = $request_comp->dir_path;
			($self->{dhandler_arg} = $self->{top_path}) =~ s{^$parent_path/?}{};
		    }
		}

		# If the component was declined previously in this request,
		# look for the next dhandler up the tree.
		if ($request_comp and $self->{declined_comps}->{$request_comp->comp_id}) {
		    $path = $request_comp->dir_path;
		    unless ($path eq '/' and $request_comp->name eq $self->dhandler_name) {
			if ($request_comp->name eq $self->dhandler_name) {
			    $path =~ s:/[^\/]+$::;
                            $path ||= '/';
			}
		    }
		    redo search;
		}
	    }

	    unless ($self->{request_comp} = $request_comp) {
		top_level_not_found_error "could not find component for initial path '$self->{top_path}'\n";
	    }

	} elsif ( ! UNIVERSAL::isa( $request_comp, 'HTML::Mason::Component' ) ) {
	    param_error "comp ($request_comp) must be a component path or a component object";
	}

 	# construct a plugin object for each plugin class in each request.
 	$self->{plugin_objects} = [];
 	foreach my $plugin (@{ delete $self->{plugins} }) {
 	    my $plugin_object = $plugin;
 	    if (! ref $plugin) {
 	        eval "use $plugin;";
 	        $plugin_object = $plugin->new();
 	    }
 	    push @{$self->{plugin_objects}}, $plugin_object;
 	}

    };

    # Handle errors.
    my $err = $@;
    if ($err and !$self->_aborted_or_declined($err)) {
	$self->_handle_error($err);
	return;
    }

}

sub use_dhandlers
{
    my $self = shift;
    return defined $self->{dhandler_name} and length $self->{dhandler_name};
}

sub alter_superclass
{
    my $self = shift;
    my $new_super = shift;

    my $class = caller;

    my $isa_ref;
    {
        no strict 'refs';
        $isa_ref = \@{"$class\::ISA"};
    }

    # handles multiple inheritance properly and preserve
    # inheritance order
    for ( my $x = 0; $x <= $#{$isa_ref} ; $x++ )
    {
        if ( $isa_ref->[$x]->isa('HTML::Mason::Request') )
        {
            my $old_super = $isa_ref->[$x];

            if ( $old_super ne $new_super )
            {
                $isa_ref->[$x] = $new_super;

                $class->valid_params( %{ $class->valid_params } );
            }

            last;
        }
    }
}

sub exec {
    my ($self) = @_;
    my $interp = $self->interp;

    # Cheap way to prevent users from executing the same request twice.
    if ($self->{execd}++) {
	error "Can only call exec() once for a given request object. Did you want to use a subrequest?";
    }

    # All errors returned from this routine will be in exception form.
    local $SIG{'__DIE__'} = \&rethrow_exception;

    #
    # $m is a dynamically scoped global containing this
    # request. This needs to be defined in the HTML::Mason::Commands
    # package, as well as the component package if that is different.
    #
    local $HTML::Mason::Commands::m = $self;

    # Save context of subroutine for use inside eval.
    my $wantarray = wantarray;
    my @result;
    eval {
	# Create base buffer.
	my $buffer = $self->create_delayed_object( 'buffer', sink => $self->out_method );
	push @{ $self->{buffer_stack} }, $buffer;
        push @{ $self->{buffer_stack} }, $buffer->new_child;

	# Build wrapper chain and index.
	my $request_comp = $self->request_comp;
	my $first_comp;
	{
	    my @wrapper_chain = ($request_comp);

	    for (my $parent = $request_comp->parent; $parent; $parent = $parent->parent) {
		unshift(@wrapper_chain,$parent);
		error "inheritance chain length > " . $self->max_recurse . " (infinite inheritance loop?)"
		    if (@wrapper_chain > $self->max_recurse);
	    }

	    $first_comp = $wrapper_chain[0];
	    $self->{wrapper_chain} = [@wrapper_chain];
	    $self->{wrapper_index} = { map
                                       { $wrapper_chain[$_]->comp_id => $_ }
                                       (0..$#wrapper_chain)
                                     };
	}

	# Get original request_args array reference to avoid copying.
        my $request_args = $self->{request_args};
	{
	    local *SELECTED;
	    tie *SELECTED, 'Tie::Handle::Mason';

	    my $old = select SELECTED;

 	    eval {
 	      foreach my $plugin (@{$self->plugin_objects}) {
 		$plugin->start_request( { request => $self,
                                          args => $request_args,
                                          wantarray => $wantarray
                                        }
                                      );
 	      }
 	    };
 	    if ($@) {
 	      select $old;
 	      rethrow_exception $@;
 	    }

	    if ($wantarray) {
		@result = eval {$self->comp({base_comp=>$request_comp}, $first_comp, @$request_args)};
	    } elsif (defined($wantarray)) {
		$result[0] = eval {$self->comp({base_comp=>$request_comp}, $first_comp, @$request_args)};
	    } else {
		eval {$self->comp({base_comp=>$request_comp}, $first_comp, @$request_args)};
	    }
 
 	    my $error = $@;
 	    
 	    # plugins called in reverse order when exiting.
 	    eval {
 	      foreach my $plugin (reverse @{$self->plugin_objects}) {
 		$plugin->end_request( { request => $self,
                                        args => $request_args,
                                        wantarray => $wantarray,
                                        error => \$error,
                                        return_value => \@result
                                      }
                                    );
 	      }
 	    };
 	    if ($@) {
 	      # plugin errors take precedence over component errors
 	      $error = $@;
 	    }
 
	    select $old;
	    rethrow_exception $error;
	}
    };

    # Handle errors.
    my $err = $@;
    if ($err and !$self->_aborted_or_declined($err)) {
	pop @{ $self->{buffer_stack} };
	pop @{ $self->{buffer_stack} };
	$self->_handle_error($err);
	return;
    }

    # Flush output buffer.
    $self->flush_buffer;
    pop @{ $self->{buffer_stack} };
    pop @{ $self->{buffer_stack} };

    # Purge code cache if necessary. We do this at the end so as not
    # to affect the response of the request as much.
    $interp->purge_code_cache;

    # Return aborted value or result.
    @result = ($err->aborted_value) if $self->aborted($err);
    @result = ($err->declined_value) if $self->declined($err);
    return $wantarray ? @result : defined($wantarray) ? $result[0] : undef;
}

#
# Display or die with error as dictated by error_mode and error_format.
#
sub _handle_error
{
    my ($self, $err) = @_;

    $self->interp->purge_code_cache;

    rethrow_exception $err if $self->is_subrequest;

    # Set error format for when error is stringified.
    if (UNIVERSAL::can($err, 'format')) {
	$err->format($self->error_format);
    }

    # In fatal mode, die with error. In display mode, output stringified error.
    if ($self->error_mode eq 'fatal') {
	rethrow_exception $err;
    } else {
	UNIVERSAL::isa( $self->out_method, 'CODE' ) ? $self->out_method->("$err") : ( ${ $self->out_method } = "$err" );
    }
}

sub subexec
{
    my $self = shift;
    my $comp = shift;

    $self->make_subrequest(comp=>$comp, args=>\@_)->exec;
}

sub make_subrequest
{
    my ($self, %params) = @_;
    my $interp = $self->interp;

    # Coerce a string 'comp' parameter into an absolute path.  Don't
    # create it if it's missing, though - it's required, but for
    # consistency we let exceptions be thrown later.
    $params{comp} = absolute_comp_path($params{comp}, $self->current_comp->dir_path)
	if exists $params{comp} && !ref($params{comp});

    # Give subrequest the same values as parent request for read/write params
    my %defaults = map { ($_, $self->$_()) } $self->_properties;

    unless ( $params{out_method} )
    {
	$defaults{out_method} = sub { $self->top_buffer->receive(@_) };
    }

    # Make subrequest, and set parent_request and request_depth appropriately.
    my $subreq = $interp->make_request(%defaults, %params);
    $subreq->{parent_request} = $self;
    $subreq->{request_depth}  = $self->request_depth+1;
    error "subrequest depth > " . $self->max_recurse . " (infinite subrequest loop?)"
	if $subreq->request_depth > $self->max_recurse;

    return $subreq;
}

sub is_subrequest
{
    my ($self) = @_;

    return $self->parent_request ? 1 : 0;
}

sub clear_and_abort
{
    my $self = shift;

    $self->clear_buffer;
    $self->abort(@_);
}

sub abort
{
    my ($self, $aborted_value) = @_;
    HTML::Mason::Exception::Abort->throw( error => 'Request->abort was called', aborted_value => $aborted_value );
}

#
# Determine whether $err (or $@ by default) is an Abort exception.
#
sub aborted {
    my ($self, $err) = @_;
    $err = $@ if !defined($err);
    return isa_mason_exception( $err, 'Abort' );
}

#
# Determine whether $err (or $@ by default) is an Decline exception.
#
sub declined {
    my ($self, $err) = @_;
    $err = $@ if !defined($err);
    return isa_mason_exception( $err, 'Decline' );
}

sub _aborted_or_declined {
    my ($self, $err) = @_;
    return $self->aborted($err) || $self->declined($err);
}

#
# Return a new cache object specific to this component.
#
sub cache
{
    my ($self, %options) = @_;

    # If using 1.0x cache API, save off options for end of routine.
    my %old_cache_options;
    if ($self->data_cache_api eq '1.0') {
	%old_cache_options = %options;
	%options = ();
    }

    # Combine defaults with options passed in here.
    if ($self->data_cache_defaults) {
	%options = (%{$self->data_cache_defaults}, %options);
    }
    $options{namespace}   ||= compress_path($self->current_comp->comp_id);
    $options{cache_root}  ||= $self->interp->cache_dir;

    # Determine cache_class, adding 'Cache::' in front of user's
    # specification if necessary.
    my $cache_class = $self->interp->cache_dir ? 'Cache::FileCache' : 'Cache::MemoryCache';
    if ($options{cache_class}) {
	$cache_class = $options{cache_class};
	$cache_class = "Cache::$cache_class" unless $cache_class =~ /::/;
	delete($options{cache_class});
    }

    # Now prefix cache class with "HTML::Mason::". This will be a
    # dynamically constructed package that simply inherits from
    # HTML::Mason::Cache::BaseCache and the chosen cache class.
    my $mason_cache_class = "HTML::Mason::$cache_class";
    unless (pkg_loaded($mason_cache_class)) {
	load_pkg('Cache::Cache', '$m->cache requires the Cache::Cache module, available from CPAN.');
	load_pkg($cache_class, 'Fix your Cache::Cache installation or choose another cache class.');
        # need to break up mention of VERSION var or else CPAN/EU::MM can choke when running 'r'
	eval sprintf('package %s; use base qw(HTML::Mason::Cache::BaseCache %s); use vars qw($' . 'VERSION); $' . 'VERSION = 1.0;',
		     $mason_cache_class, $cache_class);
	error "Error constructing mason cache class $mason_cache_class: $@" if $@;
    }

    my $cache = $mason_cache_class->new (\%options)
	or error "could not create cache object";

    # Implement 1.0x cache API or just return cache object.
    if ($self->data_cache_api eq '1.0') {
	return $self->_cache_1_x($cache, %old_cache_options);
    } else {
	return $cache;
    }
}

#
# Implement 1.0x cache API in terms of Cache::Cache.
# Supported: action, busy_lock, expire_at, expire_if, expire_in, expire_next, key, value
# Silently not supported: keep_in_memory, tie_class
#
sub _cache_1_x
{
    my ($self, $cache, %options) = @_;

    my $action = $options{action} || 'retrieve';
    my $key = $options{key} || 'main';
    
    if ($action eq 'retrieve') {
	
	# Validate parameters.
	if (my @invalids = grep(!/^(expire_if|action|key|busy_lock|keep_in_memory|tie_class)$/, keys(%options))) {
	    param_error "cache: invalid parameter '$invalids[0]' for action '$action'\n";
	}

	# Handle expire_if.
	if (my $sub = $options{expire_if}) {
	    if (my $obj = $cache->get_object($key)) {
		if ($sub->($obj->get_created_at)) {
		    $cache->expire($key);
		}
	    }
	}

	# Return the value or undef, handling busy_lock.
	if (my $result = $cache->get($key, ($options{busy_lock} ? (busy_lock=>$options{busy_lock}) : ()))) {
	    return $result;
	} else {
	    return undef;
	}

    } elsif ($action eq 'store') {

	# Validate parameters	
	if (my @invalids = grep(!/^(expire_(at|next|in)|action|key|value|keep_in_memory|tie_class)$/, keys(%options))) {
	    param_error "cache: invalid parameter '$invalids[0]' for action '$action'\n";
	}
	param_error "cache: no store value provided" unless exists($options{value});

	# Determine $expires_in if expire flag given. For the "next"
	# options, we're jumping through hoops to find the *top* of
	# the next hour or day.
	#
	my $expires_in;
	my $time = time;
	if (exists($options{expire_at})) {
	    param_error "cache: invalid expire_at value '$options{expire_at}' - must be a numeric time value\n" if $options{expire_at} !~ /^[0-9]+$/;
	    $expires_in = $options{expire_at} - $time;
	} elsif (exists($options{expire_next})) {
            my $term = $options{expire_next};
            my ($sec, $min, $hour) = localtime($time);
            if ($term eq 'hour') {
		$expires_in = 60*(59-$min)+(60-$sec);
            } elsif ($term eq 'day') {
		$expires_in = 3600*(23-$hour)+60*(59-$min)+(60-$sec);
            } else {
                param_error "cache: invalid expire_next value '$term' - must be 'hour' or 'day'\n";
            }
	} elsif (exists($options{expire_in})) {
	    $expires_in = $options{expire_in};
	}

	# Set and return the value.
	my $value = $options{value};
	$cache->set($key, $value, $expires_in);
	return $value;

    } elsif ($action eq 'expire') {
	my @keys = (ref($key) eq 'ARRAY') ? @$key : ($key);
	foreach my $key (@keys) {
	    $cache->expire($key);
	}

    } elsif ($action eq 'keys') {
	return $cache->get_keys;
    }
}

sub cache_self {
    my ($self, %options) = @_;

    return if $self->top_stack->{in_call_self};

    my (%store_options, %retrieve_options);
    my ($expires_in, $key, $cache);
    if ($self->data_cache_api eq '1.0') {
	foreach (qw(key expire_if busy_lock)) {
	    $retrieve_options{$_} = $options{$_} if (exists($options{$_}));
	}
	foreach (qw(key expire_at expire_next expire_in)) {
	    $store_options{$_} = $options{$_} if (exists($options{$_}));
	}
    } else {
	#
	# key, expires_in/expire_in, expire_if and busy_lock go into
	# the set and get methods as appropriate. All other options
	# are passed into $self->cache.
	#
	foreach (qw(expire_if busy_lock)) {
	    $retrieve_options{$_} = delete($options{$_}) if (exists($options{$_}));
	}
	$expires_in = delete $options{expires_in} || delete $options{expire_in} || 'never';
	$key = delete $options{key} || '__mason_cache_self__';
	$cache = $self->cache(%options);
    }

    my ($output, @retval);

    my $cached =
        ( $self->data_cache_api eq '1.0' ?
          $self->cache(%retrieve_options) :
          $cache->get($key, %retrieve_options)
        );

    if ($cached) {
        $self->top_buffer->remove_filter
            if $self->top_stack->{comp}->has_filter;

        ($output, my $retval) = @$cached;
        @retval = @$retval;
    } else {
        $self->call_self( \$output, \@retval );

        my $value = [$output, \@retval];
        if ($self->data_cache_api eq '1.0') {
            $self->cache(action=>'store', key=>$key, value=>$value, %store_options);
        } else {
            $cache->set($key, $value, $expires_in);
        }
    }

    #
    # Print the component output.
    #
    $self->print($output);

    #
    # Return the component return value in case the caller is interested,
    # followed by 1 indicating the cache retrieval success.
    #
    return (@retval, 1);

}

my $do_nothing = sub {};
sub call_self
{
    my ($self, $output, $retval) = @_;

    return if $self->top_stack->{in_call_self};

    # If the top buffer has a filter we need to remove it because
    # we'll be adding the filter buffer again in a moment.  This is
    # done because we might have been called via cache_self, and we
    # want to capture the _filtered_ component ou caching.
    $self->top_buffer->remove_filter
        if $self->top_stack->{comp}->has_filter;

    unless (defined $output) {
        # don't bother accumulating output that will never be seen
        $output = $do_nothing;
    }

    eval
    {
        my $top_stack = $self->top_stack;
        my $comp = $top_stack->{comp};
        my @args = @{ $top_stack->{args} };

        push @{ $self->{buffer_stack} },
            $self->top_buffer->new_child( sink => $output,
                                          ignore_flush => 1,
                                          ignore_clear => 1,
                                        );

        push @{ $self->{buffer_stack} },
            $self->top_buffer->new_child( filter_from => $comp )
                if $comp->has_filter;

        local $top_stack->{in_call_self} = 1;

        my $wantarray =
	    ( !defined $retval ? undef :
	      UNIVERSAL::isa( $retval, 'ARRAY' ) ? 1 :
	      0
	    );

        eval {
            if ($wantarray) {
                @$retval = $comp->run(@args);
            } elsif (defined $wantarray) {
                $$retval = $comp->run(@args);
            } else {
                $comp->run(@args);
            }
        };

        #
        # Whether there was an error or not we need to pop the buffer
        # stack (twice if we added a filter buffer).
        #
        if ( $comp->has_filter ) {
            my $filter = pop @{ $self->{buffer_stack} };
            $filter->flush;
        }

        pop @{ $self->{buffer_stack} };

        rethrow_exception $@;
    };

    rethrow_exception $@ unless $self->declined;

    return 1;
}

sub call_dynamic {
    my ($m, $key, @args) = @_;
    my $comp = ($m->current_comp->is_subcomp) ? $m->current_comp->owner : $m->current_comp;
    if (!defined($comp->dynamic_subs_request) or $comp->dynamic_subs_request ne $m) {
	$comp->dynamic_subs_init;
	$comp->dynamic_subs_request($m);
    }

    return $comp->run_dynamic_sub($key, @args);
}

sub call_next {
    my ($self,@extra_args) = @_;
    my $comp = $self->fetch_next
	or error "call_next: no next component to invoke";
    return $self->comp($comp, @{$self->current_args}, @extra_args);
}

sub caller
{
    my ($self) = @_;
    return $self->callers(1);
}

#
# Return a specified component from the stack, or the whole stack as a list.
#
sub callers
{
    my ($self, $levels_back) = @_;
    if (defined($levels_back)) {
	my $entry = $self->stack_entry($levels_back);
	return unless defined $entry;
	return $entry->{comp};
    } else {
	return map($_->{comp}, reverse $self->stack);
    }
}

#
# Return a specified argument list from the stack.
#
sub caller_args
{
    my ($self, $levels_back) = @_;
    param_error "caller_args expects stack level as argument" unless defined $levels_back;

    my $entry = $self->stack_entry($levels_back);
    return unless $entry;
    my $args = $entry->{args};
    return wantarray ? @$args : { @$args };
}

sub comp_exists
{
    my ($self, $path) = @_;
    return $self->interp->comp_exists(absolute_comp_path($path, $self->current_comp->dir_path)) ? 1 : 0;
}

sub decline
{
    my ($self) = @_;

    $self->clear_buffer;
    my $subreq = $self->make_subrequest
	(comp => $self->{top_path},
	 args => $self->{request_args},
	 declined_comps => {$self->request_comp->comp_id, 1, %{$self->{declined_comps}}});
    my $retval = $subreq->exec;
    HTML::Mason::Exception::Decline->throw( error => 'Request->decline was called', declined_value => $retval );
}

#
# Return the current number of stack levels. 1 means top level, 0
# means that no component has been called yet.
#
sub depth
{
    my ($self) = @_;

    # direct access for speed because this method is called on every
    # call to $m->comp
    return scalar @{ $self->{stack} };
}

#
# Given a component path (absolute or relative), returns a component.
# Handles SELF, PARENT, REQUEST, comp:method, relative->absolute
# conversion, and local subcomponents.
#
sub fetch_comp
{
    my ($self,$path) = @_;
    param_error "fetch_comp: requires path as first argument" unless defined($path);

    #
    # Handle paths SELF and PARENT
    #
    if ($path eq 'SELF') {
	return $self->base_comp;
    }
    if ($path eq 'PARENT') {
	my $c = $self->current_comp->parent
	    or error "PARENT designator used from component with no parent";
	return $c;
    }
    if ($path eq 'REQUEST') {
        return $self->request_comp;
    }

    #
    # Handle paths of the form comp_path:method_name
    #
    if (index($path,':') != -1) {
	my $method_comp;
	my ($owner_path,$method_name) = split(':',$path,2);
	my $owner_comp = $self->fetch_comp($owner_path)
	    or error "could not find component for path '$owner_path'\n";
	$owner_comp->_locate_inherited('methods',$method_name,\$method_comp)
	    or error "no method '$method_name' for component " . $owner_comp->title;
	return $method_comp;
    }

    #
    # If path does not contain a slash, check for a subcomponent in the
    # current component first.
    #
    if ($path !~ /\//) {
	my $cur_comp = $self->current_comp;
	# Check my subcomponents.
	if (my $subcomp = $cur_comp->subcomps($path)) {
	    return $subcomp;
	}
	# If I am a subcomponent, also check my owner's subcomponents.
	# This won't work when we go to multiply embedded subcomponents...
	if ($cur_comp->is_subcomp and my $subcomp = $cur_comp->owner->subcomps($path)) {
	    return $subcomp;
	}
    }

    #
    # Otherwise pass the absolute path to interp->load.
    #
    # For speed, don't call ->current_comp, instead access it directly
    $path = absolute_comp_path($path, $self->{stack}[-1]{comp}->dir_path)
	unless substr($path, 0, 1) eq '/';

    my $comp = $self->interp->load($path);

    return $comp;
}

#
# Fetch the index of the next component in wrapper chain. If current
# component is not in chain, search the component stack for the most
# recent one that was.
#
sub _fetch_next_helper {
    my ($self) = @_;
    my $index = $self->{wrapper_index}->{$self->current_comp->comp_id};
    unless (defined($index)) {
	my @callers = $self->callers;
	shift(@callers);
	while (my $comp = shift(@callers) and !defined($index)) {
	    $index = $self->{wrapper_index}->{$comp->comp_id};
	}
    }
    return $index;
}

#
# Fetch next component in wrapper chain.
#
sub fetch_next {
    my ($self) = @_;
    my $index = $self->_fetch_next_helper;
    error "fetch_next: cannot find next component in chain"
	unless defined($index);
    return $self->{wrapper_chain}->[$index+1];
}

#
# Fetch remaining components in wrapper chain.
#
sub fetch_next_all {
    my ($self) = @_;
    my $index = $self->_fetch_next_helper;
    error "fetch_next_all: cannot find next component in chain"
	unless defined($index);
    my @wc = @{$self->{wrapper_chain}};
    return @wc[($index+1)..$#wc];
}

sub file
{
    my ($self,$file) = @_;
    my $interp = $self->interp;
    unless ( File::Spec->file_name_is_absolute($file) ) {
        # use owner if current comp is a subcomp
        my $context_comp =
            ( $self->current_comp->is_subcomp ?
              $self->current_comp->owner :
              $self->current_comp );

	if ($context_comp->is_file_based) {
	    my $source_dir = $context_comp->source_dir;
	    $file = File::Spec->catfile( $source_dir, $file );
	} else {
	    $file = File::Spec->catfile( File::Spec->rootdir, $file );
	}
    }
    my $content = read_file($file,1);
    return $content;
}

sub print
{
    my $self = shift;

    # direct access for optimization cause $m->print is called a lot
    $self->{buffer_stack}[-1]->receive(@_);

    # ditto
    $self->flush_buffer if $self->{autoflush};
}

*out = \&print;

#
# Execute the given component
#
sub comp {
    my $self = shift;

    # Get modifiers: optional hash reference passed in as first argument.
    # merge multiple hash references to simplify user and internal usage.
    my %mods = ();
    %mods = (%{shift()},%mods) while ref($_[0]) eq 'HASH';

    my $comp = shift;

    param_error "comp: requires path or component as first argument"
	unless defined($comp);

    #
    # $comp can be an absolute path or component object.  If a path,
    # load into object.
    #
    my $path;
    if (!ref($comp)) {
	$path = $comp;
	$comp = $self->fetch_comp($path)
	    or error "could not find component for path '$path'\n";
    }

    #
    # Check for maximum recursion.
    #
    my $depth = $self->depth;
    error "$depth levels deep in component stack (infinite recursive call?)\n"
        if ($depth >= $self->max_recurse);

    #
    # Determine base_comp (base component for method and attribute inheritance)
    # User may override with { base_comp => $compref }
    # Don't change on SELF:x and PARENT:x calls
    # Assume they know what they are doing if a component ref is passed in
    #
    my $base_comp =
        ( exists($mods{base_comp}) ?
          $mods{base_comp} :
          # access data structure directly to optimize common case
          $self->{stack}[-1]{base_comp}
        );

    unless ( $mods{base_comp} ||	# base_comp override
	     !$path || 		# path is undef if $comp is a reference
	     ($comp->is_subcomp && ! $comp->is_method) ||
	     $path =~ m/^(?:SELF|PARENT|REQUEST)(?:\:..*)?$/ ) {
	$base_comp = ( $path =~ m/(.*):/ ?
		       $self->fetch_comp($1) :
		       $comp );
	$base_comp = $base_comp->owner if $base_comp->is_subcomp;
    }

    # Push new frame onto stack.
    push @{ $self->{stack} }, { comp => $comp,
                                args => \@_,
                                base_comp => $base_comp,
                                content => $mods{content},
                              };

    if ($mods{store}) {
	# This extra buffer is to catch flushes (in the given scalar ref).
	# The component's main buffer can then be cleared without
	# affecting previously flushed output.
        push @{ $self->{buffer_stack} },
            $self->top_buffer->new_child( sink => $mods{store},
                                          ignore_flush => 1,
                                          ignore_clear => 1,
                                        );

        # This extra buffer is here so that flushes get passed through
        # to the parent (storing buffer) but clears can be handled
        # properly as well.
        push @{ $self->{buffer_stack} }, $self->{buffer_stack}[-1]->new_child;
    }

    if ( $comp->has_filter )
    {
        push @{ $self->{buffer_stack} },
            $self->{buffer_stack}[-1]->new_child( filter_from => $comp );
    }

    my @result;

    # The eval block creates a new context so we need to get this
    # here.
    my $wantarray = wantarray;

    # plugins can modify the arguments before the component sees them!
    foreach my $plugin (@{$self->plugin_objects}) {
        $plugin->start_component( { comp => $comp,
                                    args => \@_,
                                    request => $self,
                                    wantarray => $wantarray,
                                  }
				);
    }

    #
    # Finally, call component subroutine.
    #
    eval {
        if ($wantarray) {
            @result = $comp->run(@_);
        } elsif (defined $wantarray) {
            $result[0] = $comp->run(@_);
        } else {
            $comp->run(@_);
        }
    };

    my $err = $@;

    # reverse order for the end hooks.
    foreach my $plugin (reverse @{$self->plugin_objects}) {
      $plugin->end_component( { comp => $comp,
                                args => \@_,
                                request => $self,
                                wantarray => $wantarray,
                                error => \$err,
                                return_value => \@result
                              }
			    );
    }


    if ( $comp->has_filter )
    {
        my $buffer = pop @{ $self->{buffer_stack} };

        # we don't want to send output if there was a real error
        unless ( $err && ! $self->aborted )
        {
            # have to catch errors from filter code too
            $self->print( $buffer->output );
        }
    }

    pop @{ $self->{stack} };

    if ( $mods{store} )
    {
        $self->flush_buffer;

        pop @{ $self->{buffer_stack} };
        pop @{ $self->{buffer_stack} };
    }

    rethrow_exception $err;

    return wantarray ? @result : $result[0];  # Will return undef in void context (correct)
}

#
# Like comp, but return component output.
#
sub scomp {
    my $self = shift;
    my $buf;
    $self->comp({store=>\$buf},@_);
    return $buf;
}

sub has_content {
    my $self = shift;
    return defined($self->top_stack->{content});
}

sub content {
    my $self = shift;
    my $content = $self->top_stack->{content};
    return undef unless defined($content);

    # make the stack frame look like we are still the previous component
    my $old_frame = pop @{ $self->{stack} };

    push @{ $self->{buffer_stack} }, $self->top_buffer->new_child( ignore_flush => 1 );
    eval { $content->(); };
    my $err = $@;

    my $buffer = pop @{ $self->{buffer_stack} };

    push @{ $self->{stack} }, $old_frame;

    rethrow_exception $err;

    return $buffer->output;
}

sub notes {
  my $self = shift;
  return $self->{notes} unless @_;
  
  my $key = shift;
  return $self->{notes}{$key} unless @_;
  
  return $self->{notes}{$key} = shift;
}

sub clear_buffer
{
    my $self = shift;

    $_->clear for $self->buffer_stack;
}

sub flush_buffer
{
    my $self = shift;
    for ($self->buffer_stack) {
	last if $_->ignore_flush;
	$_->flush;
    }
}

sub request_args
{
    my ($self) = @_;
    if (wantarray) {
	return @{$self->{request_args}};
    } else {
	return { @{$self->{request_args}} };
    }
}

# For backward compatibility:
*top_args = \&request_args;
*top_comp = \&request_comp;

# deprecated in 1.1x
sub time
{
    my ($self) = @_;
    my $time = $self->interp->current_time;
    $time = time() if $time eq 'real';
    return $time;
}

#
# Subroutine called by every component while in debug mode, convenient
# for breakpointing.
#
sub debug_hook
{
    1;
}


#
# stack handling
#

# Return the current stack as an array.
sub stack {
    my ($self) = @_;
    return @{ $self->{stack} };
}

# Return the stack entry 'i' slots from the /back/ of the array
sub stack_entry {
    my ($self, $i) = @_;
    return $self->{stack}->[-1 - $i];
}

# Set or retrieve the hashref at the top of the stack.
sub top_stack {
    my ($self,$href) = @_;
    error "top_stack: nothing on component stack"
	unless $self->depth > 0;
    $self->{stack}->[-1] = $href if defined($href);
    return $self->{stack}->[-1];
}

# These push/pop interfaces are not used internally (for speed) but
# may be useful externally.  For example, Component.pm pushes and pops
# a buffer object.
sub push_buffer_stack {
    my $self = shift;

    push @{ $self->{buffer_stack} }, shift;
}

sub pop_buffer_stack {
    my ($self) = @_;
    return pop @{ $self->{buffer_stack} };
}

sub push_stack {
    my ($self,$href) = @_;
    push @{ $self->{stack} }, $href;
}

sub pop_stack {
    my ($self) = @_;
    return pop @{ $self->{stack} };
}

sub buffer_stack {
    my ($self) = @_;
    return wantarray ? reverse @{ $self->{buffer_stack} } : @{ $self->{buffer_stack} };
}


#
# Accessor methods for top of stack elements.
#
sub current_comp { return $_[0]->top_stack->{comp} }
sub current_args { return $_[0]->top_stack->{args} }
sub top_buffer { return $_[0]->{buffer_stack}->[-1] }
sub base_comp { return $_[0]->top_stack->{base_comp} }

package Tie::Handle::Mason;

sub TIEHANDLE
{
    my $class = shift;


    return bless {}, $class;
}

sub PRINT
{
    my $self = shift;

    my $old = select STDOUT;
    # Use direct $m access instead of Request->instance() to optimize common case
    $HTML::Mason::Commands::m->print(@_);

    select $old;
}

sub PRINTF
{
    my $self = shift;

    # apparently sprintf(@_) won't work, it needs to be a scalar
    # followed by a list
    $self->PRINT(sprintf(shift, @_));
}

1;

__END__

=head1 NAME

HTML::Mason::Request - Mason Request Class

=head1 SYNOPSIS

    $m->abort (...)
    $m->comp (...)
    etc.

=head1 DESCRIPTION

The Request API is your gateway to all Mason features not provided by
syntactic tags. Mason creates a new Request object for every web
request. Inside a component you access the current request object via
the global C<$m>.  Outside of a component, you can use the class
method C<instance>.

=head1 COMPONENT PATHS

The methods L<Request-E<gt>comp|HTML::Mason::Request/item_comp>,
L<Request-E<gt>comp_exists|HTML::Mason::Request/item_comp_exists>, and
L<Request-E<gt>fetch_comp|HTML::Mason::Request/item_fetch_comp> take a
component path argument.  Component paths are like URL paths, and
always use a forward slash (/) as the separator, regardless of what
your operating system uses.

=over

=item *

If the path is absolute (starting with a '/'), then the component is
found relative to the component root.

=item *

If the path is relative (no leading '/'), then the component is found
relative to the current component directory.

=item *

If the path matches both a subcomponent and file-based component, the
subcomponent takes precedence.

=back

=head1 PARAMETERS TO THE new() CONSTRUCTOR

=over 4

=item autoflush

True or false, default is false. Indicates whether to flush the output buffer
after every string is output. Turn on autoflush if you need to send partial
output to the client, for example in a progress meter.

=item buffer_class

The class to use when creating buffers. Defaults to
L<HTML::Mason::Buffer|HTML::Mason::Buffer>.

=item data_cache_api

The C<$m-E<gt>cache> API to use. '1.1', the default, indicates the newer
API L<documented in this manual|HTML::Mason::Request/item_cache>.
'1.0' indicates the old API documented in 1.0x and earlier. This
compatibility layer is provided as a convenience for users upgrading
from older versions of Mason, but will not be supported indefinitely.

=item data_cache_defaults

A hash reference of default options to use for the C<$m-E<gt>cache>
command. For example, to use the C<MemoryCache> implementation
by default,

    data_cache_defaults => {cache_class => 'MemoryCache'}

These settings are overriden by options given to particular
C<$m-E<gt>cache> calls.

=item dhandler_name

File name used for L<dhandlers|HTML::Mason::Devel/dhandlers>. Default
is "dhandler".  If this is set to an empty string ("") then dhandlers
are turned off entirely.

=item error_format

Indicates how errors are formatted. The built-in choices are

=over

=item *

I<brief> - just the error message with no trace information

=item *

I<text> - a multi-line text format

=item *

I<line> - a single-line text format, with different pieces of information separated by tabs (useful for log files)

=item *

I<html> - a fancy html format

=back

The default format under L<Apache|HTML::Mason::ApacheHandler> and
L<CGI|HTML::Mason::CGIHandler> is either I<line> or I<html> depending
on whether the error mode is I<fatal> or I<output>, respectively. The
default for standalone mode is I<text>.

The formats correspond to C<HTML::Mason::Exception> methods named
as_I<format>. You can define your own format by creating an
appropriately named method; for example, to define an "xml" format,
create a method C<HTML::Mason::Exception::as_xml> patterned after one of
the built-in methods.

=item error_mode

Indicates how errors are returned to the caller.  The choices are
I<fatal>, meaning die with the error, and I<output>, meaning output
the error just like regular output.

The default under L<Apache|HTML::Mason::ApacheHandler> and
L<CGI|HTML::Mason::CGIHandler> is I<output>, causing the error to be
displayed in the browser.  The default for standalone mode is
I<fatal>.

=item max_recurse

The maximum recursion depth for the component stack, for the request
stack, and for the inheritance stack. An error is signalled if the
maximum is exceeded.  Default is 32.

=item out_method

Indicates where to send output. If out_method is a reference to a
scalar, output is appended to the scalar.  If out_method is a
reference to a subroutine, the subroutine is called with each output
string. For example, to send output to a file called "mason.out":

    my $fh = new IO::File ">mason.out";
    ...
    out_method => sub { $fh->print($_[0]) }

By default, out_method prints to standard output. Under
L<Apache|HTML::Mason::ApacheHandler>, standard output is
redirected to C<< $r->print >>.

=back

=head1 METHODS

=over

=for html <a name="item_abort"></a>

=item abort ([return value])

Ends the current request, finishing the page without returning
through components. The optional argument specifies the return
value from C<Interp::exec>; in a web environment, this ultimately
becomes the HTTP status code.

C<abort> is implemented by throwing an HTML::Mason::Exception::Abort
object and can thus be caught by eval(). The C<aborted> method is a
shortcut for determining whether a caught error was generated by
C<abort>.

If C<abort> is called from a component that has a C<< <%filter> >>,
than any output generated up to that point is filtered, I<unless>
C<abort> is called from a C<< <%shared> >> block.

=for html <a name="item_clear_and_abort"></a>

=item clear_and_abort ([return value])

This method is syntactic sugar for calling C<clear_buffer()> and then
C<abort()>.  If you are aborting the request because of an error, you
will often want to clear the buffer first so that any output generated
up to that point is not sent to the client.

=for html <a name="item_aborted"></a>

=item aborted ([$err])

Returns true or undef indicating whether the specified C<$err>
was generated by C<abort>. If no C<$err> was passed, uses C<$@>.

In this code, we catch and process fatal errors while letting C<abort>
exceptions pass through:

    eval { code_that_may_fail_or_abort() };
    if ($@) {
        die $@ if $m->aborted;

        # handle fatal errors...

C<$@> can lose its value quickly, so if you are planning to call
$m->aborted more than a few lines after the eval, you should save $@
to a temporary variable.

=for html <a name="item_base_comp"></a>

=item base_comp

Returns the current base component for method and attributes.
Initially, the base component is the same as the requested component
(returned by C<< $m->request_comp >>.  However, whenever a component
call is made, the base component changes to the called component,
unless the component call was made uses a component object for its
first argument, or the call starts with SELF:, PARENT:, or REQUEST.

=for html <a name="item_cache"></a>

=item cache (cache_class=>'...', [cache_options])

C<$m-E<gt>cache> returns a new L<cache
object|HTML::Mason::Cache::BaseCache> with a namespace specific to
this component.

I<cache_class> specifies the class of cache object to create. It
defaults to C<FileCache> in most cases, or C<MemoryCache> if the
interpreter has no data directory, and must be a backend subclass of
C<Cache::Cache>. The prefix "Cache::" need not be included.  See the
C<Cache::Cache> package for a full list of backend subclasses.
 
I<cache_options> may include any valid options to the new() method of
the cache class. e.g. for C<FileCache>, valid options include
default_expires_in and cache_depth.

See DEVEL<data caching> for a caching tutorial and examples. See the
L<HTML::Mason::Cache::BaseCache|HTML::Mason::Cache::BaseCache>
documentation for a method reference.

Note: users upgrading from 1.0x and earlier can continue to use the
old C<$m-E<gt>cache> API by setting P<data_cache_api> to '1.0'.  This
support will be removed at a later date.

=for html <a name="item_cache_self"></a>

=item cache_self ([expires_in => '...'], [key => '...'], [get_options], [cache_options])

C<$m-E<gt>cache_self> caches the entire output and return result of a
component.

C<cache_self> either returns undef, or a list containing the
return value of the component followed by '1'. You should return
immediately upon getting the latter result, as this indicates
that you are inside the second invocation of the component.

C<cache_self> takes any of parameters to C<$m-E<gt>cache>
(e.g. I<cache_depth>), any of the optional parameters to
C<$cache-E<gt>get> (I<expire_if>, I<busy_lock>), and two additional
options:

=over

=item *

I<expire_in> or I<expires_in>: Indicates when the cache expires - it
is passed as the third argument to C<$cache-E<gt>set>. e.g. '10 sec',
'5 min', '2 hours'.

=item *

I<key>: An identifier used to uniquely identify the cache results - it
is passed as the first argument to C<$cache-E<gt>get> and
C<$cache-E<gt>set>.  The default key is '__mason_cache_self__'.

=back

To cache the component's output:

    <%init>
    return if $m->cache_self(expire_in => '10 sec'[, key => 'fookey']);
    ... <rest of init> ...
    </%init>

To cache the component's scalar return value:

    <%init>
    my ($result, $cached) = $m->cache_self(expire_in => '5 min'[, key => 'fookey']);

    return $result if $cached;
    ... <rest of init> ...
    </%init>

To cache the component's list return value:

    <%init>
    my (@retval) = $m->cache_self(expire_in => '3 hours'[, key => 'fookey']);

    return @retval if pop @retval;
    ... <rest of init> ...
    </%init>

We call C<pop> on C<@retval> to remove the mandatory '1' at the end of
the list.

If a component has a C<< <%filter> >> block, then the I<filtered>
output is cached.

Note: users upgrading from 1.0x and earlier can continue to use the
old C<$m-E<gt>cache_self> API by setting P<data_cache_api> to '1.0'.
This support will be removed at a later date.

=for html <a name="item_caller_args"></a>

=item caller_args

Returns the arguments passed by the component at the specified stack
level. Use a positive argument to count from the current component and
a negative argument to count from the component at the bottom of the
stack. e.g.

    $m->caller_args(0)   # arguments passed to current component
    $m->caller_args(1)   # arguments passed to component that called us
    $m->caller_args(-1)  # arguments passed to first component executed

When called in scalar context, a hash reference is returned.  When
called in list context, a list of arguments (which may be assigned to
a hash) is returned.  Returns undef or an empty list, depending on
context, if the specified stack level does not exist.

=for html <a name="item_callers"></a>

=item callers

With no arguments, returns the current component stack as a list of
component objects, starting with the current component and ending with
the top-level component. With one numeric argument, returns the
component object at that index in the list. Use a positive argument to
count from the current component and a negative argument to count from
the component at the bottom of the stack. e.g.

    my @comps = $m->callers   # all components
    $m->callers(0)            # current component
    $m->callers(1)            # component that called us
    $m->callers(-1)           # first component executed

Returns undef or an empty list, depending on context, if the specified
stack level does not exist.

=for html <a name="item_caller"></a>

=item caller

A synonym for C<< $m->caller(1) >>, i.e. the component that called the
currently executing component.

=for html <a name="item_call_next"></a>

=item call_next ([args...])

Calls the next component in the content wrapping chain; usually called
from an autohandler. With no arguments, the original arguments are
passed to the component.  Any arguments specified here serve to
augment and override (in case of conflict) the original
arguments. Works like C<$m-E<gt>comp> in terms of return value and
scalar/list context.  See DEVEL<autohandlers> for examples.

=for html <a name="item_call_self"></a>

=item call_self (output, return)

This method allows a component to call itself so that it can filter
both its output and return values.  It is fairly advanced; for most
purposes the C<< <%filter> >> tag will be sufficient and simpler.

C<< $m->call_self >> takes two arguments.  The first is a scalar
reference and will be populated with the component output.  The second
is either a scalar or list reference and will be populated with the
component return value; the type of reference determines whether the
component will be called in scalar or list context.  Both of these
arguments are optional; you may pass undef for either of them.

C<< $m->call_self >> acts like a C<fork()> in the sense that it will
return twice with different values.  When it returns 0, you allow
control to pass through to the rest of your component.  When it
returns 1, that means the component has finished and you can begin
filtering the output and/or return value. (Don't worry, it doesn't
really do a fork! See next section for explanation.)

The following examples would generally appear at the top of a C<<
<%init> >> section.  Here is a no-op C<< $m->call_self >> that leaves
the output and return value untouched:

    <%init>
    my ($output, $retval);
    if ($m->call_self(\$output, \$retval)) {
        $m->print($output);
        return $retval;
    }
    ...

Here is a simple output filter that makes the output all uppercase.
Note that we ignore both the original and the final return value.

    <%init>
    my $output;
    if ($m->call_self(\$output, undef)) {
        $m->print(uc $output);
        return;
    }
    ...

Here is a piece of code that traps all errors occuring anywhere in the
component or its children, e.g. for the purpose of handling
application-specific exceptions. This is difficult to do with a manual
C<eval> because it would have to span multiple code sections and the
main component body.

    <%init>
    # Run this component with an eval around it
    my $in_parent = eval { $m->call_self() };
    if ($@) {
        # check $@ and do something with it
    }
    return if $in_parent;
    ...

=for html <a name="item_clear_buffer"></a>

=item clear_buffer

Clears the Mason output buffer. Any output sent before this line is
discarded. Useful for handling error conditions that can only be
detected in the middle of a request.

clear_buffer is, of course, thwarted by C<flush_buffer>.

=for html <a name="item_comp"></a>

=item comp (comp, args...)

Calls the component designated by I<comp> with the specified
option/value pairs. I<comp> may be a component path or a component
object.

Components work exactly like Perl subroutines in terms of return
values and context. A component can return any type of value, which is
then returned from the C<$m-E<gt>comp> call.

The <& &> tag provides a convenient shortcut for C<$m-E<gt>comp>.

As of 1.10, component calls can accept an initial hash reference of
I<modifiers>.  The only currently supported modifier is C<store>, which
stores the component's output in a scalar reference. For example:

  my $buf;
  my $return = $m->comp( { store => \$buf }, '/some/comp', type => 'big' );

This mostly duplicates the behavior of I<scomp>, but can be useful in
rare cases where you need to capture both a component's output and
return value.

This modifier can be used with the <& &> tag as well, for example:

  <& { store => \$buf }, '/some/comp', size => 'medium' &>

=for html <a name="item_comp_exists"></a>

=item comp_exists (comp_path)

Returns 1 if I<comp_path> is the path of an existing component, 0
otherwise.  That path given may be relative, in which case the current
component's directory path will be prepended.

=for html <a name="content"></a>

=item content

Evaluates the content (passed between <&| comp &> and </&> tags) of the 
current component, and returns the resulting text.

Returns undef if there is no content.

=for html <a name="has_content"></a>

=item has_content

Returns true if the component was called with content (i.e. with <&|
comp &> and </&> tags instead of a single <& comp &> tag). This is
generally better than checking the defined'ness of C<< $m->content >>
because it will not try to evaluate the content.

=for html <a name="item_count"></a>

=item count

Returns the number of this request, which is unique for a given
request and interpreter.

=for html <a name="item_current_comp"></a>

=item current_comp

Returns the current component object.

=for html <a name="item_decline"></a>

=item decline

Used from a top-level component or dhandler, this method clears the
output buffer, aborts the current request and restarts with the next
applicable dhandler up the tree. If no dhandler is available, an error
occurs.  This method bears no relation to the Apache DECLINED status
except in name.

=for html <a name="item_declined"></a>

=item declined ([$err])

Returns true or undef indicating whether the specified C<$err> was
generated by C<decline>. If no C<$err> was passed, uses C<$@>.

=for html <a name="item_depth"></a>

=item depth

Returns the current size of the component stack.  The lowest possible
value is 1, which indicates we are in the top-level component.

=for html <a name="item_dhandler_arg"></a>

=item dhandler_arg

If the request has been handled by a dhandler, this method returns the
remainder of the URI or C<Interp::exec> path when the dhandler directory is
removed. Otherwise returns undef.

C<dhandler_arg> may be called from any component in the request, not just
the dhandler.

=for html <a name="item_exec"></a>

=item exec (comp, args...)

Starts the request by executing the top-level component and
arguments. This is normally called for you on the main request, but
you can use it to execute subrequests.

A request can only be executed once; e.g. it is an error to call this
recursively on the same request.

=for html <a name="item_fetch_comp"></a>

=item fetch_comp (comp_path)

Given a I<comp_path>, returns the corresponding component object or
undef if no such component exists.

=for html <a name="item_fetch_next"></a>

=item fetch_next

Returns the next component in the content wrapping chain, or undef if
there is no next component. Usually called from an autohandler.  See
DEVEL<autohandlers> for usage and examples.

=for html <a name="item_fetch_next_all"></a>

=item fetch_next_all

Returns a list of the remaining components in the content wrapping
chain. Usually called from an autohandler.  See DEVEL<autohandlers>
for usage and examples.

=for html <a name="item_file"></a>

=item file (filename)

Returns the contents of I<filename> as a string. If I<filename> is a
relative path, Mason prepends the current component directory.

=for html <a name="item_flush_buffer"></a>

=item flush_buffer

Flushes the Mason output buffer. Under mod_perl, also sends HTTP
headers if they haven't been sent and calls C<< $r->rflush >> to flush
the Apache buffer. Flushing the initial bytes of output can make your
servers appear more responsive.

Attempts to flush the buffers are ignored within the context of a call
to C<< $m->scomp >> or when output is being stored in a scalar
reference, as with the C< { store =E<gt> \$out } > component call
modifier.

Additionally, if a component has a C<< <%filter> >> block, that
component is buffered until its entire output is generated.  This
means that inside that component and any components that it calls,
the buffer cannot be flushed.

=for html <a name="item_instance"></a>

=item instance

This class method returns the C<HTML::Mason::Request> currently in
use.  If called when no Mason request is active it will return
C<undef>.

If called inside a subrequest, it returns the subrequest object.

=for html <a name="item_interp"></a>

=item interp

Returns the Interp object associated with this request.

=for html <a name="item_make_subrequest"></a>

=item make_subrequest (comp => path, args => arrayref, other parameters)

This method creates a new Request object which inherits its parent's
settable properties, such as P<autoflush> and P<out_method>.  These
values may be overridden by passing parameters to this method.

The C<comp> parameter is required, while all other parameters are
optional.  It may be specified as an absolute path or as a path
relative to the current component.

See DEVEL<subrequests> for more information about subrequests.

=for html <a name="notes"></a>

=item notes (key, value)

The C<notes()> method provides a place to store application data,
giving developers a way to share data among multiple components.  Any
data stored here persists for the duration of the request, i.e. the
same lifetime as the Request object.

Conceptually, C<notes()> contains a hash of key-value pairs.
C<notes($key, $value)> stores a new entry in this hash.
C<notes($key)> returns a previously stored value.  C<notes()> without
any arguments returns a reference to the entire hash of key-value
pairs.

C<notes()> is similar to the mod_perl method C<< $r->pnotes() >>.  The
main differences are that this C<notes()> can be used in a
non-mod_perl environment, and that its lifetime is tied to the
I<Mason> request object, not the I<Apache> request object.  In
particular, a Mason subrequest has its own C<notes()> structure, but
would access the same C<< $r->pnotes() >> structure.

=for html <a name="item_out"></a>

=item out (string)

A synonym for C<$m-E<gt>print>.

=for html <a name="item_print"></a>

=item print (string)

Print the given I<string>. Rarely needed, since normally all text is just
placed in the component body and output implicitly. C<$m-E<gt>print> is useful
if you need to output something in the middle of a Perl block.

In 1.1 and on, C<print> and C<$r-E<gt>print> are remapped to C<$m-E<gt>print>,
so they may be used interchangeably. Before 1.1, one should only use
C<$m-E<gt>print>.

=for html <a name="item_request_args"></a>

=item request_args

Returns the arguments originally passed to the top level component
(see L<request_comp|HTML::Mason::Request/item_request_comp> for
definition).  When called in scalar context, a hash reference is
returned. When called in list context, a list of arguments (which may
be assigned to a hash) is returned.

=for html <a name="item_request_comp"></a>

=item request_comp

Returns the component originally called in the request. Without
autohandlers, this is the same as the first component executed.  With
autohandlers, this is the component at the end of the
C<$m-E<gt>call_next> chain.

=for html <a name="item_scomp"></a>

=item scomp (comp, args...)

Like L<comp|HTML::Mason::Request/item_comp>, but returns the component output as a string
instead of printing it. (Think sprintf versus printf.) The
component's return value is discarded.

=for html <a name="item_subexec"></a>

=item subexec (comp, args...)

This method creates a new subrequest with the specified top-level
component and arguments, and executes it. This is most often used
to perform an "internal redirect" to a new component such that
autohandlers and dhandlers take effect.

=item time

Returns the interpreter's notion of the current time (deprecated).

=back

=head1 APACHE-ONLY METHODS

These additional methods are available when running Mason with mod_perl
and the ApacheHandler.

=over

=for html <a name="item_ah"></a>

=item ah

Returns the ApacheHandler object associated with this request.

=for html <a name="item_apache_req"></a>

=item apache_req

Returns the Apache request object.  This is also available in the
global C<$r>.

=for html <a name="item_auto_send_headers"></a>

=item auto_send_headers

True or false, default is true.  Indicates whether Mason should
automatically send HTTP headers before sending content back to the
client. If you set to false, you should call C<$r-E<gt>send_http_header>
manually.

See DEVEL<sending HTTP headers> for more details about the automatic
header feature.

=back

=head1 CGI-ONLY METHODS

This additional method is available when running Mason with the
CGIHandler module.

=over

=for html <a name="item_cgi_request"></a>

=item cgi_request

Returns the Apache request emulation object, which is available as
C<$r> inside components.

See the L<CGIHandler docs|HTML::Mason::CGIHandler/"$r Methods"> for
more details.

=back

=head1 APACHE- OR CGI-ONLY METHODS

This method is available when Mason is running under either the
ApacheHandler or CGIHandler modules.

=over 4

=for html <a name="item_cgi_object"></a>

=item cgi_object

Returns the CGI object used to parse any CGI parameters submitted to
the component, assuming that you have not changed the default value of
the ApacheHandler P<args_method> parameter.  If you are using the
'mod_perl' args method, then calling this method is a fatal error.
See the L<ApacheHandler|HTML::Mason::ApacheHandler> and
L<CGIHandler|HTML::Mason::CGIHandler> documentation for more details.

=for html <a name="item_redirect_url_status_"></a>

=item redirect ($url, [$status])

Given a url, this generates a proper HTTP redirect for that URL. It
uses C<< $m->clear_and_abort >> to clear out any previous output, and
abort the request.  By default, the status code used is 302, but this
can be overridden by the user.

Since this is implemented using C<< $m->abort >>, it will be trapped
by an C< eval {} > block.  If you are using an C< eval {} > block in
your code to trap errors, you need to make sure to rethrow these
exceptions, like this:

  eval {
      ...
  };

  die $@ if $m->aborted;

  # handle other exceptions

=back

=head1 AUTHORS

Jonathan Swartz <swartz@pobox.com>, Dave Rolsky <autarch@urth.org>, Ken Williams <ken@mathforum.org>

=head1 SEE ALSO

L<HTML::Mason|HTML::Mason>,
L<HTML::Mason::Devel|HTML::Mason::Devel>,
L<HTML::Mason::Component|HTML::Mason::Component>

=cut
