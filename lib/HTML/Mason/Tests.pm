package HTML::Mason::Tests;

use strict;

use Cwd;

use File::Path;
use File::Spec;

use HTML::Mason;
use HTML::Mason::Compiler::ToObject;
use HTML::Mason::Tools qw(make_fh);

use Getopt::Long;

use vars qw($VERBOSE $DEBUG @SHARED);

BEGIN
{
    $VERBOSE = $ENV{MASON_DEBUG} || $ENV{MASON_VERBOSE};
    $DEBUG = $ENV{MASON_DEBUG};

    $| = 1;

@SHARED = ( { path => '/shared/check_error',
	      component => <<'EOF',
<% ($error) ? "Error: $error" : "No error!?" %>
<%init>
if ($error) {
  my @lines = split("\n",$error);
  $error = join("\n",@lines[0..$lines-1]);
  $error =~ s{ at .*}{ }g;
}
</%init>
<%args>
$error
$lines=>1
</%args>
EOF
	    },
	    { path => '/shared/display_comp_obj',
	      component => <<'EOF',
Declared args:
% my %decl = %{$comp->declared_args};
% foreach (sort keys %decl) {
<% $_ %><% (defined($decl{$_}->{default})) ? "=>".$decl{$_}->{default} : "" %>
% }

I am <% $comp->is_subcomp ? '' : 'not ' %>a subcomponent.
I am <% $comp->is_file_based ? '' : 'not ' %>file-based.
% if (defined($comp->name)) {
My short name is <% $comp->name =~ /anon/ ? '[anon something]' : $comp->name %>.
% }
% if ($comp->is_subcomp and defined($comp->parent_comp)) {
My parent component is <% $comp->parent_comp->title %>.
% }
% if (defined($comp->dir_path)) {
My directory is <% $comp->dir_path %>.
% }
% my @subkeys = sort keys(%{$comp->subcomps});
I have <% scalar(@subkeys) %> subcomponent(s).
% if (@subkeys) {
Including one called <% $comp->subcomps($subkeys[0])->name %>.
% }
My title is <% $comp->title =~ /anon/ ? '[anon something]' : $comp->title %>.

% if (defined($comp->object_file)) {
%   my ($subfile) = ($comp->object_file =~ m{data/(?:[^/]+/)?(obj/.*?)$});
%   if (defined($subfile)) {
My object file is /.../<% $subfile %>
%   }
% }
% if (defined($comp->path)) {
My path is <% $comp->path %>.
% }
% if (defined($comp->fq_path)) {
My fq_path is <% $comp->fq_path =~ /anon/ ? '[anon something]' : $comp->fq_path %>.
% }
% if ($comp->is_file_based and defined($comp->source_file)) {
%   my ($subfile) = ($comp->source_file =~ m{((alt_root|comps)/.*?)$});
My source file is /.../<% $subfile %>
% }
% if ($comp->is_file_based and defined($comp->source_dir)) {
%   my ($subfile) = ($comp->source_dir =~ m{((alt_root|comps)/.*?)$});
My source dir is /.../<% $subfile %>
% }

<%args>
$comp
</%args>
EOF
	    },
	  );
}

sub new
{
    my $class = shift;
    my %p = @_;

    my $self = bless {}, $class;

    GetOptions( 'create' => \$self->{create},
	      );

    die "No group name provided\n"
	unless exists $p{name};

    die "No description for test group provided\n"
	unless exists $p{description};

    $self->{name} = $p{name};
    $self->{description} = $p{description};

    $self->{support} = [];
    $self->{tests} = [];

    return $self;
}

sub add_support
{
    my $self = shift;
    my %p = @_;

    die "'support' key array member contains no 'path' key\n"
	unless exists $p{path};

    die "'support' key array member contains no 'component' key\n"
	unless exists $p{component};

    push @{ $self->{support} }, \%p;
}

sub add_test
{
    my $self = shift;
    my %p = @_;

    die "no name provided for test\n"
	unless exists $p{name};

    unless ( exists $p{path} )
    {
	$p{path} = $p{call_path} || $p{name};
    }

    my $call_path = "/$self->{name}/";
    $call_path .= exists $p{call_path} ? $p{call_path} : $p{name};
    $p{call_path} = $call_path;
    $p{call_path} =~ s,/+,/,g;

    if ( ref($p{call_args}) eq 'HASH' )
    {
	my @lst = %{$p{call_args}};
	$p{call_args} = \@lst;
    }
    elsif ( !exists($p{call_args}) ) {
	$p{call_args} = [];
    }

    die "'$p{name}' test has no description\n"
	unless exists $p{description};

    die "'$p{name}' test has no component\n"
	unless exists $p{component} || $p{skip_component};

    die "'$p{name}' test has no 'expect' or 'expect_error' key\n"
	unless exists $p{expect} || exists $p{expect_error} || $p{skip_expect} || $self->{create};

    foreach ( qw( lexer_params compiler_params interp_params ) )
    {
	die "$_ must be a hash reference"
	    if exists $p{$_} && ! UNIVERSAL::isa( $p{$_}, 'HASH' );
    }

    push @{ $self->{tests} }, \%p;
}

sub run
{
    my $self = shift;

    die "No tests exist in this group"
	unless @{ $self->{tests} };

    if ($DEBUG)
    {
	print "Will " . ( $self->{create} ? '' : 'not ' ) . "create 'expect' files\n";
    }

    $self->{test_count} = 0;

    eval
    {
	$self->_make_dirs;
	$self->_write_shared_comps;
	$self->_write_support_comps;
	$self->_run_tests;
    };

    $self->_cleanup;

    die $@ if $@;
}

sub _make_dirs
{
    my $self = shift;

    my $comp_root = $self->comp_root;
    my $data_dir = $self->data_dir;

    unless ( -d $self->comp_root )
    {
	print "Making comp_root directory: $comp_root\n" if $DEBUG;
	mkpath( $self->comp_root, 0, 0755 )
	    or die "Unable to make base test directory '$comp_root': $!";
    }

    unless ( -d $self->data_dir )
    {
	print "Making data_dir directory: $data_dir\n" if $DEBUG;
	mkpath( $self->data_dir, 0, 0755 )
	    or die "Unable to make base test directory '$data_dir': $!";
    }
}

sub base_path
{
    my $proto = shift;

    if (ref $proto)
    {
	$proto->{base_path} ||= File::Spec->catdir( cwd(), 'mason_tests' );
	return $proto->{base_path};
    }
    else
    {
	return File::Spec->catdir( cwd(), 'mason_tests' );
    }
}

sub comp_root
{
    my $proto = shift;

    return File::Spec->catdir( $proto->base_path, 'comps' );
}

sub data_dir
{
    my $proto = shift;

    return File::Spec->catdir( $proto->base_path, 'data' );
}

sub _write_shared_comps
{
    my $self = shift;

    return unless @SHARED;

    foreach my $comp ( @SHARED )
    {
	my @path = split m(/), $comp->{path};
	my $file = pop @path;

	my $dir = File::Spec->catdir( $self->comp_root, @path );

	$self->write_comp( $comp->{path}, $dir, $file, $comp->{component} );
    }
}

sub _write_support_comps
{
    my $self = shift;

    unless ( @{ $self->{support} } )
    {
	print "No support comps to create\n" if $DEBUG;
	return;
    }

    foreach my $supp ( @{ $self->{support} } )
    {
	my @path = split m(/), $supp->{path};
	my $file = pop @path;

	my $dir = File::Spec->catdir( $self->comp_root, $self->{name}, @path );

	$self->write_comp( $supp->{path}, $dir, $file, $supp->{component} );
    }
}

sub _write_test_comp
{
    my $self = shift;
    my $test = $self->{current_test};

    my @path = split m(/), $test->{path};
    my $file = pop @path;

    my $dir = File::Spec->catdir( $self->comp_root, $self->{name}, @path );
    unless ( -d $dir )
    {
	print "Making dir: $dir\n" if $DEBUG;
	mkpath( $dir, 0, 0755 )
	    or die "Unable to create directory '$dir': $!";
    }

    $self->write_comp( $test->{path}, $dir, $file, $test->{component} );
}

sub write_comp
{
    my $self = shift;
    my ($path, $dir, $file, $component) = @_;

    unless (-d $dir)
    {
	print "Making dir: $dir\n" if $DEBUG;
	mkpath( $dir, 0, 0755 )
	    or die "Unable to create directory '$dir': $!";
    }

    my $real_file = File::Spec->catfile( $dir, $file );

    print "Making component $path at $real_file\n"
	if $DEBUG;

    my $fh = make_fh();
    open $fh, ">$real_file"
	or die "Unable to write to '$real_file': $!";
    print $fh $component
	or die "Unable to write to '$real_file': $!";
    close $fh
	or die "Unable to write to '$real_file': $!";
}

sub _run_tests
{
    my $self = shift;

    my $count = scalar @{ $self->{tests} };
    print "\n1..$count\n";
    if ($VERBOSE)
    {
	print "Running $self->{name} tests ($count tests): $self->{description}\n";
    }

    my $x = 1;
    foreach my $test ( @{ $self->{tests} } )
    {
	print "Running $test->{name} (#$x): $test->{description}\n"
	    if $VERBOSE;

	$self->{current_test} = $test;
	$self->_write_test_comp unless $test->{skip_component};
	$self->_run_test;

	$x++;
    }
}

sub _run_test
{
    my $self = shift;
    my $test = $self->{current_test};

    my %lexer_params = ( exists $test->{lexer_params} ?
			 %{ $test->{lexer_params} } :
			 ()
		       );
    my %compiler_params = ( exists $test->{compiler_params} ?
			    %{ $test->{compiler_params} } :
			    ()
			  );
    my %interp_params = ( exists $test->{interp_params} ?
			  %{ $test->{interp_params} } :
			  () );
    %interp_params = ( %interp_params, %lexer_params, %compiler_params );

    if ($DEBUG && keys %interp_params)
    {
	print "Interp params:\n";
	while ( my ($k, $v) = each %interp_params)
	{
	    print "  $k => $v\n";
	}
    }

    my $buf;
    my $interp;

    if ($test->{interp})
    {
	$interp = $test->{interp};
	$interp->out_method(\$buf);
    }
    else
    {
	$interp = HTML::Mason::Interp->new( comp_root => $self->comp_root,
					    data_dir  => $self->data_dir,
					    out_method => sub { for (@_) { $buf .= $_ if defined $_ } },
					    %interp_params,
					  );
    }

    print "Calling $test->{name} test with path: $test->{call_path}\n" if $DEBUG;
    $test->{pretest_code}->() if $test->{pretest_code};
    eval { $interp->exec( $test->{call_path}, @{$test->{call_args}} ); };

    if ($@)
    {
	if ( $test->{expect_error} )
	{
	    if ( $@ =~ /$test->{expect_error}/ )
	    {
		return $self->_success
	    }
	    else
	    {
		if ($VERBOSE)
		{
		    print "Got error:\n$@\n...but expected something matching:\n$test->{expect_error}\n";
		}
		return $self->_fail;
	    }
	}
	else
	{
	    print "Unexpected error running $test->{name}:\n$@" if $VERBOSE;
	    return $self->_fail;
	}

    }
    elsif ( $test->{expect_error} )
    {
	print "Expected an error matching '$test->{expect_error}' but no error occurred\n" if $VERBOSE;
	return $self->_fail;
    }

    if ($self->{create})
    {
	print "Results for $test->{name}:\n$buf\n";
	return;
    }

    my $success = $test->{skip_expect} ? 1 : $self->check_output( actual => $buf, expect => $test->{expect} );

    $success ? $self->_success : $self->_fail;
}

sub check_output
{
    my ($self, %p) = @_;

    # Whitespace at end can vary.  (Or rather, it is varying in the tests, and
    # should be made not to vary, but I don't have time to fix it yet.)
    for ($p{actual}, $p{expect}) {  s/\s+$//  }
    my $same = ($p{actual} eq $p{expect});

    if (!$same and $VERBOSE) {
	print "Got ...\n-----\n$p{actual}\n-----\n   ... but expected ...\n-----\n$p{expect}\n-----\n";
    }
    return $same;
}

sub _fail
{
    my $self = shift;
    my $test = $self->{current_test};

    $self->{test_count}++;

    print "Result for $self->{name}: $test->{name}\nnot ok $self->{test_count}\n";
}

sub _success
{
    my $self = shift;
    my $test = $self->{current_test};

    $self->{test_count}++;

    print "Result for $self->{name}: $test->{name}\nok $self->{test_count}\n";
}

#
# We use our own rm_tree, rather than File::Path::rmtree, so that we
# can silently fail to entirely remove directories. On some systems
# .nfs files prevent total removal of directories but should not
# otherwise interfere with tests.
#
sub rm_tree {
    my ($path, $debug) = @_;
    $path =~ s#/$##;
    if (-d $path) {
	local *DIR;
	opendir DIR, $path or warn "Can't open $path: $!";
	while (defined(my $file = readdir DIR)) {
	    next if $file eq '.' or $file eq '..';
	    rm_tree("$path/$file");
	}
	closedir DIR;
	rmdir $path;
    } elsif (-f $path) {
	unlink $path;
    } else {
	warn "Can't find $path to remove";
    }
}

sub _cleanup
{
    my $self = shift;

    rm_tree( $self->{base_path}, $DEBUG ) if $self->{base_path};
}

1;

__END__

=head1 NAME

HTML::Mason::Tests - Test harness for testing Mason

=head1 SYNOPSIS

 use HTML::Mason::Tests;

 my $group = HTML::Mason::Tests->new( name => 'name of group', description => 'tests something' );
 $group->add_test( name => 'foo',
                   description => 'tests foo',
                   component => <<'EOF'
 <%args>
 $foo => 1
 </%args>
 <% $foo %>
 EOF
                   expect => <<'EOF',
 1
 EOF
                 );

=head1 DESCRIPTION

This module is designed to automate as much as possible of the Mason
test suite.  It does tasks like write component files to disk, call
them, compare the actual results to the expected results, and more.
In addition, it also is capable of printing out useful information
about test failures when run in verbose mode.  See the ADDITIONAL RUN
MODES section for more information.

It also makes sure that any given group of tests provides all the
information needed to run them (test names, components and results,
etc.).

Now you have no excuse for writing new tests (and that goes double for
me!).

=head1 METHODS

=head2 new

Takes the following parameters:

=item * name (required)

The name of the entire group of tests.

=item * description (required)

What this group tests.

=head2 add_support

Takes the following parameters:

=over 4

=item * path (required)

The path that other components will expect this component to be
reachable at.  All paths are prepended with the group name.  So '/bar'
as a support component in the 'foo' group's ultimate path would be
'/foo/bar'.

=item * component

Text of the support component.  This parameter must have a value
unless the skip_component parameter is true.

=item * skip_component

If true, then the test harness will not write a component to disk for
this test.

=head2 add_test

Takes the following parameters:

=item * name (required)

The name of this test.

=item * description (required)

What this test is testing.

=item * component (required)

Text of the component.

=item * path (optional)

The path that this component should written to.  As with support
components, this path is prepended with the group's name.  If no path
is given, the value of the name parameter is used.

=item * call_path (optional)

The path that should be used to call the component.  If none is given,
then the value is the same as the path option, if that exists,
otherwise it is /<group name>/<test name>.  If a value is given, it is
still prepended by /<group name>/.

=item * call_args (optional)

The arguments that should be passed to the component, in list or hash
reference form. If none is given, no arguments are passed.

=item * compiler_params

This is a hash reference of parameters to be passed to the Compiler->new
method.

=item * interp_params

This is a hash reference of parameters to be passed to the Interp->new
method.

=item * interp

Provide an HTML::Mason::Interp object to be used for the test.

=back

One of the following three options is required:

=over 4

=item * expect

The text expected as a result of calling the component.  This
parameter is _not_ required when running in L<Create mode|ADDITIONAL
RUN MODES>.

=item * expect_error

A regex containing that will be matched against the error returned
from the component execution.

=item * skip_expect

This causes the component to be run but its output is ignored.
However, if the component execution causes an error this will cause
the test to fail.  This is used in a few situations where it is
necessary to just run a component as part the preparation for another
test.

=head2 run

Run the tests in the group.

=back

=head2 Class methods

These methods are provided since some tests may need to know these
values.

=over 4

=head2 base_path

The base path under which the component root and data directory for
the tests are created.

=head2 comp_root

Returns the component root directory.

=head2 data_dir

Return the data directory

=head2 check_output ( actual => $actual_output, expect => $expected_output )

Given the parameters shown above, this method will check to see if the
two are equal.  If they're not equal, it will print out an error
message attempting to highlight the difference.

=back

=head1 ADDITIONAL RUN MODES

The following additional modes are available for running tests.

=over 4

=head2 Verbose mode

To turn this on, set the environment variables MASON_VERBOSE or
MASON_DEBUG as true or run the tests as 'make test TEST_VERBOSE=1'.
In this mode, the C<run> method will output information about tests as
they are run.  If a test fails, then it will also show the cause of
the failure.

=head2 Debug mode

To turn this on, set the MASON_DEBUG environment variable to a true
value.  In this mode, the C<run> method will print detailed
information of its actions.  This mode includes the output printed in
VERBOSE mode.

=head2 Create mode

If the individual tests are run from the command line with the
'--create' flag, then instead of checking the output of a component,
the test harness will simply output its results.  This allows you to
cut and paste these results back into the test file (assuming they are
correct!).

=back

=cut
