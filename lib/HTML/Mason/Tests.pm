package HTML::Mason::Tests;

use strict;

use Cwd;

use File::Path;
use File::Spec;

use HTML::Mason;

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
  $error =~ s{ at ([A-Z]:)?/.*}{ }g;
}
</%init>
<%args>
$error
$lines=>1
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

    my $call_path = "/$self->{name}/";
    $call_path .= exists $p{call_path} ? $p{call_path} : $p{name};
    $p{call_path} = $call_path;
    $p{call_path} =~ s,/+,/,g;

    $p{path} ||= $p{name};

    die "'$p{name}' test has no description\n"
	unless exists $p{description};

    die "'$p{name}' test has no component\n"
	unless exists $p{component};

    die "'$p{name}' test has no 'expect' key\n"
	unless exists $p{expect} || $self->{create};

    foreach ( qw( parser_params interp_params ) )
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

    $self->{base_path} = File::Spec->catdir( cwd(), 'mason_tests' );
    $self->{comp_root} = File::Spec->catdir( $self->{base_path},  'comps' );
    $self->{data_dir} = File::Spec->catdir( $self->{base_path},  'data' );

    print "Making comp_root directory: $self->{comp_root}\n" if $DEBUG;
    mkpath( $self->{comp_root}, 0, 0755 )
	or die "Unable to make base test directory '$self->{comp_root}': $!";

    print "Making data_dir directory: $self->{data_dir}\n" if $DEBUG;
    mkpath( $self->{data_dir}, 0, 0755 )
	or die "Unable to make base test directory '$self->{data_dir}': $!";
}

sub _write_shared_comps
{
    my $self = shift;

    return unless @SHARED;

    foreach my $comp ( @SHARED )
    {
	my @path = split m(/), $comp->{path};
	my $file = pop @path;

	my $dir = File::Spec->catdir( $self->{comp_root}, @path );

	$self->_write_comp( $comp->{path}, $dir, $file, $comp->{component} );
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

	my $dir = File::Spec->catdir( $self->{comp_root}, $self->{name}, @path );

	$self->_write_comp( $supp->{path}, $dir, $file, $supp->{component} );
    }
}

sub _write_test_comp
{
    my $self = shift;
    my $test = $self->{current_test};

    my @path = split m(/), $test->{path};
    my $file = pop @path;

    my $dir = File::Spec->catdir( $self->{comp_root}, $self->{name}, @path );
    unless ( -d $dir )
    {
	print "Making dir: $dir\n" if $DEBUG;
	mkpath( $dir, 0, 0755 )
	    or die "Unable to create directory '$dir': $!";
    }

    $self->_write_comp( $test->{path}, $dir, $file, $test->{component} );
}

sub _write_comp
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

    my $fh = do { local *FH; *FH; };
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
	$self->_write_test_comp;
	$self->_run_test;

	$x++;
    }
}

sub _run_test
{
    my $self = shift;
    my $test = $self->{current_test};

    my %params = ( exists $test->{parser_params} ?
		   %{ $test->{parser_params} } :
		   ()
		 );

    if ($DEBUG && keys %params)
    {
	print "Parser params:\n";
	while ( my ($k, $v) = each %params)
	{
	    print "$k = $v\n";
	}
    }

    my $parser = HTML::Mason::Parser->new( %params );

    %params = ( exists $test->{interp_params} ?
		%{ $test->{interp_params} } :
		() );

    if ($DEBUG && keys %params)
    {
	print "Interp params:\n";
	while ( my ($k, $v) = each %params)
	{
	    print "$k = $v\n";
	}
    }

    my $buf;
    my $interp = HTML::Mason::Interp->new( comp_root => $self->{comp_root},
					   data_dir  => $self->{data_dir},
					   out_method => \$buf,
					   %params,
					   parser => $parser );

    print "Calling $test->{name} test with path: $test->{call_path}\n" if $DEBUG;

    eval { $interp->exec( $test->{call_path} ); };

    if ($@)
    {
	print "Error running $test->{name}: $@" if $VERBOSE;
	return $self->_fail($test);
    }

    if ($self->{create} )
    {
	print "Results for $test->{name}:\n$buf\n";
	return;
    }

    my $success = $self->_check_output($buf);

    $success ? $self->_success : $self->_fail;
}

sub _check_output
{
    my $self = shift;
    my $buf = shift;
    my $test = $self->{current_test};

    my @actual = split /\n/, $buf;
    my @expect = split /\n/, $test->{expect};

    my $diff;
    if (@actual > @expect)
    {
	$diff = @actual - @expect;
	if ($VERBOSE)
	{
	    print "Actual result contained $diff extra lines.\n";
	}
    }
    elsif (@expect < @actual)
    {
	$diff = @expect - @actual;
	if ($VERBOSE)
	{
	    print "Actual result contained $diff too few lines.\n";
	}
    }

    my $actual_prev = ();
    my $expect_prev = ();
    my $limit = @actual < @expect ? @actual : @expect;
    my $line = 0;
    for ( my $x = 0; $x < $limit; $x++ )
    {
	$line++;
	if ( $actual[$x] ne $expect[$x] )
	{
	    if ($VERBOSE)
	    {
		print "Result differed from expected output at line $line\n";

		my $actual = join "\n", ( $actual_prev,
					  $actual[$x],
					  $actual[$x + 1] ? $actual[$x + 1] : () );
		my $expect = join "\n", ( $expect_prev,
					  $expect[$x],
					  $expect[$x + 1] ? $expect[$x + 1] : () );
		print "Got ...\n<<<<<\n$actual\n>>>>>\n... but expected ...\n<<<<<\n$expect\n>>>>>\n";
	    }
	    $diff = 1;
	    last;
	}
	$actual_prev = $actual[$x];
	$expect_prev = $expect[$x];
    }

    return ! $diff;
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

sub _cleanup
{
    my $self = shift;

    rmtree ($self->{base_path}, $DEBUG) if $self->{base_path};
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

=over 4

=item * new

Takes the following parameters:

=item -- name (required)

The name of the entire group of tests.

=item -- description (required)

What this group tests.

=item * add_support

Takes the following parameters:

=item -- path (required)

The path that other components will expect this component to be
reachable at.  All paths are prepended with the group name.  So '/bar'
as a support component in the 'foo' group's ultimate path would be
'/foo/bar'.

=item -- component (required)

Text of the support component.

=item * add_test

Takes the following parameters:

=item -- name (required)

The name of this test.

=item --description (required)

What this test is testing.

=item -- component (required)

Text of the component.

=item -- expect (required)

The text expected as a result of calling the component.  This
parameter is _not_ required when running in L<Create mode|ADDITIONAL
RUN MODES>.

=item -- path (optional)

The path that this component should written to.  As with support
components, this path is prepended with the group's name.  If no path
is given, the value of the name parameter is used.

=item -- call_path (optional)

The path that should be used to call the component.  If none is given,
then the value is /<group name>/<test name>.  If a value is given, it
is still prepended by /<group name>/.

=item -- parser_params

This is a hash reference of parameters to be passed to the Parser->new
method.

=item -- interp_params

This is a hash reference of parameters to be passed to the Interp->new
method.

=item * run

Run the tests in the group.

=back

=head1 ADDITIONAL RUN MODES

The following additional modes are available for running tests.

=over 4

=item * Verbose mode

To turn this on, set the environment variables MASON_VERBOSE or
MASON_DEBUG as true or run the tests as 'make test TEST_VERBOSE=1'.
In this mode, the C<run> method will output information about tests as
they are run.  If a test fails, then it will also show the cause of
the failure.

=item * Debug mode

To turn this on, set the MASON_DEBUG environment variable to a true
value.  In this mode, the C<run> method will print detailed
information of its actions.  This mode includes the output printed in
VERBOSE mode.

=item * Create mode

If the individual tests are run from the command line with the
'--create' flag, then instead of checking the output of a component,
the test harness will simply output its results.  This allows you to
cut and paste these results back into the test file (assuming they are
correct!).

=back

=cut
