use strict;

use File::Basename;
use File::Path;
use File::Spec;

use Module::Build;

my $notes = Module::Build->current->notes;

sub write_comp
{
    my $name = shift;
    my $comp = shift;

    my $file = File::Spec->catfile( $notes->{test_data}{apache_dir}, 'comps', $name );
    my $dir = dirname($file);
    mkpath( $dir, 0, 0755 ) unless -d $dir;

    open F, ">$file"
	or die "Can't write to '$file': $!";

    print F $comp;

    close F;
}

# by wiping out the subdirectories here we can catch permissions
# issues if some of the tests can't write to the data dir.
sub cleanup_data_dir
{
    return if $ENV{MASON_NO_CLEANUP};

    local *DIR;
    my $dir = File::Spec->catdir( $notes->{test_data}{apache_dir}, 'data' );
    opendir DIR, $dir
	or die "Can't open $$dir dir: $!";
    foreach ( grep { -d File::Spec->catdir( $dir, $_ ) && $_ !~ /^\./ } readdir DIR )
    {
	rmtree( File::Spec->catdir( $notes->{test_data}{apache_dir}, 'data', $_ ) );
    }
    closedir DIR;
}

sub get_pid
{
    local *PID;
    my $pid_file = File::Spec->catfile( $notes->{test_data}{apache_dir}, 'httpd.pid' );
    open PID, "<$pid_file"
	or die "Can't open $pid_file: $!";
    my $pid = <PID>;
    close PID;
    chomp $pid;
    return $pid;
}

sub test_load_apache
{
    print STDERR "\nTesting whether Apache can be started\n";
    start_httpd('');
    kill_httpd(1);
}

sub start_httpd
{
    my $def = shift;
    $def = "-D$def" if $def;

    my $httpd = File::Spec->catfile( $notes->{test_data}{apache_dir}, 'httpd' );
    my $conf_file = File::Spec->catfile( $notes->{test_data}{apache_dir}, 'httpd.conf' );
    my $cmd ="$httpd $def -f $conf_file";
    print STDERR "Executing $cmd\n";
    system ($cmd)
	and die "Can't start httpd server as '$cmd': $!";

    my $x = 0;
    print STDERR "Waiting for httpd to start.\n";
    until ( -e 't/httpd.pid' )
    {
	sleep (1);
	$x++;
	if ( $x > 10 )
	{
	    die "No t/httpd.pid file has appeared after 10 seconds.  ",
		"There is probably a problem with the configuration file that was generated for these tests.";
	}
    }
}

sub kill_httpd
{
    my $wait = shift;
    my $pid_file = File::Spec->catfile( $notes->{test_data}{apache_dir}, 'httpd.pid' );
    return unless -e $pid_file;
    my $pid = get_pid();

    print STDERR "\nKilling httpd process ($pid)\n";
    my $result = kill 'TERM', $pid;
    if ( ! $result and $! =~ /no such (?:file|proc)/i )
    {
	# Looks like apache wasn't running, so we're done
	unlink $pid_file
	    or warn "Couldn't remove $pid_file: $!";
	return;
    }
    die "Can't kill process $pid: $!" if !$result;

    if ($wait)
    {
	print STDERR "Waiting for httpd to shut down\n";
	my $x = 0;
	while ( -e $pid_file )
	{
	    sleep (1);
	    $x++;
	    if ( $x > 1 )
	    {
		my $result = kill 'TERM', $pid;
		if ( ! $result and $! =~ /no such (?:file|proc)/i )
		{
		    # Looks like apache wasn't running, so we're done
		    if ( -e $pid_file )
		    {
			unlink $pid_file
			    or warn "Couldn't remove $pid_file: $!";
		    }
		    return;
		}
	    }

	    die "$pid_file file still exists after 10 seconds.  Exiting."
		if $x > 10;
	}

    }
}


1;
