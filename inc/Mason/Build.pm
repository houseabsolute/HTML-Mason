# sneakily introduce new namespace ;)
package Mason::Build;

use strict;

use Module::Build 0.20;

use base 'Module::Build';

use File::Basename;
use File::Find;
use File::Path;

sub ACTION_build
{
    my $self = shift;

    $self->depends_on('params_pod');

    $self->SUPER::ACTION_build(@_);

    # This has to be done to the blib files or else if we run this
    # from our local repositories we end up modifying those files.
    $self->_convert_custom_pod('blib');
}

# trick ApacheHandler into not dying
sub Apache::perl_hook { 1 }
sub Apache::server { 0 }

use strict;

sub ACTION_params_pod
{
    my $self = shift;

    my $params_pod =
	File::Spec->catfile( $self->_lib_dir, 'HTML', 'Mason', 'Params.pod' );

    my $comp = File::Spec->catfile( $self->base_dir, 'inc', 'params.mtxt' );

    return if $self->up_to_date( [ $comp,
				   $self->_files_with_pod('lib') ],
				 $params_pod
			       );

    # make sure we get distro's modules
    local @INC = ( $self->_lib_dir, @INC );

    require Data::Dumper;

    eval
    {
	require HTML::Mason;
	require HTML::Mason::Compiler::ToObject;
	require HTML::Mason::ApacheHandler;
	require HTML::Mason::Tools;
    };

    if ($@)
    {
	warn "Cannot load Mason modules: $@\n";
	warn "Skipping generation of HTML::Mason::Params document\n";
	return;
    }

    my @params = $self->_find_params;
    my $pod = $self->_run_params_comp(@params);

    my $fh = HTML::Mason::Tools::make_fh();
    open $fh, ">$params_pod"
	or die "Cannot write to $params_pod";
    print $fh $pod
	or die "Cannot write to $params_pod";
    close $fh;

    $self->add_to_cleanup($params_pod);
}

sub _find_params
{
    my $self = shift;

    my %specs = Class::Container->all_specs;
    my %params;
    my %pod;

    foreach my $class_file ( $self->_files_with_params )
    {
	my $pod_text = HTML::Mason::Tools::read_file($class_file);

	while ($pod_text =~ /=item ([a-z_]+)\n\n(.*?)\n(?==item [a-z_]+\n|=back\s+=head)/sg)
	{
	    my ($name, $desc) = ($1, $2);
	    next if exists($pod{$name});
	    chomp($pod{$name} = $desc);
	}
    }

    foreach my $class ( sort keys %specs )
    {
	foreach my $name ( sort keys %{ $specs{$class}{valid_params} } )
	{
	    my $param = $specs{$class}{valid_params}{$name};
	    next unless $param->{public};
	    next if $param->{type} eq 'object';

	    $param->{name} = $name;
	    $param->{class} = $class;
	    $param->{default} = 'Varies' if $name =~ /^(?:comp_root|error_format|error_mode)$/;
	    $param->{default} = 'Print to STDOUT' if $name eq 'out_method';
	    $param->{pod} = $pod{$name} or die "could not find pod entry for $name\n";
	    $params{$name} = $param;
	}

	foreach my $obj ( sort keys %{ $specs{$class}{contained_objects} } )
	{
	    my $name = $obj . '_class';
	    my $default = $specs{$class}{contained_objects}{$obj}{class};
	    my $is_delayed = $specs{$class}{contained_objects}{$obj}{delayed};
	    $params{$name} = {
		name => $name,
		type => 'string',
		class => $class,
		default => $default,
		is_delayed => $is_delayed,
		pod => $pod{$name},
		public => 1,
	    };
	}
    }

    foreach my $spec (sort values %params)
    {
	(my $studly = $spec->{name}) =~ s/(?:^|_)(\w)/\U$1/g;
	$spec->{apache_name} =
	    HTML::Mason::ApacheHandler->studly_form( $spec->{name} );
    }

    return map { $params{$_} } sort keys %params;
}

sub _lib_dir
{
    my $self = shift;

    my $lib_dir = File::Spec->catdir( $self->base_dir, 'lib' );
}

sub _files_with_params
{
    my $self = shift;

    my @files;
    foreach my $class ( qw( ApacheHandler Compiler Compiler/ToObject
                            Interp Request Resolver/File ) )
    {
	my @class_pieces = split /\//, $class;
	$class_pieces[-1] .= '.pm';

	push @files,
	    File::Spec->catfile( $self->_lib_dir, 'HTML', 'Mason', @class_pieces );
    }

    return @files;
}

sub _run_params_comp
{
    my $self = shift;
    my @params = @_;

    my $buf;
    my $interp = HTML::Mason::Interp->new( out_method => \$buf );

    $interp->exec( '/inc/params.mtxt',
		   params => \@params,
		   pods => [ $self->_files_with_pod('lib') ],
		 );

    $buf =~ s/\s+$//s;

    return $buf;
}

sub _files_with_pod
{
    my $self = shift;
    my $dir = shift;

    return
	( grep { $self->contains_pod($_) }
	  @{ $self->rscan_dir( $dir, qr{\.pm$} ) },
	  @{ $self->rscan_dir( $dir, qr{\.pod$} ) }
	);
}

sub _convert_custom_pod
{
    my $self = shift;
    my $dir = shift;

    print "Converting custom POD tags in files under $dir\n";

    foreach my $file ( $self->_files_with_pod($dir) )
    {
	$self->_convert_pod_in_file($file);
    }
}

sub _convert_pod_in_file
{
    my $self = shift;
    my $file = shift;

    my $fh = do { local *FH; *FH; };

    open $fh, "<$file" or die "Cannot read $file: $!";
    local $_ = join '', <$fh>;
    close $fh;

    # WARNING - these regexes are run over entire .pm and .pod files -
    # be careful!

    # Convert custom P<> tags to appropriate params link
    s{P<([a-z_]+)>}
     {L<$1|HTML::Mason::Params/$1>}g;

    # Convert custom DEVEL<> tags to appropriate developer's manual link
    s{DEVEL<([\w \n]+)>}
     {the L<$1|HTML::Mason::Devel/$1> section of the developer\'s manual}mg;

    # Convert custom ADMIN<> tags to appropriate administrator's manual link
    s{ADMIN<([\w \n]+)>}
     {the L<$1|HTML::Mason::Admin/$1> section of the administrator\'s manual}mg;

    $self->_make_writeable($file);

    open $fh, ">$file" or die "Cannot write to $file: $!";
    print $fh $_;
    close $fh;
}

sub _make_writeable
{
    my $self = shift;
    my $file = shift;

    unless ( -w $file )
    {
	my $mode = (stat $file)[2];

	# let user & group write the darn thing
	$mode |= 0220;

	chmod $mode, $file
	    or die "Can't make $file writeable: $!";
    }
}

sub ACTION_dist
{
    my $self = shift;

    $self->depends_on('params_pod');
    $self->depends_on('manifest');
    $self->depends_on('distdir');

    $self->_convert_custom_pod( File::Spec->catdir( $self->dist_dir, 'lib' ) );

    my @files = $self->_generate_html_docs( $self->dist_dir );

    my $dist_manifest =	File::Spec->catfile( $self->dist_dir, 'MANIFEST' );

    # Nice use of undocumented globals.  I hate the EU::* code!
    local $ExtUtils::Manifest::MANIFEST = $dist_manifest;

    $self->_make_writeable($dist_manifest);

    ExtUtils::Manifest::maniadd( { map { $_ => '' } @files } );

    $self->_cleanup_changes_file;

    my $dist_dir = $self->dist_dir;

    $self->make_tarball($dist_dir);
    $self->delete_filetree($dist_dir);
}

sub ACTION_manifest
{
    my $self = shift;

    $self->SUPER::ACTION_manifest(@_);

    # We always generate MANIFEST when making dist
    $self->add_to_cleanup('MANIFEST');
}

sub ACTION_html_docs
{
    my $self = shift;

    $self->depends_on('build');

    $self->_generate_html_docs( File::Spec->catdir( $self->base_dir, 'blib' ),
				File::Spec->catdir( $self->base_dir ),
			      );
}

sub _generate_html_docs
{
    my $self = shift;
    my $dir = shift;
    my $target_dir = shift || $dir;

    require File::Temp;

    # should use something less sucky
    require Pod::Html;

    my $html_dir = File::Spec->catdir( $target_dir, 'htdocs' );

    my @files;
    foreach my $file ( $self->_files_with_pod( File::Spec->catdir( $dir, 'lib' ) ) )
    {
	my $html_file = $self->_pod_from_html( $file, $html_dir );

	my $rel_path = File::Spec->abs2rel( $html_file, $target_dir );

	push @files, $rel_path;
    }

    $self->_check_html_doc_links($html_dir);

    $self->add_to_cleanup( map { File::Spec->catfile( $self->base_dir, $_ ) }
			   'pod2htmd.x~~', 'pod2htmi.x~~' );

    return @files;
}

sub _pod_from_html
{
    my $self = shift;
    my ( $pod_file, $out_dir ) = @_;

    die "could not find $pod_file" unless -f $pod_file;

    # Determine html filename - will break if run on non-Unix
    my ($base) = ($pod_file =~ m/\/HTML\/(.*)\.(?:pm|pod)/);
    return unless $base;

    $base =~ s{Mason/}{};
    my $html_file = "$out_dir/$base.html";

    # Convert to html with pod2html
    print "$pod_file => $html_file\n";
    my ($rawfh, $raw_html_file) = File::Temp::tempfile();
    Pod::Html::pod2html("--infile=$pod_file", "--outfile=$raw_html_file");

    # Fix braindead pod links
    mkpath(dirname($html_file));

    my $htmlfh = do { local *FH; *FH };
    open $htmlfh, ">$html_file" or die "cannot write to $html_file: $!";

    while (<$rawfh>) {
	my $base_dir = dirname($base);
	if ($base_dir eq '.') {
	    s|HREF="/HTML/Mason/([^\"]+)"|HREF="$1"|gi;
	} else {
	    s|HREF="/HTML/Mason/([^\"]+)"|HREF="../$1"|gi;
	    s|HREF="/HTML/Mason.html"|HREF="../Mason.html"|gi;
	    s|HREF="$base_dir/([^\"]+)"|HREF="$1"|gi;
	}
	s|HREF="/HTML/Mason.html"|HREF="Mason.html"|gi;
	s/A HREF="([^\"\#]*)\#([^\"]+)"/"A HREF=\"$1\#" . $self->_escape_link($2) . "\""/gie;
	s/A NAME="([^\"]+)"/"A NAME=\"" . $self->_escape_link($1) . "\""/gie;
	print $htmlfh $_  or die "cannot write to $html_file: $!";
    }

    unlink $raw_html_file
	or die "Cannot unlink $raw_html_file: $!";

    $self->add_to_cleanup($html_file);

    return $html_file;
}

# This is how pod2html used to escape its links. It may not be
# necessary anymore, but then again some browsers may choke on the
# full unfettered link, and our swish_index depends on links looking
# like this.
sub _escape_link
{
    my $self = shift;

    my ($link) = @_;
    for ($link) { s/^\s+//; s/\s+$// }
    $link = substr($link, 0, 32);
    $link =~ s/\W+/_/g;
    return $link;
}

sub _check_html_doc_links
{
    my $self = shift;
    my $html_dir = shift;

    print "running linklint\n";
    my $lfh = do { local *FH; *FH };

    # magic incantation
    open $lfh, "linklint -limit 100000 -error -root $html_dir -xref /@ 2>&1 |"
        or die "Cannot open pipe to linklint: $!";

    my $output = do { local $/; <$lfh> };

    if (my ($error_log) = ($output =~ /(\#-+\n\# ERROR.*)/s))
    {
	print $error_log;
	die "linklint had errors";
    }
}

sub _cleanup_changes_file
{
    my $self = shift;

    # Read contents of Changes
    my $changes = File::Spec->catfile( $self->dist_dir, 'Changes' );

    my $fh = do { local *FH; *FH };
    open $fh, $changes or die "could not read $changes: $!";
    my $buf = do { local $/ = undef; <$fh> };
    close $fh;

    # Look for release numbers with no date, as we forget this often
    if (my ($relnum) = ($buf =~ /^(\d\.[\.\d]+)\s*\n/m))
    {
        warn "found release number with no date: $relnum\n";
    }

    # Find first release number
    my $i = index($buf, "1.");

    # Remove L{} documentation links; these are only for web site
    $buf =~ s/\s*L\{([^\{\}]+)\}\s*/\n/g;

    # Indent lines beginning with "- " and "[ [A-Z]" with four spaces
    $buf =~ s/^(\- |\[ [A-Z])/    $1/mg;

    # Indent everything else with six spaces
    substr($buf,$i) =~ s/^(\S)/      $1/mg;

    # Don't indent release titles at all
    $buf =~ s/\n\n\s+(\d\.[\.\d]+\s*)/\n\n$1/mg;

    $self->_make_writeable($changes);

    # Write out changed Changes
    open $fh, ">$changes" or die "could not write $changes: $!";
    print $fh $buf or die "could not write $changes: $!";
    close $fh;
}

sub ACTION_test
{
    my $self = shift;

    $self->notes( test_data => { apache_dir => $self->{args}{apache}{apache_dir},
                                 port       => $self->{args}{apache}{port},
                                 is_maintainer => -d 'CVS' ? 1 : 0,
                               } );

    $self->SUPER::ACTION_test;
}

sub ACTION_install
{
    my $self = shift;

    $self->SUPER::ACTION_install;
    $self->depends_on('delete_old_pods');
    $self->depends_on('configure_apache');

    $self->run_perl_script('install/delete_old_pods.pl');  # These could probably be separate actions.
    $self->run_perl_script('install/configure_apache.pl');
}

sub ACTION_delete_old_pods
{
    my $self = shift;

    foreach my $dir (@INC) {
        foreach my $pm ( qw( Interp ApacheHandler Request Component ) ) {
            my $pod_file = File::Spec->catfile( $dir, 'HTML', 'Mason', "$pm.pod" );

	    if ( -e $pod_file ) {
		warn "Removing obsolete documentation file $pod_file\n";
		unlink $pod_file or warn "Cannot unlink $pod_file: $!";
	    }
	}
    }
}

sub ACTION_configure_apache
{
    my $self = shift;

    my $params_file = File::Spec->catfile( $self->build_dir, 'apache_install.txt' );

    return unless -e $params_file;

    my %params = $self->_read_apache_config_params($params_file);
    my $mason_conf = $self->_write_mason_conf(%params);
    $self->_alter_httpd_conf( mason_config_file => $mason_conf, %params );
}

sub _read_apache_config_params
{
    my $self = shift;
    my $params_file = shift;

    local *INST;
    open INST, "<$params_file";
    my $install;
    eval join '', <INST>;
    die $@ if $@;

    return %$install;
}

sub _write_mason_conf
{
    my $self = shift;
    my %params = @_;

    my $conf = <<"EOF";
PerlSetVar MasonCompRoot "$params{comp_root}"
PerlSetVar MasonDataDir  "$params{data_dir}"
PerlModule HTML::Mason::ApacheHandler

<Directory "$params{comp_root}">
EOF

    if ( $params{extensions} )
    {
	my $ext_re = '(';
	$ext_re .= join '|', map { "\\.$_" } @{ $params{extensions} };
	$ext_re .= ')$';

	$conf .= qq|  <LocationMatch "$ext_re">\n|;
    }

    $conf .= <<"EOF";
    SetHandler perl-script
    PerlHandler HTML::Mason::ApacheHandler
EOF
    $conf .= "  </LocationMatch>\n" if $params{extensions};

    $conf .= "</Directory>\n";

    my $conf_dir = dirname( $params{apache_config_file} );
    my $conf_file = File::Spec->catfile( $conf_dir, 'mason.conf' );

    local *CONF;
    open CONF, ">$conf_file" or die "Cannot write $conf_file: $!";
    print CONF $conf;
    close CONF or die "Can't close $conf_file: $!";

    return $conf_file;
}

sub _alter_httpd_conf
{
    my $self = shift;
    my %params = @_;

    local *CONF;
    open CONF, "<$params{apache_config_file}"
	or die "Can't read $params{apache_config_file}: $!";

    my $new = '';
    while (<CONF>)
    {
	if ( /^# Mason config/ )
	{
	    my $skip = <CONF>; # just eat another line
	    next;
	}
	$new .= $_;
    }
    # clear off last two newlines otherwise file will add one extra
    # blank line every time this script runs
    chomp $new;
    chomp $new;
    $new .= "\n\n# Mason config\nInclude $params{mason_config_file}\n";

    close CONF or die "Can't close $params{apache_config_file}: $!";

    open CONF, ">$params{apache_config_file}"
	or die "Can't write to $params{apache_config_file}: $!";
    print CONF $new
	or die "Can't write to $params{apache_config_file}: $!";
    close CONF or die "Can't close $params{apache_config_file}: $!";
}

1;

__END__

