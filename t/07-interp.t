#!/usr/bin/perl -w
use Cwd;
use vars (qw($root $branch $comp_root $data_dir));

$branch = "interp";
my $pwd = cwd();
$root = (-f "test-common.pl") ? "$pwd/.." : (-f "t/test-common.pl") ? "$pwd" : die "ERROR: cannot find test-common.pl\n";
unshift(@INC,"$root/lib");

require "$root/t/test-common.pl";
init();

sub try_exec_with_interp {
    my ($options,$test,$iteration) = @_;
    # Create new interp based on options.
    my $interp = new HTML::Mason::Interp(comp_root => $comp_root, data_dir => $data_dir, %$options);
    try_exec($interp,$test,$iteration);
}

sub basic_interp {
    return (new HTML::Mason::Interp(comp_root => $comp_root, data_dir => $data_dir));
}

# autohandler_name/allow_recursive_autohandlers
try_exec_with_interp({},'autohandler_test/subdir/hello',1);
try_exec_with_interp({allow_recursive_autohandlers=>1},'autohandler_test/subdir/hello',2);
try_exec_with_interp({autohandler_name=>'plainfile'},'autohandler_test/subdir/hello',3);

# multiple comp_roots
{my @roots = ([main=>$comp_root],[alt=>"$root/test/alt_root/"]);
 try_exec_with_interp({comp_root=>\@roots},'comp_root_test/shared.html');
 try_exec_with_interp({comp_root=>\@roots},'comp_root_test/private1.html');
 try_exec_with_interp({comp_root=>\@roots},'comp_root_test/private2.html');}

# current_time
try_exec_with_interp({current_time=>945526402},'current_time');

# dhandler_name
try_exec_with_interp({dhandler_name=>'plainfile'},'dhandler_test/foo/blag');

# max_recurse
try_exec_with_interp({},'max_recurse_8');
try_exec_with_interp({},'max_recurse_24',1);
try_exec_with_interp({max_recurse=>21},'max_recurse_24',2);

# out_mode
try_exec_with_interp({},'out_mode',1);
try_exec_with_interp({out_mode=>'stream'},'out_mode',2);

# preloads
try_exec_with_interp({},'preloads_test/show_code_cache',1);
try_exec_with_interp({preloads=>['/interp/preloads_test/hello']},'preloads_test/show_code_cache',2);
try_exec_with_interp({preloads=>['/interp/preloads_test/*']},'preloads_test/show_code_cache',3);

# try_exec_with_interp({use_data_cache=>0},'/request/cache');

# set_global
{my $interp = basic_interp();
 $interp->parser->allow_globals(qw($global));
 $interp->set_global(global=>'parsimmon');
 try_exec($interp,'set_global');}

1;
