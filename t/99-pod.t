#!/usr/bin/perl -w
use strict;

use Test::More;

eval "use Test::Pod 1.00";

plan skip_all => "Test::Pod 1.00 not installed." if $@;
all_pod_files_ok('blib');

