#!/usr/bin/perl -w
use strict;

use File::Spec;
use Test::More;

eval "use Test::Pod 1.20";

plan skip_all => "Test::Pod 1.20 not installed." if $@;
all_pod_files_ok();

