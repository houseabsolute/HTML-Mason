#!/usr/bin/perl
#
# Run the test suite using the .pm files in this directory.
#
use Cwd;

# Put mason/dist/lib first on the include path
chdir('../..');
unshift(@INC,cwd);

# Run test suite from mason/dist directory
chdir('..');
require 't/01-basics.t';
