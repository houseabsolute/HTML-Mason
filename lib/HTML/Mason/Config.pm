# Copyright (c) 1998 by Jonathan Swartz. All rights reserved.
# This program is free software; you can redistribute it and/or modify it
# under the same terms as Perl itself.

# --------------------------------------------------------
# MLDBM configuration
# Uncomment pair of lines below to choose your DBM file type and
# persistent storage
# --------------------------------------------------------

#use MLDBM;                           	# defaults to SDBM/Data::Dumper
#$HTML::Mason::DBM_FILE_EXT = '.pag';	# this for file locking

#use MLDBM qw(DB_File Storable);        # this is the fastest, most
#$HTML::Mason::DBM_FILE_EXT = '';	# flexible combination

use MLDBM qw(DB_File Data::Dumper);	# other combinations ...
$HTML::Mason::DBM_FILE_EXT = '';      

# use MLDBM qw(GDBM_File FreezeThaw);
# $HTML::Mason::DBM_FILE_EXT = '';

# --------------------------------------------------------
# Default cache tie class
# Change the following to tie cache files to something other than MLDBM.
# Normally this should be left alone.
# --------------------------------------------------------
#$HTML::Mason::DEFAULT_CACHE_TIE_CLASS = 'MLDBM';
$HTML::Mason::DEFAULT_CACHE_TIE_CLASS = 'CMP::Mason::Serial_DB_File';

# --------------------------------------------------------
# Optional packages
# --------------------------------------------------------

# Mason uses Time::HiRes to record microsecond time values in the
# system log. If this line is commented out, times will be recorded in
# seconds only.

use Time::HiRes;

1;
