# Copyright (c) 1998,1999 by Jonathan Swartz. All rights reserved.
# This program is free software; you can redistribute it and/or modify it
# under the same terms as Perl itself.

# This is the global configuration file for HTML::Mason.

%HTML::Mason::Config = (
    
    # Default cached tie class. Change this to tie cache files to
    # something other than MLDBM.  Normally this should be left alone.
    #
    'default_cache_tie_class' => 'MLDBM',

    # Automatic file extension used for the DBM format specified
    # below.  For example, this is '' for DB_File and GDBM, '.db'
    # for NDBM, and '.pag' for SDBM. Mason needs to know this so it
    # can stat arbitrary DBM files.
    #
    'mldbm_file_ext'          => '',
    
    # The DBM format used by MLDBM. Ideally this will be one of
    # DB_File or GDBM_File. The other formats (SDBM, ODBM, NDBM) are
    # inadequate for data caching purposes due to size limitations.
    #
    'mldbm_use_db'            => 'DB_File',
	
    # The serializer used by MLDBM. Currently can be set to one of
    # Data::Dumper, Storable, or FreezeThaw.
    #
    'mldbm_serializer'        => 'Data::Dumper',

    # Determines whether to use Time::HiRes to record microsecond time
    # values in the system log. If this is 0, times will be recorded
    # in seconds only.  Typically this should be 1 if and only if
    # Time::HiRes is available.
    #
    'use_time_hires'          => 1,
);
