#!/usr/bin/perl -w

use strict;

# trick ApacheHandler into not dying
$INC{Apache} = 1;
sub Apache::perl_hook { 1 }

use Getopt::Long;

my (@mods, $handler, $server_string );

GetOptions( 'mods=s' => \@mods,
	    'handler=s' => \$handler;
	    'server_string=s' => \$server_string,
	  );
@mods = split /,/, join '', @mods;
@mods = ( qw( HTML::Mason HTML::Mason::Compiler::ToObject HTML::Mason::ApacheHandler ) )
    unless @mods;

$handler ||= 'HTML::Mason::ApacheHandler::c_handler';
$server_string ||= 'HTML::Mason::ApacheHandler::ServerString';

foreach my $mod (@mods)
{
    eval "require $_";
    die $@ if $@;
}

my $code = <<'EOF';
/* Let it henceforth be known that I stole most of this code from Matt
   Sergeant, who developed it for AxKit.  Matt solved all the hard
   problems and I tweaked it for Mason.  So ...

   Copyright 2002 Matt Sergeant and Jonathan Swartz

   - Dave Rolsky (3/21/2002)
*/

#ifndef WIN32
#include <modules/perl/mod_perl.h>
#include <httpd.h>
#include <http_config.h>
#endif
#include "mason_config.h"

/* need atoi */
#include <stdlib.h>


/* may be useful for Mason to preload stuff like LexerClass if its declared */

#ifdef WIN32
#define preload_module(name)
#else
static void preload_module(char **name)
{
  if(ind(*name, ' ') >= 0) return;
  if(**name == '-' && ++*name) return;
  if(**name == '+') ++*name;
  else if(!PERL_AUTOPRELOAD) return;
  if(!PERL_RUNNING()) return;

  maybe_load_module(*name);
}
#endif

extern SV *error_str;

void
maybe_load_module(char * name)
{
  STRLEN len;
  SV * sv_file = module2file(name);
  char * ch_file = SvPV(sv_file, len);

  if(!module_is_loaded(sv_file)) {
    perl_require_pv(ch_file);
    if (SvTRUE(ERRSV)) {
      SvREFCNT_dec(sv_file);
      croak("Mason::load_module failed: %s", SvPV(ERRSV, len));
    }
  }
    SvREFCNT_dec(sv_file);
}

static SV *module2file(char *name)
{
  SV *sv = newSVpv(name,0);
  char *s;
  for (s = SvPVX(sv); *s; s++) {
    if (*s == ':' && s[1] == ':') {
      *s = '/';
      Move(s+2, s+1, strlen(s+2)+1, char);
      --SvCUR(sv);
    }
  }
  sv_catpvn(sv, ".pm", 3);
  return sv;
}

static I32 module_is_loaded(SV *key)
{
  I32 retval = FALSE;
  if((key && hv_exists_ent(GvHV(incgv), key, FALSE)))
    retval = TRUE;
  return retval;
}

void
cleanup_av(void * av_v)
{
  AV * my_av = (AV*)av_v;

  SvREFCNT_dec((SV*)my_av);
}

void
cleanup_hv(void * hv_v)
{
  HV * my_hv = (HV*)hv_v;

  SvREFCNT_dec((SV*)my_hv);
}
EOF

$code . = <<"EOF";

void mason_module_init(server_rec *s, pool *p)
{
  STRLEN len = 0;
  SV * serverstring;
  char * serverstringc;

  serverstring = perl_get_sv("HTML::Mason::ApacheHandler::ServerString", TRUE | GV_ADDMULTI);
  serverstringc = SvPV(serverstring, len);

  ap_add_version_component(serverstringc);
}

static int mason_handler(request_rec *r)
{
  int retval;
  SV * handler_sv = newSVpv($handler, 0);

  ENTER;

  retval = perl_call_handler(handler_sv, (request_rec *)r, Nullav);

  LEAVE;

  SvREFCNT_dec(handler_sv);

  return retval;
}

EOF

my $header .= <<'EOF';
/* Let it henceforth be known that I stole most of this code from Matt
   Sergeant, who developed it for AxKit.  Matt solved all the hard
   problems and I tweaked it for Mason.  So ...

   Copyright 2002 Matt Sergeant and Jonathan Swartz

   - Dave Rolsky (3/21/2002)
*/

#ifdef WIN32
#define _INC_DIRENT
#define DIR void
#endif
#include <EXTERN.h>
#include <perl.h>
#include <XSUB.h>
#ifdef WIN32
#ifdef lstat
#define apache_lstat lstat
#undef lstat
#endif
#include "modules/perl/mod_perl.h"
#ifdef apache_lstat
#undef lstat
#define lstat apache_lstat
#undef apache_lstat
#endif
#else
#include <httpd.h>
#include <http_config.h>
 /* SUNPRO C does not know something about __attribute__ */
 #ifdef __SUNPRO_C
  #include <http_core.h>
  #include <http_main.h>
  #include <http_protocol.h>
  #include <http_request.h>
  #include <http_log.h>
 #endif
#endif


extern module MODULE_VAR_EXPORT XS_Mason;

void remove_module_cleanup(void * ignore);

HV * mason_get_config (mason_dir_config * cfg);

void maybe_load_module (char * name);

EOF


my %specs = HTML::Mason::Container->all_specs;

