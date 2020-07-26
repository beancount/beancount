// Basic test running some operation on mpdecimal data structures.

#include "mpdecimal.h"

#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main(int argc, char **argv)
{
  mpd_context_t ctx;
  mpd_init(&ctx, 38);
  ctx.traps = 0;

  mpd_t* a = mpd_new(&ctx);
  mpd_set_string(a, "4.2", &ctx);
  mpd_t* b = mpd_new(&ctx);
  mpd_set_string(b, "7.4", &ctx);

  mpd_t* result = mpd_new(&ctx);
  mpd_mul(result, a, b, &ctx);

  char status_str[MPD_MAX_FLAG_STRING];
  mpd_snprint_flags(status_str, MPD_MAX_FLAG_STRING, ctx.status);
  char* rstring = mpd_to_sci(result, 1);
  printf("%s  %s\n", rstring, status_str);

  mpd_del(a);
  mpd_del(b);
  mpd_del(result);
  mpd_free(rstring);

  return 0;
}
