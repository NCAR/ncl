# This is an automake file for NCL.
# Ed Hartnett 8/30/18

# Build this convenience library.
lib_LTLIBRARIES = libncarg_c.la

# Sources for convenience library.
libncarg_c_la_SOURCES = sbytes.f gbytes.f bcopyswap.c logic32.c		\
options.c misc.c ncarg_path.c version.c error.c atoargv.c usleep.c	\
env.c trnspprt.c lblank.f iargct.f rwchinfl.c sdbm.c i1mach.f		\
r1mach.f d1mach.f

noinst_HEADERS = options.h ncarg_path.h

include_HEADERS = c.h

EXTRA_DIST = c.h.in ncarg_path.c.in
