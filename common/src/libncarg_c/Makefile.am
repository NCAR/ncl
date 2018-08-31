# This is an automake file for NCL.
# Ed Hartnett 8/30/18

# Build this convenience library.
noinst_LTLIBRARIES = libncarg_c.la

# Sources for convenience library.
libncarg_c_la_SOURCES = gbytes.f sbytes.f bcopyswap.c gsbytes.c		\
logic32.c options.c misc.c ncarg_path.c version.c error.c atoargv.c	\
usleep.c env.c trnspprt.c rwchinfl.c sdbm.c mangle.c sdbm.c lblank.f	\
iargct.f i1mach.f r1mach.f d1mach.f c.h

noinst_HEADERS = options.h

EXTRA_DIST = c.h.in ncarg_path.c.in
