# This is an automake file for NCL.
# Ed Hartnett 8/30/18

# Build this convenience library.
lib_LTLIBRARIES = libncarg_c.la

# Sources for library.
libncarg_c_la_SOURCES = argget.f sbytes.f gbytes.f bcopyswap.c		\
logic32.c options.c misc.c ncarg_path.c version.c error.c atoargv.c	\
usleep.c env.c trnspprt.c lblank.f mangle.c iargct.f rwchinfl.c		\
sdbm.c i1mach.f r1mach.f d1mach.f

# These linker flags specify libtool version info.
# See http://www.gnu.org/software/libtool/manual/libtool.html#Libtool-versioning
# for information regarding incrementing `-version-info`.
libncarg_c_la_LDFLAGS = -version-info 1:0:0

noinst_HEADERS = options.h ncarg_path.h

include_HEADERS = c.h

argget.f:argget.f.sed
	cp $< $*.F
	$(CPP) $(CPPFLAGS) -o $@ $*.F

EXTRA_DIST = c.h.in ncarg_path.c.in argget.f.sed
