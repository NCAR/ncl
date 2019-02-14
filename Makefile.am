# This is the main automake file for NCL.
# Ed Hartnett 9/5/18

# This directory stores libtool macros, put there by aclocal.
ACLOCAL_AMFLAGS = -I m4

# Does the user want to build NCL?
if BUILD_NCL
EXTERNAL = external
NI = ni
endif

if BUILD_NGMATH
NGMATH = ngmath
endif

if BUILD_NCARG2D
NCARG2D = ncarg2d
endif

if BUILD_NCARVIEW
NCARVIEW = ncarview
endif

SUBDIRS = $(EXTERNAL) common $(NGMATH) $(NCARG2D) $(NCARVIEW) $(NI)

EXTRA_DIST = COPYING
