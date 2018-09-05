# This is the main automake file for NCL.
# Ed Hartnett 9/5/18

# This directory stores libtool macros, put there by aclocal.
ACLOCAL_AMFLAGS = -I m4

# Does the user want to build NCL?
if BUILD_NCL
EXTERNAL = external
NI = ni
endif

SUBDIRS = $(EXTERNAL) common ncarg2d ngmath ncarview $(NI)

EXTRA_DIST = COPYING
