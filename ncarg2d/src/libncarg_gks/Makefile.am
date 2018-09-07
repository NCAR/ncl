# This is an automake file for NCL.
# Ed Hartnett 8/31/18

SUBDIRS = awi bwi wiss cdrivers awiC

# Assemble libncarg_gks.
lib_LTLIBRARIES = libncarg_gks.la

# These linker flags specify libtool version info.
# See http://www.gnu.org/software/libtool/manual/libtool.html#Libtool-versioning
# for information regarding incrementing `-version-info`.
libncarg_gks_la_LDFLAGS = -version-info 1:0:0

# These convenience libraries are added together to make the
# libncarg_gks library.
libncarg_gks_la_LIBADD = awi/libncarg_awi.la bwi/libncarg_bwi.la	\
wiss/libncarg_wiss.la awiC/libncarg_awic.la cdrivers/libbasegks.la

libncarg_gks_la_SOURCES = gks.h

# Header get installed in special subdirectory.
ncargdir = $(prefix)/include/ncarg
ncarg_HEADERS = gks.h
