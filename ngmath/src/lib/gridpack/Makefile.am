# This is an automake file for NCL.
# Ed Hartnett 9/5/18

SUBDIRS = cssgrid natgrid dsgrid fitgrid csagrid shgrid

lib_LTLIBRARIES = libngmath.la

libngmath_la_LIBADD = cssgrid/libngmath_cssgrid.la		\
natgrid/libngmath_natgrid.la dsgrid/libngmath_dsgrid.la		\
fitgrid/libngmath_fitgrid.la csagrid/libngmath_csagrid.la	\
shgrid/libngmath_shgrid.la

# We need at least one source file...
libngmath_la_SOURCES = ../ngmath.h
