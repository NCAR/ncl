# This is an automake file for NCL
# Ed Hartnett 9/5/18

SUBDIRS = gridpack

ngmath.h: ngmath.h.sed
	cp ngmath.h.sed ngmath.H
	${CPP} ${CPPFLAGS} -E -o ngmath.h ngmath.H

hdrdir = $(prefix)/include/ncarg
hdr_HEADERS = ngmath.h
