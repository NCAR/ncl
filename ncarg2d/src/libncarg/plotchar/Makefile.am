# This is an automake file for NCL
# Ed Hartnett 8/30/18

# Build this convenience library.
noinst_LTLIBRARIES = libncarg_plotchar.la

# Find c.h.
AM_CPPFLAGS = -I$(top_srcdir)/common/src/libncarg_c

# Sources for convenience library.
libncarg_plotchar_la_SOURCES = plchhq.f plchmq.f plchlq.f pcgetc.f	\
pcgeti.f pcgetr.f pcsetc.f pcseti.f pcsetr.f pccchk.f pccffc.f		\
pcdchk.f pcexcd.f pcgptr.f pcgtdi.f pcgtpi.f pcmqlq.f pcsort.f		\
pcblda.f pcbdff.f pccfff.f pcffgd.f pcffgp.f pcffhl.f pcffme.f		\
pcgneg.f pcmpxy.f plchhz.f pcdlsc.f pcgpai.f pchiqu.f pcloqu.f		\
pcmequ.f pcpnwi.f pcrset.f pcfcls.f pcfopn.f pcfred.f bofred.c		\
pcbnrd.c

pcfopn.f: pcfopn.f.sed
	sed -e 's+SED_DBDIR+$(DBDIR)+' <pcfopn.f.sed >pcfopn.f

noinst_HEADER = pcffdx.h  pcffme.h  pcffsp.h

EXTRA_DIST = pcffdx.doc pcffme.doc pcffsp.doc pcffdx.h pcffme.h	\
pcffsp.h pcfopn.f.sed

CLEANFILES = pcfopn.f
