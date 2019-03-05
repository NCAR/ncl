# This is an automake file for NCL.
# Ed Hartnett 8/31/18

# Build this convenience library.
noinst_LTLIBRARIES = libncarg_bwi.la

# Find gks.h.
AM_CPPFLAGS = -I$(top_srcdir)/ncarg2d/src/libncarg_gks

# Find c.h.
AM_CPPFLAGS += -I$(top_srcdir)/common/src/libncarg_c

# Find ncargC.h.
AM_CPPFLAGS += -I$(top_srcdir)/ncarg2d/src/libncarg

# Sources for convenience library.
libncarg_bwi_la_SOURCES = g01bkd.f g01ca.f g01clp.f g01clw.f g01ctb.f	\
g01d2r.f g01d2s.f g01diq.f g01dmp.f g01esc.f g01fa.f g01flb.f		\
g01iac.f g01iws.f g01mio.f g01msg.f g01pat.f g01pbl.f g01pl.f g01pm.f	\
g01sas.f g01seg.f g01siq.f g01snp.f g01tx.f g01wdr.f g01wkt.f		\
gflcnv.f giargs.f ginlod.f gkasci.f gmflod.f gmpart.f gopnic.f		\
gputmd.f gputni.f gputpr.f gputps.f gtnlen.f gupasf.f gupdva.f		\
gupdvi.f gupdvr.f argb2ci.f wks.c

noinst_HEADERS = g01adc.h g01ast.h g01dfa.h g01ins.h g01io.h g01opc.h	\
g01prm.h g01rqa.h g01wdt.h g01wsl.h gksenu.h gksin.h wks.h g01adc.doc	\
g01dfa.doc g01ins.doc g01io.doc g01opc.doc g01wdt.doc g01wsl.doc	\
gksenu.doc
