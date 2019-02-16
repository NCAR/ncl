# This is an automake file for NCL.
# Ed Hartnett 8/31/18

# Add this convenience library from another directory.
LDADD = ${top_builddir}/ncarg2d/src/libncarg_gks/cdrivers/libbasegks.la

# Build this convenience library.
noinst_LTLIBRARIES = libncarg_awic.la

# Find gks.h.
AM_CPPFLAGS = -I$(top_srcdir)/ncarg2d/src/libncarg_gks

# Find c.h.
AM_CPPFLAGS += -I$(top_srcdir)/common/src/libncarg_c

# Find ncargC.h.
AM_CPPFLAGS += -I$(top_srcdir)/ncarg2d/src/libncarg

# Sources for convenience library.
libncarg_awic_la_SOURCES = s_gactm.c s_gacwk.c s_gca.c s_gclks.c	\
s_gclrwk.c s_gclwk.c s_gdawk.c s_geclks.c s_gerhnd.c s_gerlog.c		\
s_gesc.c s_gevtm.c s_gfa.c s_ggdp.c s_ggtitm.c s_giitm.c s_gopks.c	\
s_gopwk.c s_gpl.c s_gpm.c s_gqacwk.c s_gqasf.c s_gqcf.c s_gqchb.c	\
s_gqchh.c s_gqchsp.c s_gqchup.c s_gqchw.c s_gqchxp.c s_gqclip.c		\
s_gqcntn.c s_gqcr.c s_gqdsp.c s_gqeci.c s_gqegdp.c s_gqentn.c		\
s_gqewk.c s_gqfaci.c s_gqfaf.c s_gqfai.c s_gqfais.c s_gqfasi.c		\
s_gqgdp.c s_gqina.c s_gqln.c s_gqlvks.c s_gqlwk.c s_gqlwsc.c s_gqmk.c	\
s_gqmksc.c s_gqmntn.c s_gqnt.c s_gqops.c s_gqopwk.c s_gqpaf.c		\
s_gqpahv.c s_gqparf.c s_gqpawv.c s_gqpcr.c s_gqpfar.c s_gqplci.c	\
s_gqplf.c s_gqpli.c s_gqpmci.c s_gqpmf.c s_gqpmi.c s_gqppar.c		\
s_gqpplr.c s_gqppmr.c s_gqpra.c s_gqptxr.c s_gqpx.c s_gqpxa.c		\
s_gqpxad.c s_gqtxal.c s_gqtxci.c s_gqtxf.c s_gqtxfp.c s_gqtxi.c		\
s_gqtxx.c s_gqtxp.c s_gqwkc.c s_gqwkca.c s_gqwkcl.c s_gqwkdu.c		\
s_gqwks.c s_gqwkt.c s_grditm.c s_gsasf.c s_gschh.c s_gschsp.c		\
s_gschup.c s_gschxp.c s_gsclip.c s_gscr.c s_gselnt.c s_gsfaci.c		\
s_gsfai.c s_gsfais.c s_gsfasi.c s_gsln.c s_gslwsc.c s_gsmk.c		\
s_gsmksc.c s_gspa.c s_gsparf.c s_gsplci.c s_gspli.c s_gspmci.c		\
s_gspmi.c s_gstxal.c s_gstxci.c s_gstxfp.c s_gstxi.c s_gstxp.c		\
s_gsvp.c s_gswkvp.c s_gswkwn.c s_gswn.c s_gtx.c s_guwk.c s_gwitm.c	\
s_gcrsg.c s_gclsg.c s_gdsg.c s_gcsgwk.c s_gssgt.c s_ginst.c		\
s_gqopsg.c s_gqsgus.c s_grqst.c s_gsstm.c getcid.c rmcid.c addcid.c	\
setcid.c gstore.c

noinst_HEADERS = gstore.h
