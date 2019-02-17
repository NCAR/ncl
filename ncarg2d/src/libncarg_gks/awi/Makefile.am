# This is an automake file for NCL.
# Ed Hartnett 8/31/18

# Build this convenience library.
noinst_LTLIBRARIES = libncarg_awi.la

# Find gks.h.
AM_CPPFLAGS = -I$(top_srcdir)/ncarg2d/src/libncarg_gks

# Find c.h.
AM_CPPFLAGS += -I$(top_srcdir)/common/src/libncarg_c

# Find ncargC.h.
AM_CPPFLAGS += -I$(top_srcdir)/ncarg2d/src/libncarg

# Sources for convenience library.
libncarg_awi_la_SOURCES = gactm.f gacwk.f gaspar.f gatelm.f gca.f	\
gcelcd.f gceldc.f gceldr.f gclks.f gclrwk.f gclsg.f gclwk.f gcrsg.f	\
gcsgwk.f gdawk.f gdsg.f geclks.f gelms.f genneg.f gerhnd.f gerlog.f	\
gesc.f gescfn.f gevtm.f gfa.f gfilat.f ggdp.f ggtitm.f giitm.f		\
ginst.f gksbd.f gmarat.f gninst.f gnpart.f gopdec.f gopks.f gopwk.f	\
gpl.f gpm.f gpolat.f gprec.f gputpt.f gqacwk.f gqasf.f gqcf.f gqchb.f	\
gqchh.f gqchsp.f gqchup.f gqchw.f gqchxp.f gqclip.f gqcntn.f gqcr.f	\
gqdsp.f gqeci.f gqegdp.f gqentn.f gqewk.f gqfaci.f gqfaf.f gqfai.f	\
gqfais.f gqfasi.f gqgdp.f gqln.f gqlvks.f gqlwk.f gqlwsc.f gqmk.f	\
gqmksc.f gqmntn.f gqnt.f gqops.f gqopsg.f gqopwk.f gqpa.f gqpaf.f	\
gqparf.f gqpcr.f gqpfar.f gqplci.f gqplf.f gqpli.f gqpmci.f gqpmf.f	\
gqpmi.f gqppar.f gqpplr.f gqppmr.f gqptxr.f gqpx.f gqpxa.f gqpxad.f	\
gqsgus.f gqtxal.f gqtxci.f gqtxf.f gqtxfp.f gqtxi.f gqtxp.f gqtxx.f	\
gqwkc.f gqwkca.f gqwkcl.f gqwkdu.f gqwks.f gqwkt.f grditm.f grqst.f	\
gsasf.f gschh.f gschsp.f gschup.f gschxp.f gsclip.f gscr.f gsegdt.f	\
gsegrd.f gselnt.f gsfaci.f gsfai.f gsfais.f gsfasi.f gskpop.f gsln.f	\
gslwsc.f gsmk.f gsmksc.f gspa.f gsparf.f gsplci.f gspli.f gspmci.f	\
gspmi.f gssgt.f gsstm.f gstxal.f gstxci.f gstxfp.f gstxi.f gstxp.f	\
gsvp.f gswkvp.f gswkwn.f gswn.f gtflt.f gtx.f gtxdrw.f gtxtat.f		\
gurec.f guwk.f gwitm.f gxlate.f gxmdef.f gxopdf.f gzckst.f gzckwk.f	\
gzclli.f gzclpo.f gzcpwk.f gzfmwk.f gzgte1.f gzgte2.f gzines.f		\
gzinsl.f gziqwk.f gzn2wx.f gzn2wy.f gzname.f gznume.f gzputr.f		\
gzputs.f gzreop.f gzroi.f gzsrat.f gzusat.f gztowk.f gzw2gk.f		\
gzw2nx.f gzw2ny.f gzxid.f gzacwk.f gzclwk.f gzclrwk.f gzdawk.f		\
gzopwk.f

ggkwdr_stub.o: ggkwdr_stub.c
	$(FC) -c ${FCFLAGS} ${AM_CPPFLAGS} -o $@ $^

noinst_HEADERS = gkscom.h trbufr.h trcode.h trdefl.h trinst.h	\
trpars.h trstat.h

# Install this file.
robjdir = $(prefix)/lib/ncarg/robj
robj_HEADERS = ggkwdr_stub.o

EXTRA_DIST = ggkwdr_stub.c
