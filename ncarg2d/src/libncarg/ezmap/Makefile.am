# This is an automake file for NCL
# Ed Hartnett 8/30/18

# Build this convenience library.
noinst_LTLIBRARIES = libncarg_ezmap.la

# Find gks.h.
AM_CPPFLAGS = -I$(top_srcdir)/ncarg2d/src/libncarg_gks

# Find c.h.
AM_CPPFLAGS += -I$(top_srcdir)/common/src/libncarg_c

# Find ncargC.h.
AM_CPPFLAGS += -I$(top_srcdir)/ncarg2d/src/libncarg

# Sources for convenience library.
libncarg_ezmap_la_SOURCES = mapnga.c mapngb.c hlumapeod.f hlumapusr.f	\
idictl.f mapbd.f mapdrw.f mapeod.f mapfst.f mapgci.f mapgrd.f		\
mapgtc.f mapgtd.f mapgti.f mapgtl.f mapgtr.f mapint.f mapio.f mapiq.f	\
mapiqd.f mapit.f mapitd.f maplbl.f maplmb.f maplot.f mappos.f		\
maproj.f maprs.f maprst.f mapsav.f mapset.f mapstc.f mapstd.f		\
mapsti.f mapstl.f mapstr.f maptra.f maptri.f maptrn.f mapusr.f		\
mapvec.f mdgcog.f mdgetc.f mdgetd.f mdgeti.f mdgetl.f mdgetr.f		\
mdgnin.f mdilat.f mdilon.f mdinch.f mditve.f mdlach.f mdlbln.f		\
mdlblt.f mdloch.f mdpcem.f mdpchi.f mdpdrw.f mdpeod.f mdpfst.f		\
mdpgci.f mdpgrd.f mdpin1.f mdpin2.f mdpint.f mdpio.f mdpiq.f mdpiqd.f	\
mdpit.f mdpitd.f mdplbl.f mdplmb.f mdplot.f mdppos.f mdproj.f mdprs.f	\
mdprst.f mdpsav.f mdpset.f mdptra.f mdptre.f mdptri.f mdptrn.f		\
mdptrp.f mdpusr.f mdpvec.f mdpvp.f mdpvpd.f mdpwnd.f mdrgam.f		\
mdrgdi.f mdrgdl.f mdrgdp.f mdrgdr.f mdrged.f mdrgfa.f mdrgfp.f		\
mdrggc.f mdrgip.f mdrgof.f mdrgol.f mdrgsc.f mdrgsf.f mdrgsq.f		\
mdrgsx.f mdritd.f mdrset.f mdscal.f mdsetc.f mdsetd.f mdseti.f		\
mdsetl.f mdsetr.f mpgetc.f mpgetd.f mpgeti.f mpgetl.f mpgetr.f		\
mprset.f mpsetc.f mpsetd.f mpseti.f mpsetl.f mpsetr.f rbgdfe.f		\
rbglen.f rbidfe.f supcon.f supmap.f rdpnaz.f rdpndw.f rdpntz.f		\
rdpnuw.f mdlltd.f mdllnd.f mdgnid.f mdlacd.f mdlocd.f maqini.f		\
mdqini.f mdqin2.f maqtra.f mdqtra.f maqtri.f mdqtri.f maqtrn.f		\
mdqtrn.f aiproj.f aiprin.f haproj.f haprin.f moproj.f moprin.f		\
wtproj.f wtprin.f mpdbdi.f # mapnga.c mapngb.c

mpdbdi.f: mpdbdi.f.sed Makefile
	sed -e 's+SED_DBDIR+$(DBDIR)+' <mpdbdi.f.sed >mpdbdi.f

EXTRA_DIST = mpdbdi.f.sed

CLEANFILES = mpdbdi.f
