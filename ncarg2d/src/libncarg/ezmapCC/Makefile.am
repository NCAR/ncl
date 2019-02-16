# This is an automake file for NCL
# Ed Hartnett 8/30/18

LDADD = ${top_builddir}/common/src/libncarg_c/libncarg_c.la

# Build this convenience library.
noinst_LTLIBRARIES = libncarg_ezmapcc.la

# Find gks.h.
AM_CPPFLAGS = -I$(top_srcdir)/ncarg2d/src/libncarg_gks

# Find c.h.
AM_CPPFLAGS += -I$(top_srcdir)/common/src/libncarg_c

# Find ncargC.h.
AM_CPPFLAGS += -I$(top_srcdir)/ncarg2d/src/libncarg

# Sources for convenience library.
libncarg_ezmapcc_la_SOURCES = c_mapblm.c c_mapdrw.c c_mapfst.c		\
c_mapgrd.c c_mapgtc.c c_mapgti.c c_mapgtl.c c_mapgtr.c c_mapint.c	\
c_mapiq.c c_mapit.c c_maplbl.c c_maplmb.c c_maplot.c c_mappos.c		\
c_maproj.c c_maprs.c c_maprst.c c_mapsav.c c_mapset.c c_mapstc.c	\
c_mapsti.c c_mapstl.c c_mapstr.c c_maptri.c c_maptrn.c c_mapvec.c	\
c_supcon.c c_supmap.c c_mapgci.c c_maptra.c c_mapiqa.c c_mapiqm.c	\
c_mapita.c c_mapitm.c c_mapaci.c c_mapbla.c c_mapgrm.c c_mpgetc.c	\
c_mpgeti.c c_mpgetl.c c_mpgetr.c c_mpsetc.c c_mpseti.c c_mpsetl.c	\
c_mpsetr.c c_mapiqd.c c_mapitd.c c_mprset.c c_mpfnme.c c_mpglty.c	\
c_mpiaty.c c_mpifnb.c c_mpilnb.c c_mpiola.c c_mpiosa.c c_mpipai.c	\
c_mpipan.c c_mpipar.c c_mpisci.c c_mplnam.c c_mplndm.c c_mplndr.c	\
c_mplnri.c c_mpname.c c_mapgtd.c c_mapstd.c c_mpsetd.c c_mdgetc.c	\
c_mdgetd.c c_mdgeti.c c_mdgetl.c c_mdgetr.c c_mdsetc.c c_mdsetd.c	\
c_mdseti.c c_mdsetl.c c_mdsetr.c c_mdfnme.c c_mdglty.c c_mdiaty.c	\
c_mdifnb.c c_mdilnb.c c_mdiola.c c_mdiosa.c c_mdipai.c c_mdipan.c	\
c_mdipar.c c_mdisci.c c_mdlnam.c c_mdlndm.c c_mdlndr.c c_mdlnri.c	\
c_mdname.c c_mdpaci.c c_mdpbla.c c_mdpblm.c c_mdpdrw.c c_mdpgrd.c	\
c_mdpgrm.c c_mdpint.c c_mdplbl.c c_mdplmb.c c_mdplot.c c_mdprs.c	\
c_mdprst.c c_mdpsav.c c_mdrset.c c_mdpiq.c c_mdpiqd.c c_mdrgol.c	\
c_mdrgsf.c c_mdpfst.c c_mdpvec.c c_mdpiqa.c c_mdpiqm.c c_mdpit.c	\
c_mdpita.c c_mdpitd.c c_mdpitm.c c_mdppos.c c_mdproj.c c_mdpset.c	\
c_mdptra.c c_mdptri.c c_mdptrn.c c_mpgetd.c c_mdpgci.c c_mdgcog.c	\
c_mdlbln.c c_mdlblt.c c_mdrggc.c c_mdrgsc.c c_mdritd.c c_mdutfd.c	\
c_mdutfs.c c_mdutid.c c_mdutis.c c_mputfd.c c_mputfs.c c_mputid.c	\
c_mputis.c c_mdutin.c c_mputin.c c_mdlach.c c_mdloch.c c_mdrgdl.c	\
c_mdscal.c c_maplmm.c c_mdplmm.c c_maqini.c c_maqtra.c c_maqtri.c	\
c_maqtrn.c c_mdqini.c c_mdqtra.c c_mdqtri.c c_mdqtrn.c
