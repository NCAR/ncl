# This is an automake file for NCL.
# Ed Hartnett 9/2/18

LDADD = $(top_builddir)/ncarg2d/src/libncarg_gks/awi/ggkwdr_stub.o
LDADD += ${top_builddir}/ncarg2d/src/libncarg/libncarg.la
LDADD += $(top_builddir)/ncarg2d/src/libncarg_gks/libncarg_gks.la
LDADD += ${top_builddir}/common/src/libncarg_c/libncarg_c.la
LDADD += ${top_builddir}/ngmath/src/lib/gridpack/libngmath.la

# bin_PROGRAMS = pre2ncgm.prog

# pre2ncgm_prog_SOURCES = alover.f clipit.f frm.f garg.f intcnv.f		\
# lodctb.f mmdef.f multi.f patdrw.f patset.f pltclp.f pltit.f pnt.f	\
# posit.f pwt.f setop.f setrng.f trndt2.f trnmch.f twobyt.f xmdef.f	\
# xmopdf.f fndarg.f metred.f mninst.f trans.f trassn.f trlate.f		\
# trndat.f

fndarg.f: fndarg.f.sed
	cp $? fndarg.F
	$(CPP) $(CPPFLAGS) -o $@ fndarg.F

metred.f: metred.f.sed
	cp $? metred.F
	$(CPP) $(CPPFLAGS) -o $@ fndarg.F

mninst.f: mninst.f.sed
	cp $? mninst.F
	$(CPP) $(CPPFLAGS) -o $@ fndarg.F

trans.f: trans.f.sed
	cp $? trans.F
	$(CPP) $(CPPFLAGS) -o $@ fndarg.F

trassn.f: trassn.f.sed
	cp $? trassn.F
	$(CPP) $(CPPFLAGS) -o $@ fndarg.F

trlate.f: trlate.f.sed
	cp $? trlate.F
	$(CPP) $(CPPFLAGS) -o $@ fndarg.F

trndat.f: trndat.f.sed
	cp $? trndat.F
	$(CPP) $(CPPFLAGS) -o $@ fndarg.F

CLEANFILES = *.F fndarg.f metred.f mninst.f trans.f trassn.f trlate.f trndat.f
