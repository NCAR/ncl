# This is an automake file for NCL.
# Ed Hartnett 8/30/18

LDADD = ${top_builddir}/common/src/libncarg_c/libncarg_c.la

bin_PROGRAMS = fontcap

fontcap_SOURCES = cfaamn.f cfrdln.f cfwrit.f ffgttk.f ffinfo.f	\
ffphol.f ffppkt.f ffprcf.f ffprsa.f fftbkd.f fftkin.f sffndc.f	\
sfgtin.f sfgtkw.f sfprcf.f sfskbk.f sftbkd.f

cfaamn.f:cfaamn.f.sed
	cp cfaamn.f.sed cfaamn.F
	$(CPP) $(CPPFLAGS) -o cfaamn.f cfaamn.F

EXTRA_DIST = cfaamn.f.sed
