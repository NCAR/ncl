# This is an automake file for NCL.
# Ed Hartnett 8/30/18

LDADD = ${top_builddir}/common/src/libncarg_c/libncarg_c.la

bin_PROGRAMS = fontc
FONTC = ${top_builddir}/common/src/fontcap/fontc

fontc_SOURCES = cfaamn.f cfrdln.f cfwrit.f ffgttk.f ffinfo.f ffphol.f	\
ffppkt.f ffprcf.f ffprsa.f fftbkd.f fftkin.f sffndc.f sfgtin.f		\
sfgtkw.f sfprcf.f sfskbk.f sftbkd.f

noinst_HEADERS = capfnt.h  fntcom.h  fnterr.h  fnttab.h

cfaamn.f:cfaamn.f.sed
	cp cfaamn.f.sed cfaamn.F
	$(CPP) $(CPPFLAGS) -o cfaamn.f cfaamn.F

font1: font1.fc fontc
	$(FONTC) $< $@
font2: font2.fc fontc
	$(FONTC) $< $@
font3: font3.fc fontc
	$(FONTC) $< $@
font4: font4.fc fontc
	$(FONTC) $< $@
font5: font5.fc fontc
	$(FONTC) $< $@
font6: font6.fc fontc
	$(FONTC) $< $@
font7: font7.fc fontc
	$(FONTC) $< $@
font8: font8.fc fontc
	$(FONTC) $< $@
font9: font9.fc fontc
	$(FONTC) $< $@
font10: font10.fc fontc
	$(FONTC) $< $@
font11: font11.fc fontc
	$(FONTC) $< $@
font12: font12.fc fontc
	$(FONTC) $< $@
font13: font13.fc fontc
	$(FONTC) $< $@
font14: font14.fc fontc
	$(FONTC) $< $@
font15: font15.fc fontc
	$(FONTC) $< $@
font16: font16.fc fontc
	$(FONTC) $< $@
font17: font17.fc fontc
	$(FONTC) $< $@
font18: font18.fc fontc
	$(FONTC) $< $@
font19: font19.fc fontc
	$(FONTC) $< $@
font20: font20.fc fontc
	$(FONTC) $< $@
font21: font21.fc fontc
	$(FONTC) $< $@
font22: font22.fc fontc
	$(FONTC) $< $@
font23: font23.fc fontc
	$(FONTC) $< $@
font24: font24.fc fontc
	$(FONTC) $< $@
font25: font25.fc fontc
	$(FONTC) $< $@
font26: font26.fc fontc
	$(FONTC) $< $@
font27: font27.fc fontc
	$(FONTC) $< $@
font28: font28.fc fontc
	$(FONTC) $< $@
font29: font29.fc fontc
	$(FONTC) $< $@
font30: font30.fc fontc
	$(FONTC) $< $@
font31: font31.fc fontc
	$(FONTC) $< $@
font32: font32.fc fontc
	$(FONTC) $< $@
font33: font33.fc fontc
	$(FONTC) $< $@
font34: font34.fc fontc
	$(FONTC) $< $@
font35: font35.fc fontc
	$(FONTC) $< $@
font36: font36.fc fontc
	$(FONTC) $< $@
font37: font37.fc fontc
	$(FONTC) $< $@

# Install these data files.
fontcapdir = $(prefix)/lib/ncarg/fontcaps
fontcap_HEADERS = font10 font14 font18 font21 font29 font34 font3	\
font7 font11 font15 font19 font22 font2 font35 font4 font8 font12	\
font16 font1 font25 font30 font36 font5 font9 font13 font17 font20	\
font26 font33 font37 font6

EXTRA_DIST = cfaamn.f.sed font10.fc font14.fc font18.fc font21.fc	\
font29.fc font34.fc font3.fc font7.fc font11.fc font15.fc font19.fc	\
font22.fc font2.fc font35.fc font4.fc font8.fc font12.fc font16.fc	\
font1.fc font25.fc font30.fc font36.fc font5.fc font9.fc font13.fc	\
font17.fc font20.fc font26.fc font33.fc font37.fc font6.fc

CLEANFILES = $(fontcap_HEADERS)
