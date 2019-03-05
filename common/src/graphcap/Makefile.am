# This is an automake file for NCL.
# Ed Hartnett 8/30/18

LDADD = ${top_builddir}/common/src/libncarg_c/libncarg_c.la

bin_PROGRAMS = graphc
GRAPHCAP = ${top_builddir}/common/src/graphcap/graphc

graphc_SOURCES = capchg.f binput.f bndcls.f capast.f capdat.f		\
cappar.f dascls.f devcls.f devcol.f devmap.f fndchr.f grinit.f		\
gtflt.f gtint.f gtlog.f gtstr.f intcnv.f lincls.f marcls.f mtchit.f	\
plgcls.f rascls.f rdline.f skpblk.f txtcls.f

aed.a: aed.a.gc graphc
	$(GRAPHCAP) aed.a.gc > aed.a
aed.b: aed.b.gc graphc
	$(GRAPHCAP) aed.b.gc > aed.b
adm5: adm5.gc graphc
	$(GRAPHCAP) $< $@
balsml: balsml.gc graphc
	$(GRAPHCAP) $< $@
hpgl: hpgl.gc graphc
	$(GRAPHCAP) $< $@
hp150: hp150.gc graphc
	$(GRAPHCAP) $< $@
hp2648a: hp2648a.gc graphc
	$(GRAPHCAP) $< $@
hp7475a: hp7475a.gc graphc
	$(GRAPHCAP) $< $@
hp7510a: hp7510a.gc graphc
	$(GRAPHCAP) $< $@
hpgl2: hpgl2.gc graphc
	$(GRAPHCAP) $< $@
hpgl2.dual: hpgl2.dual.gc graphc
	$(GRAPHCAP) $< $@
hpgl2pe: hpgl2pe.gc graphc
	$(GRAPHCAP) $< $@
hpgl2pe.dual: hpgl2pe.dual.gc graphc
	$(GRAPHCAP) $< $@
imagen: imagen.gc graphc
	$(GRAPHCAP) $< $@
imagen.port: imagen.port.gc graphc
	$(GRAPHCAP) $< $@
pc.mono: pc.mono.gc graphc
	$(GRAPHCAP) $< $@
ps.color: ps.color.gc graphc
	$(GRAPHCAP) $< $@
ps.land.color: ps.land.color.gc graphc
	$(GRAPHCAP) $< $@
ps.land.mono: ps.land.mono.gc graphc
	$(GRAPHCAP) $< $@
ps.mono: ps.mono.gc graphc
	$(GRAPHCAP) $< $@
qms800: qms800.gc graphc
	$(GRAPHCAP) $< $@
r6211: r6211.gc graphc
	$(GRAPHCAP) $< $@
s100: s100.gc graphc
	$(GRAPHCAP) $< $@
t4010: t4010.gc graphc
	$(GRAPHCAP) $< $@
t4025: t4025.gc graphc
	$(GRAPHCAP) $< $@
t4105: t4105.gc graphc
	$(GRAPHCAP) $< $@
t4107: t4107.gc graphc
	$(GRAPHCAP) $< $@
t4107.seg: t4107.seg.gc graphc
	$(GRAPHCAP) $< $@
t4115: t4115.gc graphc
	$(GRAPHCAP) $< $@
t4115.seg: t4115.seg.gc graphc
	$(GRAPHCAP) $< $@
tal1590: tal1590.gc graphc
	$(GRAPHCAP) $< $@
tekalike: tekalike.gc graphc
	$(GRAPHCAP) $< $@
versaterm: versaterm.gc graphc
	$(GRAPHCAP) $< $@
vt100: vt100.gc graphc
	$(GRAPHCAP) $< $@
vt125: vt125.gc graphc
	$(GRAPHCAP) $< $@
vt220: vt220.gc graphc
	$(GRAPHCAP) $< $@
vt240: vt240.gc graphc
	$(GRAPHCAP) $< $@
vt330: vt330.gc graphc
	$(GRAPHCAP) $< $@
vt340: vt340.gc graphc
	$(GRAPHCAP) $< $@
vt340w: vt340w.gc graphc
	$(GRAPHCAP) $< $@
ditroff: ditroff.gc graphc
	$(GRAPHCAP) $< $@

capast.f: capast.f.sed
	cp capast.f.sed capast.F
	${CPP} ${CPPFLAGS} -E -o capast.f capast.F

capchg.f: capchg.f.sed
	cp capchg.f.sed capchg.F
	${CPP} ${CPPFLAGS} -E -o capchg.f capchg.F

# Install these data files.
graphcapdir = $(prefix)/lib/ncarg/graphcaps
graphcap_HEADERS = qms800 r6211 s100 t4010 t4025 t4105 t4107		\
t4107.seg t4115 t4115.seg tal1590 tekalike versaterm vt100 vt125	\
vt220 vt240 vt330 vt340 vt340w

EXTRA_DIST = adm5.gc aed.a.gc aed.b.gc balsml.gc hpgl.gc hp150.gc	\
hp2648a.gc hp7475a.gc hp7510a.gc hpgl2.gc hpgl2.dual.gc hpgl2pe.gc	\
hpgl2pe.dual.gc imagen.gc imagen.port.gc pc.mono.gc ps.color.gc		\
ps.land.color.gc ps.land.mono.gc ps.mono.gc qms800.gc r6211.gc		\
s100.gc t4010.gc t4025.gc t4105.gc t4107.gc t4107.seg.gc t4115.gc	\
t4115.seg.gc tal1590.gc tekalike.gc versaterm.gc vt100.gc vt125.gc	\
vt220.gc vt240.gc vt330.gc vt340.gc vt340w.gc ditroff.gc capast.f.sed	\
capchg.f.sed

CLEANFILES = aed.a aed.b capast.F capast.f capchg.F capchg.f	\
$(graphcap_HEADERS)
