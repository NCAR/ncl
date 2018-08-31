# This is an automake file for NCL.
# Ed Hartnett 8/30/18

LDADD = ${top_builddir}/common/src/libncarg_c/libncarg_c.la

bin_PROGRAMS = graphcap
GRAPHC = graphcap

graphcap_SOURCES = capchg.f binput.f bndcls.f capast.f capdat.f		\
cappar.f dascls.f devcls.f devcol.f devmap.f fndchr.f grinit.f		\
gtflt.f gtint.f gtlog.f gtstr.f intcnv.f lincls.f marcls.f mtchit.f	\
plgcls.f rascls.f rdline.f skpblk.f txtcls.f

# rule for processing graphcaps
.gc:
	@echo "	Processing graphcap $@"
	@$(GRAPHC) $< $@

aed.a: aed.a.gc
	@$(GRAPHC) < aed.a.gc > aed.a
aed.b: aed.b.gc
	@$(GRAPHC) < aed.b.gc > aed.b

capast.f: capast.f.sed
	cp capast.f.sed capast.F
	${CPP} ${CPPFLAGS} -E -o capast.f capast.F

capchg.f: capchg.f.sed
	cp capchg.f.sed capchg.F
	${CPP} ${CPPFLAGS} -E -o capchg.f capchg.F

EXTRA_DIST = adm5.gc aed.a.gc aed.b.gc balsml.gc hpgl.gc hp150.gc	\
hp2648a.gc hp7475a.gc hp7510a.gc hpgl2.gc hpgl2.dual.gc hpgl2pe.gc	\
hpgl2pe.dual.gc imagen.gc imagen.port.gc pc.mono.gc ps.color.gc		\
ps.land.color.gc ps.land.mono.gc ps.mono.gc qms800.gc r6211.gc		\
s100.gc t4010.gc t4025.gc t4105.gc t4107.gc t4107.seg.gc t4115.gc	\
t4115.seg.gc tal1590.gc tekalike.gc versaterm.gc vt100.gc vt125.gc	\
vt220.gc vt240.gc vt330.gc vt340.gc vt340w.gc ditroff.gc capast.f.sed	\
capchg.f.sed
