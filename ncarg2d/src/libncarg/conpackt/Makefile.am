# This is an automake file for NCL
# Ed Hartnett 8/30/18

# We use these two programs to build the fortran source.
IFTRAN = ${top_builddir}/ncarg2d/src/libncarg/Iftran/Iftran
FSPLIT = ${top_builddir}/ncarg2d/src/libncarg/Iftran/Fsplit

# Build this convenience library.
noinst_LTLIBRARIES = libncarg_conpackt.la

# Sources for convenience library. These are built by running
# Iftran/Fsplit on file CodeIftran.
libncarg_conpackt_la_SOURCES = ctabgc.f ctback.f ctblda.f ctcflb.f	\
ctchcf.f ctchcl.f ctchhl.f ctchil.f ctchll.f ctcica.f ctclam.f		\
ctcldm.f ctcldr.f ctcltr.f ctcpag.f ctdrpl.f ctdrsg.f ctfran.f		\
ctgetc.f ctgeti.f ctgetr.f ctgiws.f ctgrws.f ctgvai.f cthchm.f		\
cthchr.f cthllb.f cthlso.f ctinlb.f ctinrc.f ctinrk.f ctlbam.f		\
ctlbdr.f ctmesh.f ctmviw.f ctmvrw.f ctmxyz.f ctnumb.f ctpisb.f		\
ctpitt.f ctpkcl.f ctpklb.f ctpklp.f ctplar.f ctplps.f ctranf.f		\
ctrset.f ctsbst.f ctscae.f ctsetc.f ctseti.f ctsetr.f ctsort.f		\
ctstls.f cttdbf.f cttdbm.f cttdca.f cttddm.f cttmrg.f cttmtl.f		\
cttmtx.f cttrcl.f cttreg.f cttroe.f cttrve.f ctwlam.f hluctchcf.f	\
hluctchcl.f hluctchhl.f hluctchil.f hluctchll.f hluctmxyz.f		\
hluctscae.f icaedg.f icapnt.f icapnx.f hlucttmtl.f hluicaedg.f		\
hluicapnt.f hluctmesh.f

# Note that all these files must be built.
BUILT_SOURCES = IftranRun $(libncarg_conpackt_la_SOURCES)

# Track dependencies to allow parallel builds.
ctabgc.f: IftranRun
ctback.f: IftranRun
ctblda.f: IftranRun
ctcflb.f: IftranRun
ctchcf.f: IftranRun
ctchcl.f: IftranRun
ctchhl.f: IftranRun
ctchil.f: IftranRun
ctchll.f: IftranRun
ctcica.f: IftranRun
ctclam.f: IftranRun
ctcldm.f: IftranRun
ctcldr.f: IftranRun
ctcltr.f: IftranRun
ctcpag.f: IftranRun
ctdrpl.f: IftranRun
ctdrsg.f: IftranRun
ctfran.f: IftranRun
ctgetc.f: IftranRun
ctgeti.f: IftranRun
ctgetr.f: IftranRun
ctgiws.f: IftranRun
ctgrws.f: IftranRun
ctgvai.f: IftranRun
cthchm.f: IftranRun
cthchr.f: IftranRun
cthllb.f: IftranRun
cthlso.f: IftranRun
ctinlb.f: IftranRun
ctinrc.f: IftranRun
ctinrk.f: IftranRun
ctlbam.f: IftranRun
ctlbdr.f: IftranRun
ctmesh.f: IftranRun
ctmviw.f: IftranRun
ctmvrw.f: IftranRun
ctmxyz.f: IftranRun
ctnumb.f: IftranRun
ctpisb.f: IftranRun
ctpitt.f: IftranRun
ctpkcl.f: IftranRun
ctpklb.f: IftranRun
ctpklp.f: IftranRun
ctplar.f: IftranRun
ctplps.f: IftranRun
ctranf.f: IftranRun
ctrset.f: IftranRun
ctsbst.f: IftranRun
ctscae.f: IftranRun
ctsetc.f: IftranRun
ctseti.f: IftranRun
ctsetr.f: IftranRun
ctsort.f: IftranRun
ctstls.f: IftranRun
cttdbf.f: IftranRun
cttdbm.f: IftranRun
cttdca.f: IftranRun
cttddm.f: IftranRun
cttmrg.f: IftranRun
cttmtl.f: IftranRun
cttmtx.f: IftranRun
cttrcl.f: IftranRun
cttreg.f: IftranRun
cttroe.f: IftranRun
cttrve.f: IftranRun
ctwlam.f: IftranRun
hluctchcf.f: IftranRun
hluctchcl.f: IftranRun
hluctchhl.f: IftranRun
hluctchil.f: IftranRun
hluctchll.f: IftranRun
hluctmxyz.f: IftranRun
hluctscae.f: IftranRun
icaedg.f: IftranRun
icapnt.f: IftranRun
icapnx.f: IftranRun
hlucttmtl.f: IftranRun
hluicaedg.f: IftranRun
hluicapnt.f: IftranRun
hluctmesh.f: IftranRun

# Target to trigger building of all fortran files.
IftranRun : CodeIftran
	@$(IFTRAN) < CodeIftran > CodeFortran
	@$(FSPLIT) -both CodeFortran
	touch IftranRun

# Include all source code in dist.
EXTRA_DIST = CodeIftran

CLEANFILES = $(BUILT_SOURCES) CodeFortran
