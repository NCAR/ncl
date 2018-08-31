# This is an automake file for NCL
# Ed Hartnett 8/30/18

# We use these two programs to build the fortran source.
IFTRAN = ${top_builddir}/ncarg2d/src/libncarg/Iftran/Iftran
FSPLIT = ${top_builddir}/ncarg2d/src/libncarg/Iftran/Fsplit

# Build this convenience library.
noinst_LTLIBRARIES = libncarg_conpack.la

# Sources for convenience library. These are built by running
# Iftran/Fsplit on file CodeIftran.
libncarg_conpack_la_SOURCES = cpblda.f cpback.f cpchcf.f cpchcl.f	\
cpchhl.f cpchil.f cpchll.f cpclam.f cpcldm.f cpcldr.f cpcnrc.f		\
cpdrpl.f cpezct.f cpgetc.f cpgeti.f cpgetr.f cpgvai.f cpinrk.f		\
cplbam.f cplbdr.f cpmpxy.f cpmviw.f cpmvrw.f cppkcl.f cppklb.f		\
cppklp.f cprect.f cprset.f cpsetc.f cpseti.f cpsetr.f cpsprs.f		\
cpsps1.f cpsp1a.f cpsps2.f cpsp2a.f cpcflb.f cpcpag.f cpdrsg.f		\
cpgiws.f cpgrws.f cphllb.f cpinit.f cpinlb.f cpinrc.f cpnumb.f		\
cpplar.f cpplps.f cpranf.f cpsbst.f cpsort.f cpstls.f cptrcl.f		\
cptreg.f cptres.f cptrev.f cpcltr.f cpcica.f cphchm.f cphchr.f		\
cpscae.f cptroe.f cptrve.f cpwlam.f hlucpchcf.f hlucpchcl.f		\
hlucpchhl.f hlucpchil.f hlucpchll.f hlucpmpxy.f hlucpscae.f cphlso.f	\
cphls2.f

# Note that all these files must be built.
BUILT_SOURCES = IftranRun $(libncarg_conpack_la_SOURCES)

# Track dependencies to allow parallel builds.
cpback.f: IftranRun

# Target to trigger building of all fortran files.
IftranRun : CodeIftran
	@$(IFTRAN) < CodeIftran > CodeFortran
	@$(FSPLIT) -both CodeFortran
	touch IftranRun

# Include all source code in dist.
EXTRA_DIST = CodeIftran

CLEANFILES = $(BUILT_SOURCES) CodeFortran
