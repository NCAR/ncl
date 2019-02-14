# This is an automake file for NCL
# Ed Hartnett 8/30/18

# We use these two programs to build the fortran source.
IFTRAN = ${top_builddir}/ncarg2d/src/libncarg/Iftran/Iftran
FSPLIT = ${top_builddir}/ncarg2d/src/libncarg/Iftran/Fsplit

# Build this convenience library.
noinst_LTLIBRARIES = libncarg_vaspackt.la

# Sources for convenience library. These are built by running
# Iftran/Fsplit on file CodeIftran.
libncarg_vaspackt_la_SOURCES = hluvtchil.f hluvtchzf.f hluvtmxyz.f	\
icavpt.f icavpx.f vtabgc.f vtback.f vtblda.f vtchil.f vtchzf.f		\
vtcudr.f vtcvdm.f vtcvdr.f vtdrel.f vtdrpl.f vtfran.f vtgetc.f		\
vtgeti.f vtgetr.f vtgiws.f vtgrws.f vtinlb.f vtinrc.f vtinrk.f		\
vtmesh.f vtmviw.f vtmvrw.f vtmxyz.f vtnumb.f vtpisb.f vtpitt.f		\
vtrand.f vtrset.f vtrvsp.f vtsbst.f vtsetc.f vtseti.f vtsetr.f		\
vtsldm.f vtsldr.f vtsort.f vtsvdm.f vtsvdr.f vttdbf.f vttdbm.f		\
vttddm.f vttdfm.f vttlom.f vttmrg.f vttmtl.f vttmtx.f vttpom.f		\
vttsom.f vtvrap.f vtvrll.f vtzflb.f

# Note that all these files must be built.
BUILT_SOURCES = IftranRun $(libncarg_vaspackt_la_SOURCES)

# Track dependencies to allow parallel builds.
hluvtchil.f: IftranRun
hluvtchzf.f: IftranRun
hluvtmxyz.f: IftranRun
icavpt.f: IftranRun
icavpx.f: IftranRun
vtabgc.f: IftranRun
vtback.f: IftranRun
vtblda.f: IftranRun
vtchil.f: IftranRun
vtchzf.f: IftranRun
vtcudr.f: IftranRun
vtcvdm.f: IftranRun
vtcvdr.f: IftranRun
vtdrel.f: IftranRun
vtdrpl.f: IftranRun
vtfran.f: IftranRun
vtgetc.f: IftranRun
vtgeti.f: IftranRun
vtgetr.f: IftranRun
vtgiws.f: IftranRun
vtgrws.f: IftranRun
vtinlb.f: IftranRun
vtinrc.f: IftranRun
vtinrk.f: IftranRun
vtmesh.f: IftranRun
vtmviw.f: IftranRun
vtmvrw.f: IftranRun
vtmxyz.f: IftranRun
vtnumb.f: IftranRun
vtpisb.f: IftranRun
vtpitt.f: IftranRun
vtrand.f: IftranRun
vtrset.f: IftranRun
vtrvsp.f: IftranRun
vtsbst.f: IftranRun
vtsetc.f: IftranRun
vtseti.f: IftranRun
vtsetr.f: IftranRun
vtsldm.f: IftranRun
vtsldr.f: IftranRun
vtsort.f: IftranRun
vtsvdm.f: IftranRun
vtsvdr.f: IftranRun
vttdbf.f: IftranRun
vttdbm.f: IftranRun
vttddm.f: IftranRun
vttdfm.f: IftranRun
vttlom.f: IftranRun
vttmrg.f: IftranRun
vttmtl.f: IftranRun
vttmtx.f: IftranRun
vttpom.f: IftranRun
vttsom.f: IftranRun
vtvrap.f: IftranRun
vtvrll.f: IftranRun
vtzflb.f: IftranRun

# Target to trigger building of all fortran files.
IftranRun : CodeIftran
	@$(IFTRAN) < CodeIftran > CodeFortran
	@$(FSPLIT) -both CodeFortran
	touch IftranRun

# Include all source code in dist.
EXTRA_DIST = CodeIftran

CLEANFILES = $(BUILT_SOURCES) CodeFortran
