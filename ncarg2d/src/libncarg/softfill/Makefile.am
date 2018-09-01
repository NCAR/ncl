# This is an automake file for NCL
# Ed Hartnett 8/30/18

# We use these two programs to build the fortran source.
IFTRAN = ${top_builddir}/ncarg2d/src/libncarg/Iftran/Iftran
FSPLIT = ${top_builddir}/ncarg2d/src/libncarg/Iftran/Fsplit

# Build this convenience library.
noinst_LTLIBRARIES = libncarg_softfill.la

# Sources for convenience library. These are built by running
# Iftran/Fsplit on file CodeIftran.
libncarg_softfill_la_SOURCES = sfsgfa.f sfwrld.f sfnorm.f sfgetc.f	\
sfgeti.f sfgetp.f sfgetr.f sfsetc.f sfseti.f sfsetp.f sfsetr.f		\
sfsort.f sfblda.f

# Note that all these files must be built.
BUILT_SOURCES = IftranRun $(libncarg_softfill_la_SOURCES)

# Track dependencies to allow parallel builds.
sfsgfa.f: IftranRun
sfwrld.f: IftranRun
sfnorm.f: IftranRun
sfgetc.f: IftranRun
sfgeti.f: IftranRun
sfgetp.f: IftranRun
sfgetr.f: IftranRun
sfsetc.f: IftranRun
sfseti.f: IftranRun
sfsetp.f: IftranRun
sfsetr.f: IftranRun
sfsort.f: IftranRun
sfblda.f: IftranRun

# Target to trigger building of all fortran files.
IftranRun : CodeIftran
	@$(IFTRAN) < CodeIftran > CodeFortran
	@$(FSPLIT) -both CodeFortran
	touch IftranRun

# Include all source code in dist.
EXTRA_DIST = CodeIftran

CLEANFILES = $(BUILT_SOURCES) CodeFortran
