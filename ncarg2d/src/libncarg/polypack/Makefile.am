# This is an automake file for NCL
# Ed Hartnett 8/30/18

# We use these two programs to build the fortran source.
IFTRAN = ${top_builddir}/ncarg2d/src/libncarg/Iftran/Iftran
FSPLIT = ${top_builddir}/ncarg2d/src/libncarg/Iftran/Fsplit

# Build this convenience library.
noinst_LTLIBRARIES = libncarg_polypack.la

# Sources for convenience library. These are built by running
# Iftran/Fsplit on file CodeIftran.
libncarg_polypack_la_SOURCES = ppdipo.f ppditr.f ppinpo.f ppintr.f	\
ppplcl.f ppppap.f ppunpo.f ppuntr.f ppzbts.f

# Note that all these files must be built.
BUILT_SOURCES = IftranRun $(libncarg_polypack_la_SOURCES)

# Track dependencies to allow parallel builds.
ppdipo.f: IftranRun
ppditr.f: IftranRun
ppinpo.f: IftranRun
ppintr.f: IftranRun
ppplcl.f: IftranRun
ppppap.f: IftranRun
ppunpo.f: IftranRun
ppuntr.f: IftranRun
ppzbts.f: IftranRun

# Target to trigger building of all fortran files.
IftranRun : CodeIftran
	@$(IFTRAN) < CodeIftran > CodeFortran
	@$(FSPLIT) -both CodeFortran
	touch IftranRun

# Include all source code in dist.
EXTRA_DIST = CodeIftran

CLEANFILES = $(BUILT_SOURCES) CodeFortran
