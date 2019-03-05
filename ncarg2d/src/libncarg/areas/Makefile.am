# This is an automake file for NCL.
# Ed Hartnett 8/30/18

# We use these two programs to build the fortran source.
IFTRAN = ${top_builddir}/ncarg2d/src/libncarg/Iftran/Iftran
FSPLIT = ${top_builddir}/ncarg2d/src/libncarg/Iftran/Fsplit

# Build this convenience library.
noinst_LTLIBRARIES = libncarg_areas.la

# Sources for convenience library. These are built by running
# Iftran/Fsplit on file CodeIftran.
libncarg_areas_la_SOURCES = arblda.f arrat2.f ardat2.f ardbda.f	\
ardbpa.f ardrln.f aredam.f argeti.f argetr.f argpai.f argtai.f	\
arinam.f arini2.f arinit.f armpia.f armvam.f arpram.f arscam.f	\
arseti.f arsetr.f

# Note that all these files must be built.
BUILT_SOURCES = IftranRun $(libncarg_areas_la_SOURCES)

# Track dependencies to allow parallel builds.
arblda.f: IftranRun
arrat2.f: IftranRun
ardat2.f: IftranRun
ardbda.f: IftranRun
ardbpa.f: IftranRun
ardrln.f: IftranRun
aredam.f: IftranRun
argeti.f: IftranRun
argetr.f: IftranRun
argpai.f: IftranRun
argtai.f: IftranRun
arinam.f: IftranRun
arini2.f: IftranRun
arinit.f: IftranRun
armpia.f: IftranRun
armvam.f: IftranRun
arpram.f: IftranRun
arscam.f: IftranRun
arseti.f: IftranRun
arsetr.f: IftranRun

# Target to trigger building of all fortran files.
IftranRun : CodeIftran
	$(IFTRAN) < CodeIftran > CodeFortran
	$(FSPLIT) -both CodeFortran
	touch IftranRun

# Include all source code in dist.
EXTRA_DIST = CodeIftran

CLEANFILES = $(BUILT_SOURCES) CodeFortran IftranRun
