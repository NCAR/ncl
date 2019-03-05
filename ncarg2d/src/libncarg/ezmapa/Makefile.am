# This is an automake file for NCL
# Ed Hartnett 8/30/18

# We use these two programs to build the fortran source.
IFTRAN = ${top_builddir}/ncarg2d/src/libncarg/Iftran/Iftran
FSPLIT = ${top_builddir}/ncarg2d/src/libncarg/Iftran/Fsplit

# Build this convenience library.
noinst_LTLIBRARIES = libncarg_ezmapa.la

# Sources for convenience library. These are built by running
# Iftran/Fsplit on file CodeIftran.
libncarg_ezmapa_la_SOURCES = mapaci.f mapbla.f mapblm.f mapgrm.f	\
mapiqa.f mapiqm.f mapita.f mapitm.f maplmm.f mdpaci.f mdpbla.f		\
mdpblm.f mdpchm.f mdpgrm.f mdpiqa.f mdpiqm.f mdpita.f mdpitm.f		\
mdplmm.f mdrdnm.f

# Note that all these files must be built.
BUILT_SOURCES = IftranRun $(libncarg_ezmapa_la_SOURCES)

# Track dependencies to allow parallel builds.
mapaci.f: IftranRun
mapbla.f: IftranRun
mapblm.f: IftranRun
mapgrm.f: IftranRun
mapiqa.f: IftranRun
mapiqm.f: IftranRun
mapita.f: IftranRun
mapitm.f: IftranRun
maplmm.f: IftranRun
mdpaci.f: IftranRun
mdpbla.f: IftranRun
mdpblm.f: IftranRun
mdpchm.f: IftranRun
mdpgrm.f: IftranRun
mdpiqa.f: IftranRun
mdpiqm.f: IftranRun
mdpita.f: IftranRun
mdpitm.f: IftranRun
mdplmm.f: IftranRun
mdrdnm.f: IftranRun

# Target to trigger building of all fortran files.
IftranRun : CodeIftran
	@$(IFTRAN) < CodeIftran > CodeFortran
	@$(FSPLIT) -both CodeFortran
	touch IftranRun

# Include all source code in dist.
EXTRA_DIST = CodeIftran

CLEANFILES = $(BUILT_SOURCES) CodeFortran
