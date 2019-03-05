# This is part of the NCL package.
# Ed Hartnett 9/1/18

LDADD = ${top_builddir}/common/src/libncarg_c/libncarg_c.la
LDADD += ${top_builddir}/ncarg2d/src/libncarg/libncarg.la
LDADD += ${top_builddir}/ncarg2d/src/libncarg_gks/libncarg_gks.la
LDADD += ${top_builddir}/ngmath/src/lib/gridpack/libngmath.la

bin_PROGRAMS = WritePlotcharData

WRITEPLOTCHARDATA = ${top_builddir}/ncarg2d/src/db/plotchar/WritePlotcharData

WritePlotcharData_SOURCES = pccchk.f pcdchk.f pcexcd.f pcwb15.f	\
pcwbin.f pcwrda.f

PlotcharData: WritePlotcharData
	$(WRITEPLOTCHARDATA)

# Install this file.
ncarg_databasedir = $(prefix)/lib/ncarg/database
ncarg_database_HEADERS = PlotcharData

EXTRA_DIST = pwritxd1 pwritxd2

CLEANFILES = PlotcharData
