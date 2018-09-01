# This is part of the NCL package.
# Ed Hartnett 9/1/18

LDADD = ${top_builddir}/common/src/libncarg_c/libncarg_c.la
LDADD += ${top_builddir}/ncarg2d/src/libncarg/libncarg.la

bin_PROGRAMS = ConvertMapData WriteLineFile WriteNameFile

WRITENAMEFILE = ${top_builddir}/ncarg2d/src/db/ezmap/WriteNameFile
WRITELINEFILE = ${top_builddir}/ncarg2d/src/db/ezmap/WriteLineFile
CONVERTMAPDATA = ${top_builddir}/ncarg2d/src/db/ezmap/ConvertMapData

ConvertMapData_SOURCES = ConvertMapData.f
WriteLineFile_SOURCES = WriteLineFile.f
WriteNameFile_SOURCES = WriteNameFile.f

# Output to /dev/null because WriteLineFile and WriteNameFile each
# generates a line of output we don't want to show.
Earth..1.lines: WriteLineFile
	echo Earth..1 | $(WRITELINEFILE) > /dev/null
Earth..2.lines: WriteLineFile
	echo Earth..2 | $(WRITELINEFILE) > /dev/null
Earth..3.lines: WriteLineFile
	echo Earth..3 | $(WRITELINEFILE) > /dev/null
Earth..4.lines: WriteLineFile
	echo Earth..4 | $(WRITELINEFILE) > /dev/null

Earth..1.names: WriteNameFile
	echo Earth..1 | $(WRITENAMEFILE) > /dev/null
Earth..2.names: WriteNameFile
	echo Earth..2 | $(WRITENAMEFILE) > /dev/null
Earth..3.names: WriteNameFile
	echo Earth..3 | $(WRITENAMEFILE) > /dev/null
Earth..4.names: WriteNameFile
	echo Earth..4 | $(WRITENAMEFILE) > /dev/null

EzmapOutlines.PO: ConvertMapData
	$(CONVERTMAPDATA)
EzmapOutlines.PS: ConvertMapData EzmapOutlines.PO
EzmapOutlines.US: ConvertMapData EzmapOutlines.PO
EzmapOutlines.CO: ConvertMapData EzmapOutlines.PO

# Install these data files.
ncarg_databasedir = $(prefix)/lib/ncarg/database
ncarg_database_HEADERS = Earth..1.lines Earth..2.lines Earth..3.lines	\
Earth..4.lines Earth..1.names Earth..2.names Earth..3.names		\
Earth..4.names EzmapOutlines.PO EzmapOutlines.PS EzmapOutlines.US	\
EzmapOutlines.CO

EXTRA_DIST = Earth..1 Earth..2 Earth..3 Earth..4

CLEANFILES = $(ncarg_database_HEADERS)
