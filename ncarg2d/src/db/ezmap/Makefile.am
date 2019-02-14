# This is part of the NCL package.
# Ed Hartnett 9/1/18

LDADD = ${top_builddir}/common/src/libncarg_c/libncarg_c.la

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

# Had to change these files names because git (as used by Jenkins)
# could not see them.
Earth..1.areas:
	cp Earth__1_areas Earth..1.areas
Earth..2.areas:
	cp Earth__2_areas Earth..2.areas
Earth..3.areas:
	cp Earth__3_areas Earth..3.areas
Earth..4.areas:
	cp Earth__4_areas Earth..4.areas

EzmapOutlines.PO: ConvertMapData
	$(CONVERTMAPDATA)
EzmapOutlines.PS: ConvertMapData EzmapOutlines.PO
EzmapOutlines.US: ConvertMapData EzmapOutlines.PO
EzmapOutlines.CO: ConvertMapData EzmapOutlines.PO

EzmapAreaInfo.CO:
	cp EzmapAreaInfo_CO EzmapAreaInfo.CO
EzmapAreaInfo.PO:
	cp EzmapAreaInfo_PO EzmapAreaInfo.PO
EzmapAreaInfo.PS:
	cp EzmapAreaInfo_PS EzmapAreaInfo.PS
EzmapAreaInfo.US:
	cp EzmapAreaInfo_US EzmapAreaInfo.US

noinst_PROGRAMS = mapconvrt
mapconvrt_SOURCES = mapconvrt.f
ezmapdata: ezmapdat mapconvrt
	./mapconvrt

# Install these data files.
ncarg_databasedir = $(prefix)/lib/ncarg/database
ncarg_database_HEADERS = Earth..1.lines Earth..2.lines Earth..3.lines	\
Earth..4.lines Earth..1.names Earth..2.names Earth..3.names		\
Earth..4.names EzmapOutlines.PO EzmapOutlines.PS EzmapOutlines.US	\
EzmapOutlines.CO Earth..1.areas Earth..2.areas Earth..3.areas		\
Earth..4.areas EzmapAreaInfo.CO EzmapAreaInfo.PO EzmapAreaInfo.PS	\
EzmapAreaInfo.US ezmap_area_ids ezmapdata

EXTRA_DIST = Earth..1 Earth..2 Earth..3 Earth..4 EzmapAreaInfo_CO	\
EzmapAreaInfo_PO EzmapAreaInfo_PS EzmapAreaInfo_US Earth__1_areas	\
Earth__2_areas Earth__3_areas Earth__4_areas ezmapdat

CLEANFILES = Earth..1.lines Earth..2.lines Earth..3.lines		\
Earth..4.lines Earth..1.names Earth..2.names Earth..3.names		\
Earth..4.names EzmapOutlines.PO EzmapOutlines.PS EzmapOutlines.US	\
EzmapOutlines.CO Earth..1.areas Earth..2.areas Earth..3.areas		\
Earth..4.areas ezmapdata
