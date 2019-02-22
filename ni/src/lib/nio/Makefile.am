# This is an automake file for NCL.
# Ed Hartnett 9/6/18

# Find gks.h.
AM_CPPFLAGS = -I$(top_srcdir)/ncarg2d/src/libncarg_gks

# Find c.h.
AM_CPPFLAGS += -I$(top_srcdir)/common/src/libncarg_c

# Find ncargC.h.
AM_CPPFLAGS += -I$(top_srcdir)/ncarg2d/src/libncarg

AM_CPPFLAGS += -I$(top_srcdir)/ni/src/lib/hlu -I$(top_srcdir)/ni/src/ncl

lib_LTLIBRARIES = libnio.la

libnio_la_SOURCES = cmpf.c nioBase.c nio.c nioCallbacks.c		\
nioConvert.c nioError.c niohlu.c nio_io.c nioNclHLUStub.c nioNresDB.c	\
nio_path.c nioQuarks.c nioUtils.c

if USE_NETCDF4
libnio_la_SOURCES += NclNetCDF4.c
endif

if USE_HDF4
libnio_la_SOURCES += NclHDF.c
endif

if USE_HDFEOS
libnio_la_SOURCES += NclHDFEOS.c
endif

if USE_HDF5
libnio_la_SOURCES += NclHDF5.c NclNewHDF5.c h5reader.c h5writer.c
endif

if USE_GRIB2
libnio_la_SOURCES += NclGRIB2.c
endif

if USE_GDAL
libnio_la_SOURCES += NclOGR.c
endif

include_HEADERS = cmpf.h nioBaseI.h nioCallbacks.h nioc.h		\
nioConvertP.h nio.h niohluP.h nioNresDB.h nioResList.h nioVarArg.h	\
nioBase.h nioBaseP.h nioCallbacksP.h nioConvert.h nioError.h niohlu.h	\
nioNgSizeT.h nio_path.h nioUtils.h
