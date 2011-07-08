#ifdef __cpluplus
extern "C" {
#endif
#include <stdio.h>
#ifdef NIO_LIB_ONLY
#include "niohlu.h"
#include "nioNresDB.h"
#else
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/NresDB.h>
#endif

#include "defs.h"
#include "NclDataDefs.h"
#include "NclFileInterfaces.h"

extern int grib_version;
extern NclFormatFunctionRec GribRec;

#ifdef BuildGRIB2

#include "NclGRIB2.h"
extern NclFormatFunctionRec Grib2Rec;

#endif   /* BuildGRIB2 */

#ifdef BuildHDFEOS
extern NclFormatFunctionRecPtr HDFEOSAddFileFormat(
#if	NhlNeedProto
void
#endif
);
#endif

#ifdef BuildHDF4
extern NclFormatFunctionRecPtr HDFAddFileFormat(
#if	NhlNeedProto
void
#endif
);
#endif

#ifdef BuildHDFEOS5
extern NclFormatFunctionRecPtr HDFEOS5AddFileFormat(
#if     NhlNeedProto
void
#endif
);
#endif

#ifdef BuildHDF5
extern NclFormatFunctionRecPtr HDF5AddFileFormat(
#if     NhlNeedProto
void
#endif
);
#endif

#ifdef BuildOPENDAP
extern NclFormatFunctionRecPtr OpenDapAddFileFormat(
#if     NhlNeedProto
void
#endif
);
#endif

extern NclFormatFunctionRecPtr NetCdfAddFileFormat(
#if	NhlNeedProto
void
#endif
);

#ifdef USE_NETCDF4_FEATURES
extern NclFormatFunctionRecPtr NC4AddFileFormat(
#if	NhlNeedProto
void
#endif
);
#endif

extern NclFormatFunctionRecPtr GribAddFileFormat(
#if	NhlNeedProto
void
#endif
);
extern NclFormatFunctionRecPtr CcmAddFileFormat(
#if	NhlNeedProto
void
#endif
);

#ifdef BuildGDAL
extern NclFormatFunctionRecPtr OGRAddFileFormat(
#if	NhlNeedProto
void
#endif
);

extern NclFormatFunctionRecPtr NewOGRAddFileFormat(void);
#endif

void _NclAddFileFormats
#if	NhlNeedProto
(void)
#else 
()
#endif
{
	_NclRegisterFormat(NetCdfAddFileFormat,"cdf");
	_NclRegisterFormat(NetCdfAddFileFormat,"nc");
	_NclRegisterFormat(NetCdfAddFileFormat,"nc3");
	_NclRegisterFormat(NetCdfAddFileFormat,"nc4");
	_NclRegisterFormat(NetCdfAddFileFormat,"netcdf");
#ifdef BuildHDF4
	_NclRegisterFormat(HDFAddFileFormat,"hdf");
	_NclRegisterFormat(HDFAddFileFormat,"hd");
	_NclRegisterFormat(HDFAddFileFormat,"h4");
#endif
#ifdef BuildHDFEOS
	_NclRegisterFormat(HDFEOSAddFileFormat,"hdfeos");
	_NclRegisterFormat(HDFEOSAddFileFormat,"he2");
	_NclRegisterFormat(HDFEOSAddFileFormat,"he4");
#endif
#ifdef BuildHDFEOS5
	_NclRegisterFormat(HDFEOS5AddFileFormat,"hdfeos5");
	_NclRegisterFormat(HDFEOS5AddFileFormat,"he5");
#endif
#ifdef BuildHDF5
        _NclRegisterFormat(HDF5AddFileFormat,"h5");
#endif
	_NclRegisterFormat(GribAddFileFormat,"gr");
	_NclRegisterFormat(GribAddFileFormat,"gr1");
	_NclRegisterFormat(GribAddFileFormat,"grb");
	_NclRegisterFormat(GribAddFileFormat,"grib");
	_NclRegisterFormat(GribAddFileFormat,"grb1");
	_NclRegisterFormat(GribAddFileFormat,"grib1");
#ifdef  BuildGRIB2
	_NclRegisterFormat(GribAddFileFormat,"gr2");
	_NclRegisterFormat(GribAddFileFormat,"grib2");
	_NclRegisterFormat(GribAddFileFormat,"grb2");
#endif  /* BuildGRIB2 */
	_NclRegisterFormat(CcmAddFileFormat,"ccm");

#ifdef BuildOPENDAP
	_NclRegisterFormat(OpenDapAddFileFormat,"opendap");
#endif

#ifdef  BuildGDAL
        /* file types supported by OGR... */
        _NclRegisterFormat(OGRAddFileFormat, "shp");  /* shapefile */
        _NclRegisterFormat(OGRAddFileFormat, "mif");  /* mapinfo */
        _NclRegisterFormat(OGRAddFileFormat, "gmt");  /* GMT   */

        /**** Although GDAL/OGR will recognize any of these TIGER suffixes, the sheer number 
         **** is excessive, confusing, and not all of these file type will be present.
         **** Since the "rt1" file is required, we'll adopt it as the conventional suffix.
         **** -- RLB, 5/2009
         ****/
        _NclRegisterFormat(OGRAddFileFormat, "rt1");  /* TIGER */
        /** _NclRegisterFormat(OGRAddFileFormat, "rt2");     */
        /** _NclRegisterFormat(OGRAddFileFormat, "rt3");     */  /* for pre-2002 files */
        /** _NclRegisterFormat(OGRAddFileFormat, "rt4");     */
        /** _NclRegisterFormat(OGRAddFileFormat, "rt5");     */
        /** _NclRegisterFormat(OGRAddFileFormat, "rt6");     */
        /** _NclRegisterFormat(OGRAddFileFormat, "rt7");     */
        /** _NclRegisterFormat(OGRAddFileFormat, "rt8");     */
        /** _NclRegisterFormat(OGRAddFileFormat, "rt9");     */  /* for pre-2002 files */
        /** _NclRegisterFormat(OGRAddFileFormat, "rta");     */
        /** _NclRegisterFormat(OGRAddFileFormat, "rtb");     */
        /** _NclRegisterFormat(OGRAddFileFormat, "rtc");     */
        /** _NclRegisterFormat(OGRAddFileFormat, "rte");     */
        /** _NclRegisterFormat(OGRAddFileFormat, "rth");     */
        /** _NclRegisterFormat(OGRAddFileFormat, "rti");     */
        /** _NclRegisterFormat(OGRAddFileFormat, "rtm");     */
        /** _NclRegisterFormat(OGRAddFileFormat, "rtp");     */
        /** _NclRegisterFormat(OGRAddFileFormat, "rtr");     */
        /** _NclRegisterFormat(OGRAddFileFormat, "rts");     */
        /** _NclRegisterFormat(OGRAddFileFormat, "rtt");     */
        /** _NclRegisterFormat(OGRAddFileFormat, "rtu");     */
        /** _NclRegisterFormat(OGRAddFileFormat, "rtz");     */
        /* TIGER: see http://www.census.gov/geo/www/tiger/tiger2006se/tgr2006se.html */
#endif

	/*These is for NetCDF4.
	 *where this file will be scanned to find the second match.
	 *The new file-structure is used when found the second match.
	 */
#ifdef USE_NETCDF4_FEATURES
	_NclRegisterFormat(NC4AddFileFormat,"cdf");
	_NclRegisterFormat(NC4AddFileFormat,"nc");
	_NclRegisterFormat(NC4AddFileFormat,"nc3");
	_NclRegisterFormat(NC4AddFileFormat,"nc4");
	_NclRegisterFormat(NC4AddFileFormat,"netcdf");

#ifdef  BuildGDAL
        /* file types supported by OGR in new file structure */
        _NclRegisterFormat(NewOGRAddFileFormat, "shp");  /* shapefile */
        _NclRegisterFormat(NewOGRAddFileFormat, "mif");  /* mapinfo */
        _NclRegisterFormat(NewOGRAddFileFormat, "gmt");  /* GMT   */
#endif
#endif

	return;
}

NclFormatFunctionRecPtr GribAddFileFormat 
#if	NhlNeedProto
(void)
#else 
()
#endif
{
    switch (grib_version) {
        case 0:
            /* fallthrough */

        case 1:
            return(&GribRec);
            break;

#ifdef  BuildGRIB2
        case 2:
            return(&Grib2Rec);
            break;
#endif  /* BuildGRIB2 */

        case -1:
            /* fallthrough */

        default:
            return NULL;   
    }
}



#ifdef __cpluplus
}
#endif
