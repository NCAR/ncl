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

extern NclFormatFunctionRecPtr NetCdfAddFileFormat(
#if	NhlNeedProto
void
#endif
);
extern NclFormatFunctionRecPtr HDFAddFileFormat(
#if	NhlNeedProto
void
#endif
);

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
	_NclRegisterFormat(HDFAddFileFormat,"hdf");
#ifdef BuildHDFEOS
	_NclRegisterFormat(HDFEOSAddFileFormat,"hdfeos");
	_NclRegisterFormat(HDFEOSAddFileFormat,"he2");
	_NclRegisterFormat(HDFEOSAddFileFormat,"he4");
#endif
	_NclRegisterFormat(HDFAddFileFormat,"hd");
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
