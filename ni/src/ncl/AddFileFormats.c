#ifdef __cpluplus
extern "C" {
#endif
#include <stdio.h>
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/NresDB.h>

#include "defs.h"
#include "NclDataDefs.h"
#include "NclFileInterfaces.h"

extern int grib_version;

extern NclFormatFunctionRec GribRec;
#include "NclGRIB2.h"
extern NclFormatFunctionRec Grib2Rec;

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
	_NclRegisterFormat(NetCdfAddFileFormat,"netcdf");
	_NclRegisterFormat(HDFAddFileFormat,"hdf");
#ifdef BuildHDFEOS
	_NclRegisterFormat(HDFEOSAddFileFormat,"hdfeos");
	_NclRegisterFormat(HDFEOSAddFileFormat,"he2");
	_NclRegisterFormat(HDFEOSAddFileFormat,"he4");
#endif
	_NclRegisterFormat(HDFAddFileFormat,"hd");
	_NclRegisterFormat(GribAddFileFormat,"grb");
	_NclRegisterFormat(GribAddFileFormat,"grib");
	_NclRegisterFormat(GribAddFileFormat,"grib2");
	_NclRegisterFormat(GribAddFileFormat,"grb2");
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

        case 2:
            return(&Grib2Rec);
            break;

        case -1:
            /* fallthrough */

        default:
            return NULL;   
    }
}



#ifdef __cpluplus
}
#endif
