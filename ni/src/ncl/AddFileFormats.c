#ifdef __cpluplus
extern "C" {
#endif
#include <stdio.h>
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/NresDB.h>

#include "defs.h"
#include "NclDataDefs.h"
#include "NclFileInterfaces.h"

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


#ifdef __cpluplus
}
#endif
