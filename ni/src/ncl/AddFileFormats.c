#ifdef __cpluplus
extern "C" {
#endif
#include <stdio.h>
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/NresDB.h>

#include "defs.h"
#include "NclDataDefs.h"
#include "NclFileInterfaces.h"

extern NclFormatFunctionRecPtr NetCdfAddFileFormat(
#ifdef NhlNeedProto
void
#endif
);

void _NclAddFileFormats
#if  __STDC__
(void)
#else 
()
#endif
{
	_NclRegisterFormat(NetCdfAddFileFormat,"cdf");
	_NclRegisterFormat(NetCdfAddFileFormat,"nc");
	return;
}


#ifdef __cpluplus
}
#endif
