


#ifdef __cpluplus
extern "C" {
#endif
#include <stdio.h>
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/NresDB.h>

#include <ncarg/ncl/defs.h>
#include <ncarg/ncl/NclDataDefs.h>
#include <ncarg/ncl/NclFileInterfaces.h>

extern NclFormatFunctionRecPtr NetCdfAddFileFormat(
#if     NhlNeedProto
void
#endif
);
extern NclFormatFunctionRecPtr HDFAddFileFormat(
#if     NhlNeedProto
void
#endif
);


void NclAddUserFileFormats
#if NhlNeedProto
(void)
#else
()
#endif
{
	return;
}

#ifdef __cpluplus
}
#endif
