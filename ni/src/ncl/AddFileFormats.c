#ifdef __cpluplus
extern "C" {
#endif
#include <stdio.h>
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/NresDB.h>

#include <data_objs/NclVar.h>
#include <data_objs/NclMultiDValdoubleData.h>
#include <data_objs/NclMultiDValfloatData.h>
#include <data_objs/NclMultiDValintData.h>
#include <data_objs/NclMultiDValshortData.h>
#include <data_objs/NclMultiDVallongData.h>
#include <data_objs/NclMultiDValstringData.h>
#include <defs.h>
#include <Symbol.h>
#include <y.tab.h>
#include <data_objs/DataSupport.h>
#include <data_objs/NclFileInterfaces.h>
#include <data_objs/NclFile.h>
#include <data_objs/FileSupport.h>

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
