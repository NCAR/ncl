#include <stdio.h>
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/NresDB.h>
#include "defs.h"
#include "NclFile.h"
#include "NclFileInterfaces.h"

typedef struct _NclFormatList NclFormatList;

struct _NclFormatList {
NclQuark  file_extension;
NclAddFileFormat format_func;
};

NclFormatList *formats;
int format_list_size = 32;
int num_formats = 0;


void _NclRegisterFormat
#if  __STDC__
(NclAddFileFormat thefunc,char* file_extension)
#else
(thefunc,file_extension)
NclAddFileFormat thefunc;
char* file_extension;
#endif
{
	static first = 1;
	NclFormatList* tmp;

	if(first) {
		formats = (NclFormatList*)NclMalloc(
				(unsigned)sizeof(NclFormatList)
				*format_list_size);
		first = 0;
	}

	if(num_formats + 1 == format_list_size) {
		tmp = (NclFormatList*)NclRealloc((void*)formats,
				(unsigned)sizeof(NclFormatList) 
				*format_list_size * 2);
		if(tmp == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"A memory allocation error occured could not register new format");
			return;
		} else {
			formats = tmp;
			format_list_size *= 2;
		}
	}
	formats[num_formats].file_extension = NrmStringToQuark(file_extension);
	formats[num_formats].format_func = thefunc;
	num_formats++;
	return;
}

NclFormatFunctionRecPtr _NclGetFormatFuncs
#if  __STDC__
(NclQuark file_extension)
#else 
(file_extension)
	NclQuark file_extension;
#endif
{
	int i; 
	for(i = 0; i<num_formats; i++) {
		if(formats[i].file_extension == file_extension) {
			return((*formats[i].format_func)());
		}
	}
	return(NULL);
}
