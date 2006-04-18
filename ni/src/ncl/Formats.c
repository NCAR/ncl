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
#if	NhlNeedProto
(NclAddFileFormat thefunc,char* file_extension)
#else
(thefunc,file_extension)
NclAddFileFormat thefunc;
char* file_extension;
#endif
{
	static int first = 1;
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
			NhlPError(NhlFATAL,NhlEUNKNOWN,"A memory allocation error occurred could not register new format");
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
#if	NhlNeedProto
(NclQuark file_extension)
#else 
(file_extension)
	NclQuark file_extension;
#endif
{
	int i; 
	char ext[16];
        char *cp;

        strncpy(ext,NrmQuarkToString(file_extension),sizeof(ext));
        ext[sizeof(ext)-1] = '\0';
	for (cp = ext; *cp != '\0'; cp++) {
		*cp = tolower(*cp);	
        }
        file_extension = NrmStringToQuark(ext);
	
	for(i = 0; i<num_formats; i++) {
		if(formats[i].file_extension == file_extension) {
			return((*formats[i].format_func)());
		}
	}
	return(NULL);
}

logical _NclFormatEqual
#if NhlNeedProto
(
	NclQuark file_ext1, 
	NclQuark file_ext2
)
#else
(file_ext1, file_ext2)
NclQuark file_ext1;
NclQuark file_ext2;
#endif
{
	NclAddFileFormat format_func1;
	char ext[16];
        char *cp;
	NclQuark qext1,qext2;
	int i;

        strncpy(ext,NrmQuarkToString(file_ext1),sizeof(ext));
        ext[sizeof(ext)-1] = '\0';
	for (cp = ext; *cp != '\0'; cp++) {
		*cp = tolower(*cp);	
        }
        qext1 = NrmStringToQuark(ext);
	for(i = 0; i<num_formats; i++) {
		if(formats[i].file_extension == qext1) {
			format_func1 = formats[i].format_func;
		}
	}

        strncpy(ext,NrmQuarkToString(file_ext2),sizeof(ext));
        ext[sizeof(ext)-1] = '\0';
	for (cp = ext; *cp != '\0'; cp++) {
		*cp = tolower(*cp);	
        }
        qext2 = NrmStringToQuark(ext);
	for(i = 0; i<num_formats; i++) {
		if(formats[i].file_extension == qext2) {
			if (format_func1 == formats[i].format_func)
				return 1;
			return 0;
		}
	}
	return 0;
}

