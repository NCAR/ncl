
#include <stdio.h>
#ifdef NIO_LIB_ONLY
#include "niohlu.h"
#include "nioNresDB.h"
#else
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/NresDB.h>
#endif
#include "defs.h"
#include "NclFile.h"
#include "NclFileInterfaces.h"
#include <ctype.h>
#include <unistd.h>

typedef struct _NclFormatList NclFormatList;

struct _NclFormatList {
NclQuark  file_extension;
NclAddFileFormat format_func;
};

NclFormatList *formats;
int format_list_size = 32;
int num_formats = 0;
int grib_version;

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
	NclQuark lfile_ext;

	lfile_ext = _NclGetLower(file_extension);
	
	for(i = 0; i<num_formats; i++) {
		if(formats[i].file_extension == lfile_ext) {
			return((*formats[i].format_func)());
		}
	}
	return(NULL);
}

NclFormatFunctionRecPtr _NclGetFormatFuncsWithAdvancedFileStructure(NclQuark file_extension)
{
	int i, n=1; 
	NclQuark lfile_ext;

	lfile_ext = _NclGetLower(file_extension);
	
	for(i = 0; i<num_formats; i++)
	{
		if(formats[i].file_extension == lfile_ext)
		{
			n = i+1;
			break;
		}
	}

	for(i = n; i<num_formats; i++)
	{
		if(formats[i].file_extension == lfile_ext)
			return((*formats[i].format_func)());
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
	NclAddFileFormat format_func1 = NULL;
	char ext[16];
        char *cp;
	NclQuark qext1,qext2;
	int i;

	qext1 = _NclGetLower(file_ext1);
	for(i = 0; i<num_formats; i++) {
		if(formats[i].file_extension == qext1) {
			format_func1 = formats[i].format_func;
			break;
		}
	}
	if (!format_func1) 
		return 0;

	qext2 = _NclGetLower(file_ext2);
	for(i = 0; i<num_formats; i++) {
		if(formats[i].file_extension == qext2) {
			if (format_func1 == formats[i].format_func)
				return 1;
			return 0;
		}
	}
	return 0;
}

int _NclGribVersion
#if NhlNeedProto
(NclQuark path)
#else
(path)
    NclQuark path;
#endif
{
    ng_size_t gbuf_size = 1024;
    unsigned char buf[4 * gbuf_size];
    int len,
        version = -1;
    int j;

    static void *vbuf;
    FILE    *fd;

    fd = fopen(NrmQuarkToString(path), "r");
    vbuf = (void *) NclMalloc(4 * getpagesize());
    setvbuf(fd, vbuf, _IOFBF, 4 * getpagesize());

    /*
     * Read file, look for sequence 'G' 'R' 'I' 'B' ; version will follow
     */
    (void) fseek(fd, 0L, SEEK_SET);
    while ((len = fread((void *) buf, 1, 4 * gbuf_size, fd)) > 0) {
            for (j = 0; j < len - 8; j++) {
		    /* look for "GRIB" indicator */
		    if (buf[j] != 'G') {
			    continue;
		    } else {
			    if ((buf[j + 1] == 'R' && buf[j + 2] == 'I' && buf[j + 3] == 'B')) {
				    version = buf[j + 7];
				    /* we have the version; return immediately */
				    fclose(fd);
				    NclFree(vbuf);
				    return version;
			    }
		    }
            }
    }

    if (fd) {
        fclose(fd);
    }

    NclFree(vbuf);
    return version;
}
