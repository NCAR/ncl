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

int GribVersion
#if NhlNeedProto
(NclQuark path)
#else
(path)
    NclQuark path;
#endif
{
# define GBUFSZ_T 1024
    unsigned char buf[4 * GBUFSZ_T];
    int len,
    version;

    int i,
        j;

    static void *vbuf;
    FILE    *fd;

    fd = fopen(NrmQuarkToString(path), "r");
    vbuf = (void *) NclMalloc(4 * getpagesize());
    setvbuf(fd, vbuf, _IOFBF, 4 * getpagesize());

    (void) fseek(fd, 0L, SEEK_SET);
    i = 0;
    while (i < 100) {
        len = fread((void*) buf, 1, 4 * GBUFSZ_T, fd);
        if (len > 0) {
            for (j = 0; j < len; j++) {
                /* look for "GRIB" indicator */
                if (buf[j] != 'G') {
                    continue;
                } else {
                    if ((buf[j + 1] == 'R' && buf[j + 2] == 'I' && buf[j + 3] == 'B')) {
                        version = buf[j + 7];
                        break;
                    }
                }
            }
            i++;
        }

    }

    if (fd) {
        (void) fseek(fd, 0L, SEEK_SET);
        fclose(fd);
    }

    return version;
}
