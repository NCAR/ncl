#include <stdio.h>
#include <stdlib.h>
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/NresDB.h>
#include <ncarg/hlu/Callbacks.h>
#include "defs.h"
#include <netcdf.h>
#include "NclDataDefs.h"
#include "NclFileInterfaces.h"
#include "NclMdInc.h"
#include "DataSupport.h"
#include "NclCCM.h"
#include <math.h>
#include "Symbol.h"
#include "Machine.h"
#include "parser.h"
#include "TypeSupport.h"
#include "NclBuiltInSupport.h"


static unsigned char cray_missing_value[8] = { 0x40,0x78,0xc0,0x97,0xce,0x7b,0xc9,0x07 };


extern int COSGetRecord(
#if NhlNeedProto
int /*fd*/, 
int /*block_number*/,
int /*offset*/,
char **/*buffer*/,
int* /*finish_block*/,
int* /*finish_offset*/
#endif
);

extern char EndOfRec(
);

extern forward_index(
#if NhlNeedProto
unsigned char *
#endif
);

extern double DoubleIt(
#if NhlNeedProto
char* /*buffer*/
#endif
);
extern int IntIt(
#if NhlNeedProto
char* /*buffer*/
#endif
);
extern float FloatIt(
#if NhlNeedProto
char* /*buffer*/
#endif
);

extern int* IntEm(
#if NhlNeedProto
char* /*buffer*/,
int /*nwords*/
#endif
);
extern float* FloatEm(
#if NhlNeedProto
char* /*buffer*/,
int /*nwords*/
#endif
);
extern double *DoubleEm(
#if NhlNeedProto
char* /*buffer*/,
int /*nwords*/
#endif
);





static int IsCOSBlocked
#if NhlNeedProto
(int fd)
#else
(fd)
int fd;
#endif
{
        BCW cw;
        char bytes[8][511];

        read(fd,&cw,sizeof(BCW));

        if ( cw.type != 0 || cw.bnhi != 0 || cw.bnlo != 0 ) return 0;

         /* Skip over the rest of the first block. */

        if ( read(fd, bytes, WORD_SIZE*511) != 511*WORD_SIZE ) return 0;

        /* Read and interpret second control word. */

        if ( read(fd, &cw, sizeof( BCW )) != sizeof(BCW) ) return 0;

        if ( cw.type != 0 || cw.bnhi != 0 || cw.bnlo != 1 ) return 0;

        lseek(fd,0,SEEK_SET);

        return(1);
}




NhlErrorTypes _NclICrayBinNumRec
#if NhlNeedProto
(void)
#else
()
#endif
{
        NclStackEntry data;
        NclMultiDValData tmp_md;
        string *fpath;
        NclScalar missing;
        int     has_missing = 0;
        unsigned char    control_word[WORD_SIZE];
        int fd = -1;
        int ind;
	int cb_off = 0;
        int cb = 0;
        int i;
        int dimsize = 1;
	long real_offset = 0;
        long end_offset = 0;
        int total = 0;
        int len;
        int n;
        int index = 0;
        char tmpc;
	int done = 0;


	fpath = (string*)NclGetArgValue(
                0,
                1,
                NULL,
                NULL,
                &missing,
                &has_missing,
                NULL,
                0);
        if(has_missing &&(missing.stringval == *fpath)) {
                NhlPError(NhlFATAL,NhlEUNKNOWN,"fbinnumrec: path is a missing value, can't continue");
                return(NhlFATAL);
        }
        fd = open(_NGResolvePath(NrmQuarkToString(*fpath)),O_RDONLY);

        if(fd == -1) {
                NhlPError(NhlFATAL,NhlEUNKNOWN,"fbinnumrec: could not open (%s) check path and permissions, can't continue",NrmQuarkToString(*fpath));
                return(NhlFATAL);
        }
	if(IsCOSBlocked(fd)) {
		cb = 0;
		cb_off = 0;
		real_offset = cb * BLOCK_SIZE + cb_off * WORD_SIZE;

		lseek(fd,real_offset,SEEK_SET);
		n = read(fd,control_word,WORD_SIZE);
		if(n != WORD_SIZE)  {
/*
* ERROR
*/
			return(NhlFATAL);
		}
		while(!done) {
			while(forward_index(control_word) == 0) {
				end_offset += WORD_SIZE;
				n = read(fd,control_word,WORD_SIZE);
				if(n != WORD_SIZE) {
	/*
	* ERROR
	*/
					done = 1;	
					break;
	/*
					return(NhlFATAL);
	*/
				}
			}
			if(!done) {
				control_word[0] = control_word[0] & (char)0017;
				while((EndOfRec(control_word) != CEOR)){
					end_offset += WORD_SIZE;
					len = forward_index(control_word);
					end_offset += len* WORD_SIZE;
					end_offset = lseek(fd,end_offset,SEEK_SET);
					n = read(fd,control_word,WORD_SIZE);
					if(n != WORD_SIZE) {
		/*
		* ERROR
		*/
						return(NhlFATAL);
					}
				}
				total = total + 1;
			}
		}
		return(NclReturnValue(
                	&total,
                	1,
                	&dimsize,
                	NULL,
                	NCL_int,
                	1
		        ));

	} else {
                NhlPError(NhlFATAL,NhlEUNKNOWN,"craybinnumrec: (%s) is not a COS blocked file. Can not read",NrmQuarkToString(*fpath));
		close(fd);
		return(NhlFATAL);
	}
}
