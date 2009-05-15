
#ifdef __cplusplus
extern "C" {
#endif
#include <stdio.h>
#include <string.h>
#include "niohlu.h"
#include "nioVarArg.h"
#include "defs.h"
FILE *stdout_fp;


FILE *_NclGetOutputStream
#if     NhlNeedProto
(void)
#else
()
#endif
{
	stdout_fp = stdout;
        return(stdout_fp);
}

/*
int (*pit)(
FILE *,
const char *,
va_list 
);
*/

NclVaPrintFunc pit = vfprintf;
int nclfprintf
#if NhlNeedVarArgProto
(FILE *fp,char *fmt,...)
#else
(fp,format,va_alist)
	FILE *fp;
	char *fmt;
	va_dcl
#endif
{
	int ret = 0;
	va_list ap;
	VA_START(ap,fmt);
	ret = (*pit)(fp,fmt,ap);
	va_end(ap);
	return(ret);
}

NclVaPrintFunc _NclSetPrintFunc
#if	NhlNeedProto
(NclVaPrintFunc thepit)
#else
(thepit)
NclVaPrintFunc thepit;
#endif
{
	NclVaPrintFunc tmp;

	tmp = pit;
	pit = thepit;
	return(tmp);
}
