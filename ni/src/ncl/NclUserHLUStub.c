#ifdef __cpluplus
extern "C" {
#endif
#include <stdio.h>
#include <ncarg/hlu/hluP.h>
#include <ncarg/hlu/BaseP.h>

extern void _NclAddSingleObj(
#if     NhlNeedProto
char * /*name*/,
struct _NhlClassRec * /* the_ptr */
#endif
);

void NclAddUserHLUObjs
#if	NhlNeedProto
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

