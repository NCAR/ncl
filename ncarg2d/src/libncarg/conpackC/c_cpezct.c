/*
 *	$Id: c_cpezct.c,v 1.1 1997-04-11 17:41:03 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_cpezct
#ifdef NeedFuncProto
(
    float *zdat,
    int mzdt,
    int nzdt
)
#else
(zdat,mzdt,nzdt)
    float *zdat;
    int mzdt;
    int nzdt;
#endif
{
    NGCALLF(cpezct,CPEZCT)(zdat,&mzdt,&nzdt);
}
