/*
 *	$Id: c_cpcldr.c,v 1.1 1997-04-11 17:41:01 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_cpcldr
#ifdef NeedFuncProto
(
    float *zdat,
    float *rwrk,
    int *iwrk
)
#else
(zdat,rwrk,iwrk)
    float *zdat;
    float *rwrk;
    int *iwrk;
#endif
{
    NGCALLF(cpcldr,CPCLDR)(zdat,rwrk,iwrk);
}
