/*
 *	$Id: c_cpback.c,v 1.1 1997-04-11 17:40:59 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_cpback
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
    NGCALLF(cpback,CPBACK)(zdat,rwrk,iwrk);
}
