/*
 *	$Id: c_cplbdr.c,v 1.1 1997-04-11 17:41:05 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_cplbdr
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
    NGCALLF(cplbdr,CPLBDR)(zdat,rwrk,iwrk);
}
