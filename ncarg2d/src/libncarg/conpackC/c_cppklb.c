/*
 *	$Id: c_cppklb.c,v 1.1 1997-04-11 17:41:10 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_cppklb
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
    NGCALLF(cppklb,CPPKLB)(zdat,rwrk,iwrk);
}
