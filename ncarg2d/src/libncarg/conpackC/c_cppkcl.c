/*
 *	$Id: c_cppkcl.c,v 1.1 1997-04-11 17:41:09 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_cppkcl
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
    NGCALLF(cppkcl,CPPKCL)(zdat,rwrk,iwrk);
}    
