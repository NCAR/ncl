/*
 *	$Id: c_cplbam.c,v 1.1 1997-04-11 17:41:05 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_cplbam
#ifdef NeedFuncProto
(
    float *zdat,
    float *rwrk,
    int *iwrk,
    int *iama
)
#else
(zdat,rwrk,iwrk,iama)
    float *zdat;
    float *rwrk;
    int *iwrk;
    int *iama;
#endif
{
    NGCALLF(cplbam,CPLBAM)(zdat,rwrk,iwrk,iama);
}
