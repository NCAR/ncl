/*
 *	$Id: c_cpclam.c,v 1.1 1997-04-11 17:41:00 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_cpclam
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
    NGCALLF(cpclam,CPCLAM)(zdat,rwrk,iwrk,iama);
}
