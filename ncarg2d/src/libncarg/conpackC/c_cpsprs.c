/*
 *	$Id: c_cpsprs.c,v 1.1 1997-04-11 17:41:13 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_cpsprs
#ifdef NeedFuncProto
(
    float *zsps,
    int ksps,
    int msps,
    int nsps,
    float *rwrk,
    int krwk,
    int *iwrk,
    int kiwk,
    float *zdat,
    int kzdt
)
#else
(zsps,ksps,msps,nsps,rwrk,krwk,iwrk,kiwk,zdat,kzdt)
    float *zsps;
    int ksps;
    int msps;
    int nsps;
    float *rwrk;
    int krwk;
    int *iwrk;
    int kiwk;
    float *zdat;
    int kzdt;
#endif
{
    NGCALLF(cpsprs,CPSPRS)(zsps,&ksps,&msps,&nsps,rwrk,&krwk,iwrk,&kiwk,zdat,&kzdt);
}
