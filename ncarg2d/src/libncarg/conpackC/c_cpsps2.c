/*
 *	$Id: c_cpsps2.c,v 1.1 1997-04-11 17:41:14 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_cpsps2
#ifdef NeedFuncProto
(
    float *xsps,
    float *ysps,
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
(xsps,ysps,zsps,ksps,msps,nsps,rwrk,krwk,iwrk,kiwk,zdat,kzdt)
    float *xsps;
    float *ysps;
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
    NGCALLF(cpsps2,CPSPS2)(xsps,ysps,zsps,&ksps,&msps,&nsps,rwrk,&krwk,iwrk,&kiwk,zdat,&kzdt);
}
