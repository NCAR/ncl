/*
 *	$Id: c_cprect.c,v 1.1 1997-04-11 17:41:10 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_cprect
#ifdef NeedFuncProto
(
    float *zdat,
    int kzdt,
    int mzdt,
    int nzdt,
    float *rwrk,
    int krwk,
    int *iwrk,
    int kiwk
)
#else
(zdat,kzdt,mzdt,nzdt,rwrk,krwk,iwrk,kiwk)
    float *zdat;
    int kzdt;
    int mzdt;
    int nzdt;
    float *rwrk;
    int krwk;
    int *iwrk;
    int kiwk;
#endif
{
    NGCALLF(cprect,CPRECT)(zdat,&kzdt,&mzdt,&nzdt,rwrk,&krwk,iwrk,&kiwk);
}
