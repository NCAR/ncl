/*
 *	$Id: c_cpcldm.c,v 1.1 1997-04-11 17:41:00 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_cpcldm
#ifdef NeedFuncProto
(
    float *zdat,
    float *rwrk,
    int *iwrk,
    int *iama,
    int (*rtpl_)(
        float *xcra,
        float *ycra,
        int *ncra,
        int *iaia,
        int *igia,
        int *nagi
              )
)
#else
(zdat,rwrk,iwrk,iama,rtpl_)
    float *zdat;
    float *rwrk;
    int *iwrk;
    int *iama;
    int (*rtpl_)();
#endif
{
    NGCALLF(cpcldm,CPCLDM)(zdat,rwrk,iwrk,iama,rtpl_);
}
