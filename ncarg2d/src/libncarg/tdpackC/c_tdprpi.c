/*
 *      $Id: c_tdprpi.c,v 1.1 1997-06-30 21:47:45 kennison Exp $
 */
#include <ncarg/ncargC.h>

void c_tdprpi
#ifdef NeedFuncProto
(
    float  xi2d,
    float  yi2d,
    float *xipa,
    float *yipa
)
#else
(xi2d,yi2d,xipa,yipa)
    float  xi2d;
    float  yi2d;
    float *xipa;
    float *yipa;
#endif
{
    float xi2d2,yi2d2;
    xi2d2=xi2d;
    yi2d2=yi2d;
    NGCALLF(tdprpi,TDPRPI)(&xi2d2,&yi2d2,xipa,yipa);
}
