/*
 *      $Id: c_tdprpa.c,v 1.1 1997-06-30 21:47:43 kennison Exp $
 */
#include <ncarg/ncargC.h>

void c_tdprpa
#ifdef NeedFuncProto
(
    float  xipa,
    float  yipa,
    float *xi2d,
    float *yi2d
)
#else
(xipa,yipa,xi2d,yi2d)
    float  xipa;
    float  yipa;
    float *xi2d;
    float *yi2d;
#endif
{
    float xipa2,yipa2;
    xipa2=xipa;
    yipa2=yipa;
    NGCALLF(tdprpa,TDPRPA)(&xipa2,&yipa2,xi2d,yi2d);
}
