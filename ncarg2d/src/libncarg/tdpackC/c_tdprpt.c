/*
 *      $Id: c_tdprpt.c,v 1.1 1997-06-30 21:47:46 kennison Exp $
 */
#include <ncarg/ncargC.h>

void c_tdprpt
#ifdef NeedFuncProto
(
    float  xi3d,
    float  yi3d,
    float  zi3d,
    float *xi2d,
    float *yi2d
)
#else
(xi3d,yi3d,zi3d,xi2d,yi2d)
    float  xi3d;
    float  yi3d;
    float  zi3d;
    float *xi2d;
    float *yi2d;
#endif
{
    float xi3d2,yi3d2,zi3d2;
    xi3d2=xi3d;
    yi3d2=yi3d;
    zi3d2=zi3d;
    NGCALLF(tdprpt,TDPRPT)(&xi3d2,&yi3d2,&zi3d2,xi2d,yi2d);
}
