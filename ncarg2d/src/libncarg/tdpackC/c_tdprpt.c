/*
 *      $Id: c_tdprpt.c,v 1.2 1997-07-02 22:26:59 kennison Exp $
 */
#include <ncarg/ncargC.h>

void c_tdprpt
#ifdef NeedFuncProto
(
    float  ui3d,
    float  vi3d,
    float  wi3d,
    float *xi2d,
    float *yi2d
)
#else
(ui3d,vi3d,wi3d,xi2d,yi2d)
    float  ui3d;
    float  vi3d;
    float  wi3d;
    float *xi2d;
    float *yi2d;
#endif
{
    float ui3d2,vi3d2,wi3d2;
    ui3d2=ui3d;
    vi3d2=vi3d;
    wi3d2=wi3d;
    NGCALLF(tdprpt,TDPRPT)(&ui3d2,&vi3d2,&wi3d2,xi2d,yi2d);
}
