/*
 *      $Id: c_tdstri.c,v 1.1 1997-06-30 21:47:52 kennison Exp $
 */
#include <ncarg/ncargC.h>

void c_tdstri
#ifdef NeedFuncProto
(
    float *u,
    int    nu,
    float *v,
    int    nv,
    float *w,
    int    lw1d,
    float *rtri,
    int    mtri,
    int   *ntri,
    int    irst
)
#else
(u,nu,v,nv,w,lw1d,rtri,mtri,ntri,irst)
    float *u;
    int    nu;
    float *v;
    int    nv;
    float *w;
    int    lw1d;
    float *rtri;
    int    mtri;
    int   *ntri;
    int    irst;
#endif
{
    int nu2,nv2,lw1d2,mtri2,irst2;
    nu2=nu;
    nv2=nv;
    lw1d2=lw1d;
    mtri2=mtri;
    irst2=irst;
    NGCALLF(tdstri,TDSTRI)(u,&nu2,v,&nv2,w,&lw1d2,rtri,&mtri2,ntri,&irst2);
}
