/*
 *      $Id: c_tditri.c,v 1.1 1997-06-30 21:47:33 kennison Exp $
 */
#include <ncarg/ncargC.h>

void c_tditri
#ifdef NeedFuncProto
(
    float *u,
    int    nu,
    float *v,
    int    nv,
    float *w,
    int    nw,
    float *f,
    int    lf1d,
    int    lf2d,
    float  fiso,
    float *rtri,
    int    mtri,
    int   *ntri,
    int    irst
)
#else
(u,nu,v,nv,w,nw,f,lf1d,lf2d,fiso,rtri,mtri,ntri,irst)
    float *u;
    int    nu;
    float *v;
    int    nv;
    float *w;
    int    nw;
    float *f;
    int    lf1d;
    int    lf2d;
    float  fiso;
    float *rtri;
    int    mtri;
    int   *ntri;
    int    irst;
#endif
{
    int nu2,nv2,nw2,lf1d2,lf2d2;
    float fiso2;
    int mtri2,irst2;
    nu2=nu;
    nv2=nv;
    nw2=nw;
    lf1d2=lf1d;
    lf2d2=lf2d;
    fiso2=fiso;
    mtri2=mtri;
    irst2=irst;
    NGCALLF(tditri,TDITRI)(u,&nu2,v,&nv2,w,&nw2,f,&lf1d2,&lf2d2,&fiso2,
                                               rtri,&mtri2,ntri,&irst2);
}
