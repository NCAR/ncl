/*
 *      $Id: c_tdmtri.c,v 1.1 1997-06-30 21:47:39 kennison Exp $
 */
#include <ncarg/ncargC.h>

void c_tdmtri
#ifdef NeedFuncProto
(
    int    imrk,
    float  umrk,
    float  vmrk,
    float  wmrk,
    float  smrk,
    float *rtri,
    int    mtri,
    int   *ntri,
    int    irst,
    float  umin,
    float  vmin,
    float  wmin,
    float  umax,
    float  vmax,
    float  wmax
)
#else
(imrk,umrk,vmrk,wmrk,smrk,rtri,mtri,ntri,irst,umin,vmin,wmin,umax,vmax,wmax)
    int    imrk;
    float  umrk;
    float  vmrk;
    float  wmrk;
    float  smrk;
    float *rtri;
    int    mtri;
    int   *ntri;
    int    irst;
    float  umin;
    float  vmin;
    float  wmin;
    float  umax;
    float  vmax;
    float  wmax;
#endif
{
    int   imrk2;
    float umrk2;
    float vmrk2;
    float wmrk2;
    float smrk2;
    int   mtri2;
    int   irst2;
    float umin2;
    float vmin2;
    float wmin2;
    float umax2;
    float vmax2;
    float wmax2;
    imrk2=imrk;
    umrk2=umrk;
    vmrk2=vmrk;
    wmrk2=wmrk;
    smrk2=smrk;
    mtri2=mtri;
    irst2=irst;
    umin2=umin;
    vmin2=vmin;
    wmin2=wmin;
    umax2=umax;
    vmax2=vmax;
    wmax2=wmax;
    NGCALLF(tdmtri,TDMTRI)(&imrk2,&umrk2,&vmrk2,&wmrk2,&smrk2,rtri,&mtri2,
                   ntri,&irst2,&umin2,&vmin2,&wmin2,&umax2,&vmax2,&wmax2);
}
