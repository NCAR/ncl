/*
 *      $Id: c_tdgrid.c,v 1.1 1997-06-30 21:47:29 kennison Exp $
 */
#include <ncarg/ncargC.h>

void c_tdgrid
#ifdef NeedFuncProto
(
    float xbeg,
    float xstp,
    int   noxs,
    float ybeg,
    float ystp,
    int   noys,
    int   igrd
)
#else
(xbeg,xstp,noxs,ybeg,ystp,noys,igrd)
    float xbeg;
    float xstp;
    int   noxs;
    float ybeg;
    float ystp;
    int   noys;
    int   igrd;
#endif
{
    float xbeg2,xstp2;
    int   noxs2;
    float ybeg2,ystp2;
    int   noys2,igrd2;
    xbeg2=xbeg;
    xstp2=xstp;
    noxs2=noxs;
    ybeg2=ybeg;
    ystp2=ystp;
    noys2=noys;
    igrd2=igrd;
    NGCALLF(tdgrid,TDGRID)(&xbeg2,&xstp2,&noxs2,&ybeg2,&ystp2,&noys2,&igrd2);
}
