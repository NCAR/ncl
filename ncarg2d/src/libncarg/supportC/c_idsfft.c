/*
 *	$Id: c_idsfft.c,v 1.1 1997-04-11 17:58:55 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_idsfft
#ifdef NeedFuncProto
(
    int md,
    int ndp,
    float *xd,
    float *yd,
    float *zd,
    int nxi,
    int nyi,
    int nzi,
    float *xi,
    float *yi,
    float *zi,
    int *iwk,
    float *wk
)
#else
(md,ndp,xd,yd,zd,nxi,nyi,nzi,xi,yi,zi,iwk,wk)
    int md;
    int ndp;
    float *xd;
    float *yd;
    float *zd;
    int nxi;
    int nyi;
    int nzi;
    float *xi;
    float *yi;
    float *zi;
    int *iwk;
    float *wk;
#endif
{
    NGCALLF(idsfft,IDSFFT)(&md,&ndp,xd,yd,zd,&nxi,&nyi,&nzi,xi,yi,zi,iwk,wk);
}
