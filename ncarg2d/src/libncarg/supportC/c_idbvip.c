/*
 *	$Id: c_idbvip.c,v 1.1 1997-04-11 17:58:54 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_idbvip
#ifdef NeedFuncProto
(
    int md,
    int ndp,
    float *xd,
    float *yd,
    float *zd,
    int nip,
    float *xi,
    float *yi,
    float *zi,
    int *iwk,
    float *wk
)
#else
(md,ndp,xd,yd,zd,nip,xi,yi,zi,iwk,wk)
    int md;
    int ndp;
    float *xd;
    float *yd;
    float *zd;
    int nip;
    float *xi;
    float *yi;
    float *zi;
    int *iwk;
    float *wk;
#endif
{
    NGCALLF(idbvip,IDBVIP)(&md,&ndp,xd,yd,zd,&nip,xi,yi,zi,iwk,wk);
}
