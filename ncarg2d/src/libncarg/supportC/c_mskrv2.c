/*
 *	$Id: c_mskrv2.c,v 1.1 1997-04-11 17:44:58 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_mskrv2 
#ifdef NeedFuncProto
(
    float t,
    float *xs,
    float *ys,
    int n,
    float *x,
    float *y,
    float *xp,
    float *yp,
    float *s,
    float sigma,
    int ics,
    float *slp
)
#else
(t,xs,ys,n,x,y,xp,yp,s,sigma,ics,slp)
    float t;
    float *xs;
    float *ys;
    int n;
    float *x;
    float *y;
    float *xp;
    float *yp;
    float *s;
    float sigma;
    int ics;
    float *slp;
#endif
{
    float t2, sigma2;

    t2 = t;
    sigma2 = sigma;

    NGCALLF(mskrv2,MSKRV2)(&t2,xs,ys,&n,x,y,xp,yp,s,&sigma2,&ics,slp);
}
