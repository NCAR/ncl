/*
 *	$Id: c_mskrv1.c,v 1.1 1997-04-11 17:44:57 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_mskrv1 
#ifdef NeedFuncProto
(
    int n,
    float *x,
    float *y,
    float slp1,
    float slpn,
    float *xp,
    float *yp,
    float *temp,
    float *s,
    float sigma,
    int islpsw
)
#else
(n,x,y,slp1,slpn,xp,yp,temp,s,sigma,islpsw)
    int n;
    float *x;
    float *y;
    float slp1;
    float slpn;
    float *xp;
    float *yp;
    float *temp;
    float *s;
    float sigma;
    int islpsw;
#endif
{
    float slp12, slpn2, sigma2;

    slp12 = slp1;
    slpn2 = slpn;
    sigma2 = sigma;
    NGCALLF(mskrv1,MSKRV1)(&n,x,y,&slp12,&slpn2,xp,yp,temp,s,&sigma2,&islpsw);
}
