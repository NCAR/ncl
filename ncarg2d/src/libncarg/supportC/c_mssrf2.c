/*
 *	$Id: c_mssrf2.c,v 1.1 1997-04-11 17:45:00 haley Exp $
 */
#include <ncarg/ncargC.h>

float c_mssrf2 
#ifdef NeedFuncProto
(
    float xx,
    float yy,
    int m,
    int n,
    float *x,
    float *y,
    float *z,
    int iz,
    float *zp,
    float sigma
)
#else
(xx,yy,m,n,x,y,z,iz,zp,sigma)
    float xx;
    float yy;
    int m;
    int n;
    float *x;
    float *y;
    float *z;
    int iz;
    float *zp;
    float sigma;
#endif
{
    float xx2, yy2, sigma2, xmssrf2;
    extern float NGCALLF(mssrf2,MSSRF2)();
    sigma2 = sigma;
    xx2 = xx;
    yy2 = yy;

    xmssrf2 = (float)NGCALLF(mssrf2,MSSRF2)(&xx2,&yy2,&m,&n,x,y,z,&iz,zp,&sigma2);
    return(xmssrf2);
}
