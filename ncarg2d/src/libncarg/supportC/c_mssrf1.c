/*
 *	$Id: c_mssrf1.c,v 1.1 1997-04-11 17:44:59 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_mssrf1
#ifdef NeedFuncProto
(
    int m,
    int n,
    float *x,
    float *y,
    float *z,
    int iz,
    float *zx1,
    float *zxm,
    float *zy1,
    float *zyn,
    float zxy11,
    float zxym1,
    float zxy1n,
    float zxymn,
    int islpsw,
    float *zp,
    float *temp,
    float sigma,
    int *ierr
)
#else
(m,n,x,y,z,iz,zx1,zxm,zy1,zyn,zxy11,zxym1,zxy1n,zxymn,islpsw,zp,temp,sigma,ierr)
    int m;
    int n;
    float *x;
    float *y;
    float *z;
    int iz;
    float *zx1;
    float *zxm;
    float *zy1;
    float *zyn;
    float zxy11;
    float zxym1;
    float zxy1n;
    float zxymn;
    int islpsw;
    float *zp;
    float *temp;
    float sigma;
    int *ierr;
#endif
{
    float sigma2,zxy112,zxym12,zxy1n2,zxymn2;

    zxy112 = zxy11;
    zxym12 = zxym1;
    zxy1n2 = zxy1n;
    zxymn2 = zxymn;
    sigma2 = sigma;

    NGCALLF(mssrf1,MSSRF1)(&m,&n,x,y,z,&iz,zx1,zxm,zy1,zyn,&zxy112,&zxym12,&zxy1n2,&zxymn2,&islpsw,zp,temp,&sigma2,ierr);
}
