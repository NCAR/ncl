/*
 *	$Id: c_mssrf1.c,v 1.5 2008-07-23 16:17:05 haley Exp $
 */
/************************************************************************
*                                                                       *
*                Copyright (C)  2000                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
*    The use of this Software is governed by a License Agreement.       *
*                                                                       *
************************************************************************/

#include <ncarg/ncargC.h>

extern void NGCALLF(mssrf1,MSSRF1)(int*,int*,float*,float*,float*,int*,
                                   float*,float*,float*,float*,float*,
                                   float*,float*,float*,int*,float*,float*,
                                   float*,int*);


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
    NGCALLF(mssrf1,MSSRF1)(&m,&n,x,y,z,&iz,zx1,zxm,zy1,zyn,&zxy11,&zxym1,
                           &zxy1n,&zxymn,&islpsw,zp,temp,&sigma,ierr);
}
