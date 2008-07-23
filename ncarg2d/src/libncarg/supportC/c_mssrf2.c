/*
 *	$Id: c_mssrf2.c,v 1.5 2008-07-23 16:17:05 haley Exp $
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

extern float NGCALLF(mssrf2,MSSRF2)(float*,float*,int*,int*,float*,float*,
									float*,int*,float*,float*);

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
    float xmssrf2;

    xmssrf2 = (float)NGCALLF(mssrf2,MSSRF2)(&xx,&yy,&m,&n,x,y,z,&iz,zp,
                                            &sigma);
    return(xmssrf2);
}
