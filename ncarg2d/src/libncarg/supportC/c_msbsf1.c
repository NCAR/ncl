/*
 *	$Id: c_msbsf1.c,v 1.5 2008-07-23 16:17:04 haley Exp $
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

extern void NGCALLF(msbsf1,MSBSF1)(int*,int*,float*,float*,float*,float*,
                                   float*,int*,float*,float*,float*);


void c_msbsf1 
#ifdef NeedFuncProto
(
    int m,
    int n,
    float xmin,
    float xmax,
    float ymin,
    float ymax,
    float *z,
    int iz,
    float *zp,
    float *temp,
    float sigma
)
#else
(m,n,xmin,xmax,ymin,ymax,z,iz,zp,temp,sigma)
    int m;
    int n;
    float xmin;
    float xmax;
    float ymin;
    float ymax;
    float *z;
    int iz;
    float *zp;
    float *temp;
    float sigma;
#endif
{
    NGCALLF(msbsf1,MSBSF1)(&m,&n,&xmin,&xmax,&ymin,&ymax,z,&iz,zp,temp,
                           &sigma);
}
