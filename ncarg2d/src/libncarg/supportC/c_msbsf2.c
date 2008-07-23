/*
 *	$Id: c_msbsf2.c,v 1.5 2008-07-23 16:17:04 haley Exp $
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

extern void NGCALLF(msbsf2,MSBSF2)(float*,float*,int*,float*,float*,int*,
                                   float*,int*,int*,int*,float*,float*,
                                   float*,float*,float*,int*,float*,float*,
                                   float*);


void c_msbsf2
#ifdef NeedFuncProto
(
    float dxmin,
    float dxmax,
    int md,
    float dymin,
    float dymax,
    int nd,
    float *dz,
    int idz,
    int m,
    int n,
    float xmin,
    float xmax,
    float ymin,
    float ymax,
    float *z,
    int iz,
    float *zp,
    float *work,
    float sigma
)
#else
(dxmin,dxmax,md,dymin,dymax,nd,dz,idz,m,n,xmin,xmax,ymin,ymax,z,iz,zp,work,sigma)
    float dxmin;
    float dxmax;
    int md;
    float dymin;
    float dymax;
    int nd;
    float *dz;
    int idz;
    int m;
    int n;
    float xmin;
    float xmax;
    float ymin;
    float ymax;
    float *z;
    int iz;
    float *zp;
    float *work;
    float sigma;
#endif
{
    NGCALLF(msbsf2,MSBSF2)(&dxmin,&dxmax,&md,&dymin,&dymax,&nd,dz,&idz,
                           &m,&n,&xmin,&xmax,&ymin,&ymax,z,&iz,zp,work,
                           &sigma);
}
