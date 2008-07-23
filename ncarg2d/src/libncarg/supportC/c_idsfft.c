/*
 *	$Id: c_idsfft.c,v 1.4 2008-07-23 16:17:04 haley Exp $
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

extern void NGCALLF(idsfft,IDSFFT)(int*,int*,float*,float*,float*,int*,int*,
								   int*,float*,float*,float*,int*,float*);

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
