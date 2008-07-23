/*
 *	$Id: c_mskrv2.c,v 1.5 2008-07-23 16:17:04 haley Exp $
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

extern void NGCALLF(mskrv2,MSKRV2)(float*,float*,float*,int*,float*,float*,
                                   float*,float*,float*,float*,int*,float*);

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
    NGCALLF(mskrv2,MSKRV2)(&t,xs,ys,&n,x,y,xp,yp,s,&sigma,&ics,slp);
}
