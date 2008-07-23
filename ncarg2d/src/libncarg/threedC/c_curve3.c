/*
 *	$Id: c_curve3.c,v 1.5 2008-07-23 16:17:07 haley Exp $
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

extern void NGCALLF(curve3,CURVE3)(float*,float*,float*,int*);

void c_curve3 
#ifdef NeedFuncProto
(
    float *u,
    float *v,
    float *w,
    int n
)
#else
 (u,v,w,n)
    float *u;
    float *v;
    float *w;
    int n;
#endif
{
    NGCALLF(curve3,CURVE3)(u,v,w,&n);
}
