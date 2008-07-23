/*
 *	$Id: c_fence3.c,v 1.5 2008-07-23 16:17:07 haley Exp $
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

extern void NGCALLF(fence3,FENCE3)(float*,float*,float*,int*,int*,float*);

void c_fence3
#ifdef NeedFuncProto
(
    float *u,
    float *v,
    float *w,
    int n,
    int ior,
    float bot
)
#else
(u,v,w,n,ior,bot)
    float *u;
    float *v;
    float *w;
    int n;
    int ior;
    float bot;
#endif
{
    NGCALLF(fence3,FENCE3)(u,v,w,&n,&ior,&bot);
}
