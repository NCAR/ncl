/*
 *	$Id: c_stinit.c,v 1.5 2008-07-23 16:17:04 haley Exp $
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

extern void NGCALLF(stinit,STINIT)(float*,int*,float*,int*,float*,int*,
                                   int*,int*,float*,int*);

void c_stinit
#ifdef NeedFuncProto
(
    float *u,
    int lu,
    float *v,
    int lv,
    float *p,
    int lp,
    int m,
    int n,
    float *wrk,
    int lw
)
#else
(u,lu,v,lv,p,lp,m,n,wrk,lw)
    float *u;
    int lu;
    float *v;
    int lv;
    float *p;
    int lp;
    int m;
    int n;
    float *wrk;
    int lw;
#endif
{
    NGCALLF(stinit,STINIT)(u,&lu,v,&lv,p,&lp,&m,&n,wrk,&lw);
}
