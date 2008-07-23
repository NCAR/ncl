/*
 *	$Id: c_velvec.c,v 1.5 2008-07-23 16:17:08 haley Exp $
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

extern void NGCALLF(velvec,VELVEC)(float*,int*,float*,int*,int*,int*,float*,
                                   float*,int*,int*,float*);

void c_velvec
#ifdef NeedFuncProto
(
    float *u,
    int lu,
    float *v,
    int lv,
    int m,
    int n,
    float flo,
    float hi,
    int nset,
    int ispv,
    float *spv
)
#else
(u,lu,v,lv,m,n,flo,hi,nset,ispv,spv)
    float *u;
    int lu;
    float *v;
    int lv;
    int m;
    int n;
    float flo;
    float hi;
    int nset;
    int ispv;
    float *spv;
#endif
{
    NGCALLF(velvec,VELVEC)(u,&lu,v,&lv,&m,&n,&flo,&hi,&nset,&ispv,spv);
}
