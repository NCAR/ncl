/*
 *	$Id: c_velvct.c,v 1.5 2008-07-23 16:17:08 haley Exp $
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

extern void NGCALLF(velvct,VELVCT)(float*,int*,float*,int*,int*,int*,float*,
                                   float*,int*,int*,int*,float*);

void c_velvct
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
    int length,
    int ispv,
    float *spv
)
#else
(u,lu,v,lv,m,n,flo,hi,nset,length,ispv,spv)
    float *u;
    int lu;
    float *v;
    int lv;
    int m;
    int n;
    float flo;
    float hi;
    int nset;
    int length;
    int ispv;
    float *spv;
#endif
{
    NGCALLF(velvct,VELVCT)(u,&lu,v,&lv,&m,&n,&flo,&hi,&nset,&length,&ispv,spv);
}
