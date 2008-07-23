/*
 *      $Id: c_tdstri.c,v 1.5 2008-07-23 16:17:07 haley Exp $
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

extern void NGCALLF(tdstri,TDSTRI)(float*,int*,float*,int*,float*,int*,
                                   float*,int*,int*,int*);

void c_tdstri
#ifdef NeedFuncProto
(
    float *u,
    int    nu,
    float *v,
    int    nv,
    float *w,
    int    lw1d,
    float *rtri,
    int    mtri,
    int   *ntri,
    int    irst
)
#else
(u,nu,v,nv,w,lw1d,rtri,mtri,ntri,irst)
    float *u;
    int    nu;
    float *v;
    int    nv;
    float *w;
    int    lw1d;
    float *rtri;
    int    mtri;
    int   *ntri;
    int    irst;
#endif
{
    NGCALLF(tdstri,TDSTRI)(u,&nu,v,&nv,w,&lw1d,rtri,&mtri,ntri,&irst);
}
