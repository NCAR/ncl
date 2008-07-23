/*
 *      $Id: c_tditri.c,v 1.5 2008-07-23 16:17:06 haley Exp $
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

extern void NGCALLF(tditri,TDITRI)(float*,int*,float*,int*,float*,int*,
                                   float*,int*,int*,float*,float*,int*,
                                   int*,int*);

void c_tditri
#ifdef NeedFuncProto
(
    float *u,
    int    nu,
    float *v,
    int    nv,
    float *w,
    int    nw,
    float *f,
    int    lf1d,
    int    lf2d,
    float  fiso,
    float *rtri,
    int    mtri,
    int   *ntri,
    int    irst
)
#else
(u,nu,v,nv,w,nw,f,lf1d,lf2d,fiso,rtri,mtri,ntri,irst)
    float *u;
    int    nu;
    float *v;
    int    nv;
    float *w;
    int    nw;
    float *f;
    int    lf1d;
    int    lf2d;
    float  fiso;
    float *rtri;
    int    mtri;
    int   *ntri;
    int    irst;
#endif
{
    NGCALLF(tditri,TDITRI)(u,&nu,v,&nv,w,&nw,f,&lf1d,&lf2d,&fiso,
                                           rtri,&mtri,ntri,&irst);
}
