/*
 *	$Id: c_hsvrgb.c,v 1.5 2008-07-23 16:16:41 haley Exp $
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

extern void NGCALLF(hsvrgb,HSVRGB)(float*,float*,float*,float*,float*,float*);

void c_hsvrgb
#ifdef NeedFuncProto
(
    float h,
    float s,
    float v,
    float *r,
    float *g,
    float *b
)
#else
(h,s,v,r,g,b)
    float h;
    float s;
    float v;
    float *r;
    float *g;
    float *b;
#endif
{
    NGCALLF(hsvrgb,HSVRGB)(&h,&s,&v,r,g,b);
}
