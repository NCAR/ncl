/*
 *	$Id: c_rgbhls.c,v 1.5 2008-07-23 16:16:41 haley Exp $
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

extern void NGCALLF(rgbhls,RGBHLS)(float*,float*,float*,float*,float*,float*);

void c_rgbhls
#ifdef NeedFuncProto
(
    float r,
    float g,
    float b,
    float *h,
    float *l,
    float *s
)
#else
( r, g, b, h, l, s )
    float r;
    float g;
    float b;
    float *h;
    float *l;
    float *s;
#endif
{
    NGCALLF(rgbhls,RGBHLS)( &r, &g, &b, h, l, s );
}
