/*
 *	$Id: c_rgbhsv.c,v 1.5 2008-07-23 16:16:41 haley Exp $
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

extern void NGCALLF(rgbhsv,RGBHSV)(float*,float*,float*,float*,float*,float*);

void c_rgbhsv
#ifdef NeedFuncProto
(
    float r,
    float g,
    float b,
    float *h,
    float *s,
    float *v
)
#else
( r, g, b, h, s, v )
    float r;
    float g;
    float b;
    float *h;
    float *s;
    float *v;
#endif
{
    NGCALLF(rgbhsv,RGBHSV)( &r, &g, &b, h, s, v );
}   
