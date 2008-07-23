/*
 *	$Id: c_hlsrgb.c,v 1.5 2008-07-23 16:16:41 haley Exp $
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

extern void NGCALLF(hlsrgb,HLSRGB)(float*,float*,float*,float*,float*,float*);

void c_hlsrgb
#ifdef NeedFuncProto
(
    float h,
    float l,
    float s,
    float *r,
    float *g,
    float *b
)
#else
(h,l,s,r,g,b)
    float h;
    float l;
    float s;
    float *r;
    float *g;
    float *b;
#endif
{
    NGCALLF(hlsrgb,HLSRGB)( &h, &l, &s, r, g, b );
}   
