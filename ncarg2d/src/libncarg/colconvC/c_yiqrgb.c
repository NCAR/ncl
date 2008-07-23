/*
 *	$Id: c_yiqrgb.c,v 1.5 2008-07-23 16:16:42 haley Exp $
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

extern void NGCALLF(yiqrgb,YIQRGB)(float*,float*,float*,float*,float*,float*);

void c_yiqrgb
#ifdef NeedFuncProto
(
    float y,
    float i,
    float q,
    float *r,
    float *g,
    float *b
)
#else
 ( y, i, q, r, g, b )
    float y;
    float i;
    float q;
    float *r;
    float *g;
    float *b;
#endif
{
    NGCALLF(yiqrgb,YIQRGB)( &y, &i, &q, r, g, b );
}
