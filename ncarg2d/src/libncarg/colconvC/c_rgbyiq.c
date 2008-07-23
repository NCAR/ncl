/*
 *	$Id: c_rgbyiq.c,v 1.5 2008-07-23 16:16:42 haley Exp $
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

extern void NGCALLF(rgbyiq,RGBYIQ)(float*,float*,float*,float*,float*,float*);

void c_rgbyiq 
#ifdef NeedFuncProto
(
    float r,
    float g,
    float b,
    float *y,
    float *i,
    float *q
)
#else
 ( r, g, b, y, i, q )
    float r;
    float g;
    float b;
    float *y;
    float *i;
    float *q;
#endif
{
    NGCALLF(rgbyiq,RGBYIQ)( &r, &g, &b, y, i, q );
}
