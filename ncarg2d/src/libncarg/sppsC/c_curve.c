/*
 *	$Id: c_curve.c,v 1.5 2008-07-23 16:17:01 haley Exp $
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

extern void NGCALLF(curve,CURVE)(float*,float*,int*);

void c_curve
#ifdef NeedFuncProto
(
    float *px,
    float *py,
    int np
)
#else
(px,py,np)
    float *px;
    float *py;
    int np;
#endif
{
    NGCALLF(curve,CURVE)(px,py,&np);
}
