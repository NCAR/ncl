/*
 *	$Id: c_points.c,v 1.5 2008-07-23 16:17:02 haley Exp $
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

extern void NGCALLF(points,POINTS)(float*,float*,int*,int*,int*);

void c_points
#ifdef NeedFuncProto
(
    float *px,
    float *py,
    int np,
    int ic,
    int il
)
#else
(px,py,np,ic,il)
    float *px;
    float *py;
    int np;
    int ic;
    int il;
#endif
{
    NGCALLF(points,POINTS)(px,py,&np,&ic,&il);
}
