/*
 *	$Id: c_point3.c,v 1.5 2008-07-23 16:17:07 haley Exp $
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

extern void NGCALLF(point3,POINT3)(float*,float*,float*);

void c_point3
#ifdef NeedFuncProto
(
    float u,
    float v,
    float w
)
#else
(u,v,w)
    float u;
    float v;
    float w;
#endif
{
    NGCALLF(point3,POINT3)(&u,&v,&w);
}
