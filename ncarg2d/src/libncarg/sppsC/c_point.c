/*
 *	$Id: c_point.c,v 1.5 2008-07-23 16:17:02 haley Exp $
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

extern void NGCALLF(point,POINT)(float*,float*);

void c_point
#ifdef NeedFuncProto
(
    float px,
    float py
)
#else
(px,py)
    float px;
    float py;
#endif
{
    NGCALLF(point,POINT)(&px,&py);
}
