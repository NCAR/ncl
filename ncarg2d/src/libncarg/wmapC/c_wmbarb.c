/*
 *	$Id: c_wmbarb.c,v 1.5 2008-07-23 16:17:08 haley Exp $
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

extern void NGCALLF(wmbarb,WMBARB)(float*,float*,float*,float*);

void c_wmbarb
#ifdef NeedFuncProto
(
    float x,
    float y,
    float u,
    float v
)
#else
(x,y,u,v)
    float x;
    float y;
    float u;
    float v;
#endif
{
    NGCALLF(wmbarb,WMBARB)(&x,&y,&u,&v);
}
