/*
 *	$Id: c_wmvmap.c,v 1.2 2008-07-23 16:17:09 haley Exp $
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

extern void NGCALLF(wmvectmap,WMVECTMAP)(float*,float*,float*,float*);

void c_wmvectmap
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
    NGCALLF(wmvectmap,WMVECTMAP)(&x,&y,&u,&v);
}
