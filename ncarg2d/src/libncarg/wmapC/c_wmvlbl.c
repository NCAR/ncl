/*
 *	$Id: c_wmvlbl.c,v 1.2 2008-07-23 16:17:09 haley Exp $
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

extern void NGCALLF(wmvlbl,WMVLBL)(float*,float*);

void c_wmvlbl
#ifdef NeedFuncProto
(
    float x,
    float y
)
#else
(x,y,u,v)
    float x;
    float y;
#endif
{
    NGCALLF(wmvlbl,WMVLBL)(&x,&y);
}
