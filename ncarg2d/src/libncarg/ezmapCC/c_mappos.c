/*
 *	$Id: c_mappos.c,v 1.2 2008-07-23 16:16:48 haley Exp $
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

extern void NGCALLF(mappos,MAPPOS)(float*,float*,float*,float*);

void c_mappos
#ifdef NeedFuncProto
(
    float arg1,
    float arg2,
    float arg3,
    float arg4
)
#else
(arg1,arg2,arg3,arg4)
    float arg1;
    float arg2;
    float arg3;
    float arg4;
#endif
{
    NGCALLF(mappos,MAPPOS)(&arg1,&arg2,&arg3,&arg4);
}
