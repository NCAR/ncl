/*
 *	$Id: c_mapvec.c,v 1.2 2008-07-23 16:16:48 haley Exp $
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

extern void NGCALLF(mapvec,MAPVEC)(float*,float*);

void c_mapvec
#ifdef NeedFuncProto
(
    float xlat,
    float xlon
)
#else
(xlat,xlon)
    float xlat;
    float xlon;
#endif
{
    NGCALLF(mapvec,MAPVEC)(&xlat,&xlon);
}
