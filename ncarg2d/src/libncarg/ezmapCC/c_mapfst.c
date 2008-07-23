/*
 *	$Id: c_mapfst.c,v 1.2 2008-07-23 16:16:46 haley Exp $
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

extern void NGCALLF(mapfst,MAPFST)(float*,float*);

void c_mapfst
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
    NGCALLF(mapfst,MAPFST)(&xlat,&xlon);
}
