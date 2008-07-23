/*
 *	$Id: c_mapbla.c,v 1.2 2008-07-23 16:16:46 haley Exp $
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

extern void NGCALLF(mapbla,MAPBLA)(int*);

void c_mapbla
#ifdef NeedFuncProto
(
    int *iamp
)
#else
(iamp)
    int *iamp;
#endif
{
    NGCALLF(mapbla,MAPBLA)(iamp);
}
