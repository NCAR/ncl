/*
 *	$Id: c_dashdb.c,v 1.5 2008-07-23 16:16:45 haley Exp $
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

extern void NGCALLF(dashdb,DASHDB)(int*);

void c_dashdb
#ifdef NeedFuncProto
(
    int *ipat
)
#else
(ipat)
    int *ipat;
#endif
{
    NGCALLF(dashdb,DASHDB)(ipat);
}
