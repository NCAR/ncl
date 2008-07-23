/*
 *	$Id: c_mapsav.c,v 1.2 2008-07-23 16:16:48 haley Exp $
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

extern void NGCALLF(mapsav,MAPSAV)(int*);

void c_mapsav
#ifdef NeedFuncProto
(
    int ifno
)
#else
(ifno)
    int ifno;
#endif
{
    NGCALLF(mapsav,MAPSAV)(&ifno);
}
