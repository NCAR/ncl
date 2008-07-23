/*
 *	$Id: c_getsi.c,v 1.5 2008-07-23 16:17:01 haley Exp $
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

extern void NGCALLF(getsi,GETSI)(int*,int*);

void c_getsi
#ifdef NeedFuncProto
(
    int *ix,
    int *iy 
)
#else
(ix,iy)
    int *ix;
    int *iy;
#endif
{
    NGCALLF(getsi,GETSI)(ix,iy);
}
