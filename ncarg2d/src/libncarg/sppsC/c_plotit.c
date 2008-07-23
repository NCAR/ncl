/*
 *	$Id: c_plotit.c,v 1.5 2008-07-23 16:17:02 haley Exp $
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

extern void NGCALLF(plotit,PLOTIT)(int*,int*,int*);

void c_plotit
#ifdef NeedFuncProto
(
    int ix,
    int iy,
    int ip
)
#else
(ix,iy,ip)
    int ix;
    int iy;
    int ip;
#endif
{
    NGCALLF(plotit,PLOTIT)(&ix,&iy,&ip);
}
