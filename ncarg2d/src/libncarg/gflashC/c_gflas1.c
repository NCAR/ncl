/*
 *	$Id: c_gflas1.c,v 1.5 2008-07-23 16:16:54 haley Exp $
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

extern void NGCALLF(gflas1,GFLAS1)(int*);

void c_gflas1
#ifdef NeedFuncProto
(
    int iname
)
#else
(iname)
    int iname;
#endif
{
   NGCALLF(gflas1,GFLAS1)(&iname);
}
