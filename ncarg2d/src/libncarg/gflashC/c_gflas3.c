/*
 *	$Id: c_gflas3.c,v 1.5 2008-07-23 16:16:55 haley Exp $
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

extern void NGCALLF(gflas3,GFLAS3)(int*);

void c_gflas3
#ifdef NeedFuncProto
(
    int iname
)
#else
(iname)
    int iname;
#endif
{
    NGCALLF(gflas3,GFLAS3)(&iname);
}
