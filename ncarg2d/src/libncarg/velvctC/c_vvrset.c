/*
 *	$Id: c_vvrset.c,v 1.4 2008-07-23 16:17:08 haley Exp $
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

void c_vvrset
#ifdef NeedFuncProto
(
    void
)
#else
()
#endif
{
    NGCALLF(vvrset,VVRSET)();
}
