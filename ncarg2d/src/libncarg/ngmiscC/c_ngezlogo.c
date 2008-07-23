/*
 *	$Id: c_ngezlogo.c,v 1.2 2008-07-23 16:16:57 haley Exp $
 */
/************************************************************************
*                                                                       *
*                Copyright (C)  2002                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
*    The use of this Software is governed by a License Agreement.       *
*                                                                       *
************************************************************************/

#include <ncarg/ncargC.h>

extern void NGCALLF(ngezlogo,NGEZLOGO)();

void c_ngezlogo
#ifdef NeedFuncProto
(
)
#else
()
#endif
{
    NGCALLF(ngezlogo,NGEZLOGO)();
}
