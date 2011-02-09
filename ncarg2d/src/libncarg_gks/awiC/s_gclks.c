/*
 *	$Id: s_gclks.c,v 1.4 2008-07-23 17:24:19 haley Exp $
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

/*
 *  Close workstation  
 */

#include <ncarg/gks.h>

extern void NGCALLF(gclks,GCLKS)();

void gclose_gks
#ifdef NeedFuncProto
(
    void
)
#else
()
#endif
{
    NGCALLF(gclks,GCLKS)();
}
