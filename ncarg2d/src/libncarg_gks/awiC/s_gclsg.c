/*
 *	$Id: s_gclsg.c,v 1.4 2008-07-23 17:24:19 haley Exp $
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
 *  Close Segment
 */

#include <ncarg/gks.h>

extern void NGCALLF(gclsg,GCLSG)();

void gclose_seg
#ifdef NeedFuncProto
(
    void
)
#else
()
#endif
{
    NGCALLF(gclsg,GCLSG)();
}
