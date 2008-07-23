/*
 *	$Id: s_gcrsg.c,v 1.5 2008-07-23 17:24:19 haley Exp $
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
 *  Create Segment
 */

#include <ncarg/gks.h>

extern void NGCALLF(gcrsg,GCRSG)(Gint*);

void gcreate_seg
#ifdef NeedFuncProto
(
    Gint seg_name  /* segment name */
)
#else
( seg_name )
    Gint seg_name;
#endif
{
    NGCALLF(gcrsg,GCRSG)(&seg_name);
}
