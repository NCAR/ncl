/*
 *	$Id: s_gdsg.c,v 1.5 2008-07-23 17:24:19 haley Exp $
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
 *  Delete Segment
 */

#include <ncarg/gks.h>

extern void NGCALLF(gdsg,GDSG)(Gint*);

void gdel_seg
#ifdef NeedFuncProto
(
    Gint seg_name  /* segment name */
)
#else
( seg_name )
    Gint seg_name;
#endif
{
    NGCALLF(gdsg,GDSG)(&seg_name);
}
