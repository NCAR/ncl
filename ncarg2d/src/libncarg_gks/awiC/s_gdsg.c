/*
 *	$Id: s_gdsg.c,v 1.1 1997-03-05 19:12:47 haley Exp $
 */
/*
 *  Delete Segment
 */

#include <ncarg/gks.h>

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
