/*
 *	$Id: s_gcrsg.c,v 1.1 1997-03-05 19:12:46 haley Exp $
 */
/*
 *  Create Segment
 */

#include <ncarg/gks.h>

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
