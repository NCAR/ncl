/*
 *	$Id: s_gclsg.c,v 1.1 1997-03-05 19:12:46 haley Exp $
 */
/*
 *  Close Segment
 */

#include <ncarg/gks.h>

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
