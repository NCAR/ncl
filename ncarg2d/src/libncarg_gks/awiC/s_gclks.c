/*
 *	$Id: s_gclks.c,v 1.1 1997-03-05 19:12:45 haley Exp $
 */
/*
 *  Close workstation  
 */

#include <ncarg/gks.h>

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
