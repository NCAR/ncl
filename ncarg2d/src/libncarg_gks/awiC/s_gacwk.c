/*
 *	$Id: s_gacwk.c,v 1.1 1997-03-05 19:12:45 haley Exp $
 */
/*
 *  Activate workstation  
 */

#include <ncarg/gks.h>

void gactivate_ws
#ifdef NeedFuncProto
(
    Gint ws_id  /* workstation identifier */
)
#else
( ws_id )
    Gint ws_id;
#endif
{
    NGCALLF(gacwk,GACWK)(&ws_id);
}
