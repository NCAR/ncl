/*
 *	$Id: s_gdawk.c,v 1.1 1997-03-05 19:12:47 haley Exp $
 */
/*
 * Deactivate workstation 
 */

#include <ncarg/gks.h>

void gdeactivate_ws
#ifdef NeedFuncProto
(
    Gint ws_id  /* workstation identifier */
)
#else
( ws_id )
    Gint ws_id;
#endif
{
    NGCALLF(gdawk,GDAWK)(&ws_id);
}
