/*
 *  $Id: s_guwk.c,v 1.1 1997-03-05 19:13:34 haley Exp $
 */
/*
 *  Update workstation   
 */

#include <ncarg/gks.h>

void gupd_ws
#ifdef NeedFuncProto
(
    Gint            ws_id,           /* workstation identifier   */
    Gupd_regen_flag upd_regen_flag   /* update regeneration flag */
)
#else
( ws_id, upd_regen_flag )
    Gint            ws_id;
    Gupd_regen_flag upd_regen_flag;
#endif
{
    NGCALLF(guwk,GUWK)(&ws_id,&upd_regen_flag);
}
