/*
 *	$Id: s_gclrwk.c,v 1.1 1997-03-05 19:12:46 haley Exp $
 */
/*
 *  Clear workstation  
 */

#include <ncarg/gks.h>

void gclear_ws
#ifdef NeedFuncProto
(
    Gint ws_id,             /* workstation identifier */
    Gctrl_flag  ctrl_flag   /* control flag           */
)
#else
( ws_id, ctrl_flag )
    Gint ws_id;
    Gctrl_flag  ctrl_flag;
#endif
{
    NGCALLF(gclrwk,GCLRWK)(&ws_id,&ctrl_flag);
}
