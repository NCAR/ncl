/*
 *	$Id: s_gsstm.c,v 1.1 1997-03-05 19:13:29 haley Exp $
 */
/*
 *  Set string mode
 */

#include <ncarg/gks.h>

void gset_string_mode
#ifdef NeedFuncProto
(
    Gint         ws_id,      /* workstation identifier */
    Gint         string_num, /* string device number   */
    Gop_mode     op_mode,    /* operating mode         */
    Gecho_switch echo_switch /* echo switch            */
)
#else
( ws_id, string_num, op_mode, echo_switch )
    Gint         ws_id;
    Gint         string_num;
    Gop_mode     op_mode;
    Gecho_switch echo_switch;
#endif
{
    NGCALLF(gsstm,GSSTM)(&ws_id,&string_num,&op_mode,&echo_switch);
}
