/*
 *	$Id: s_gsstm.c,v 1.5 2008-07-23 17:24:24 haley Exp $
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
 *  Set string mode
 */

#include <ncarg/gks.h>

extern void NGCALLF(gsstm,GSSTM)(Gint*,Gint*,Gop_mode*,Gecho_switch*);

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
