/*
 *  $Id: s_guwk.c,v 1.5 2008-07-23 17:24:25 haley Exp $
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
 *  Update workstation   
 */

#include <ncarg/gks.h>

extern void NGCALLF(guwk,GUWK)(Gint*,Gupd_regen_flag*);

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
