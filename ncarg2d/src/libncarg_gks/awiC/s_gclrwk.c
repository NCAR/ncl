/*
 *	$Id: s_gclrwk.c,v 1.5 2008-07-23 17:24:19 haley Exp $
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
 *  Clear workstation  
 */

#include <ncarg/gks.h>

extern void NGCALLF(gclrwk,GCLRWK)(Gint*,Gctrl_flag*);

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
