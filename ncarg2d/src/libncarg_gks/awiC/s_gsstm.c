/*
 *	$Id: s_gsstm.c,v 1.3 2000-08-01 14:36:00 haley Exp $
 */
/************************************************************************
*                                                                       *
*                Copyright (C)  2000                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
* This file is free software; you can redistribute it and/or modify     *
* it under the terms of the GNU Lesser General Public License as        *
* published by the Free Software Foundation; either version 2.1 of the  *
* License, or (at your option) any later version.                       *
*                                                                       *
* This software is distributed in the hope that it will be useful, but  *
* WITHOUT ANY WARRANTY; without even the implied warranty of            *
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
* Lesser General Public License for more details.                       *
*                                                                       *
* You should have received a copy of the GNU Lesser General Public      *
* License along with this software; if not, write to the Free Software  *
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307   *
* USA.                                                                  *
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
