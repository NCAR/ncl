/*
 *  $Id: s_guwk.c,v 1.3 2000-08-01 14:36:02 haley Exp $
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
