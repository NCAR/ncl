/*
 *  $Id: s_gswkwn.c,v 1.2 2000-07-12 17:06:25 haley Exp $
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
 *  Set workstation window  
 */

#include <ncarg/gks.h>

void gset_ws_win
#ifdef NeedFuncProto
(
    Gint   ws_id,           /* workstation identifier    */
    Glimit *ws_win_limits   /* workstation window limits */
)
#else
( ws_id, ws_win_limits )
    Gint   ws_id;
    Glimit *ws_win_limits;
#endif
{
    NGCALLF(gswkwn,GSWKWN)(&ws_id,&ws_win_limits->x_min,&ws_win_limits->x_max,
                                  &ws_win_limits->y_min,&ws_win_limits->y_max);
}
