/*
 *	$Id: s_gcsgwk.c,v 1.4 2000-08-22 15:08:44 haley Exp $
 */
/************************************************************************
*                                                                       *
*                Copyright (C)  2000                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
* This file is free software; you can redistribute it and/or modify     *
* it under the terms of the GNU General Public License as published     *
* by the Free Software Foundation; either version 2 of the License, or  *
* (at your option) any later version.                                   *
*                                                                       *
* This software is distributed in the hope that it will be useful, but  *
* WITHOUT ANY WARRANTY; without even the implied warranty of            *
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
* General Public License for more details.                              *
*                                                                       *
* You should have received a copy of the GNU General Public License     *
* along with this software; if not, write to the Free Software         *
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307   *
* USA.                                                                  *
*                                                                       *
************************************************************************/

/*
 *  Copy Segment to Workstation
 */

#include <ncarg/gks.h>

extern void NGCALLF(gcsgwk,GCSGWK)(Gint*,Gint*);

void gcopy_seg_ws
#ifdef NeedFuncProto
(
    Gint ws_id,    /* workstation identifier */
    Gint seg_name  /* segment name           */
)
#else
( ws_id, seg_name )
    Gint ws_id;     /* workstation identifier */
    Gint seg_name;  /* segment name           */
#endif
{
    NGCALLF(gcsgwk,GCSGWK)(&ws_id,&seg_name);
}
