/*
 *  $Id: s_gqwkdu.c,v 1.3 2000-08-01 14:35:56 haley Exp $
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
 *  Inquire workstation deferral and update states  
 */

#include <ncarg/gks.h>

extern void NGCALLF(gqwkdu,GQWKDU)(Gint*,Gint*,Gdefer_mode*,Girg_mode*,
                                   Gdisp_space_empty*,Gnew_frame_nec_upd*);

void ginq_ws_defer_upd_sts
#ifdef NeedFuncProto
(
    Gint               ws_id,       /* workstation identifier         */
    Gint               *err_ind,    /* OUT error indicator            */
    Gdefer_mode        *defer_mode, /* OUT deferral mode              */
    Girg_mode          *irg_mode,   /* OUT implicit regeneration mode */
    Gdisp_space_empty  *disp_empty, /* OUT display space empty        */
    Gnew_frame_nec_upd *new_frame   /* OUT new frame action necessary 
                                       at update                      */
)
#else
(ws_id,err_ind,defer_mode,irg_mode,disp_empty,new_frame)
    Gint               ws_id;       /* workstation identifier         */
    Gint               *err_ind;    /* OUT error indicator            */
    Gdefer_mode        *defer_mode; /* OUT deferral mode              */
    Girg_mode          *irg_mode;   /* OUT implicit regeneration mode */
    Gdisp_space_empty  *disp_empty; /* OUT display space empty        */
    Gnew_frame_nec_upd *new_frame;  /* OUT new frame action necessary 
                                       at update                      */
#endif
{
    NGCALLF(gqwkdu,GQWKDU)(&ws_id,err_ind,defer_mode,irg_mode,
                           disp_empty,new_frame);
}





