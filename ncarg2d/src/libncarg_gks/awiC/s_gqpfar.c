/*
 *	$Id: s_gqpfar.c,v 1.4 2000-08-22 15:09:04 haley Exp $
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
 *  Inquire predefined fill area representation  
 */

#include <ncarg/gks.h>

extern void NGCALLF(gqpfar,GQPFAR)(Gint*,Gint*,Gint*,Gfill_int_style*,
                                   Gint*,Gint*);

void ginq_pred_fill_rep
#ifdef NeedFuncProto
(
    Gint         ws_type,   /* workstation type              */
    Gint         ind,       /* predefined index              */
    Gint         *err_ind,  /* OUT error indicator           */
    Gfill_bundle *fill_rep  /* OUT predefined fill area rep. */
)
#else
( ws_type, ind, err_ind, fill_rep )
    Gint ws_type;
    Gint ind;
    Gint *err_ind;
    Gfill_bundle *fill_rep;
#endif
{
    NGCALLF(gqpfar,GQPFAR)(&ws_type,&ind,err_ind,&fill_rep->int_style,
                           &fill_rep->style_ind,&fill_rep->colr_ind);
}
