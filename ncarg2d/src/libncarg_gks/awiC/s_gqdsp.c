/*
 *  $Id: s_gqdsp.c,v 1.4 2000-08-22 15:08:53 haley Exp $
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
 *  Inquire display space size  
 */

#include <ncarg/gks.h>

extern void NGCALLF(gqdsp,GQDSP)(Gint*,Gint*,Gdc_units*,Gfloat*,Gfloat*,
                                 Gint*,Gint*);

void ginq_disp_space_size
#ifdef NeedFuncProto
(
    Gint             ws_type,    /* workstation type         */
    Gint             *err_ind,   /* OUT error indicator      */
    Gdisp_space_size *disp_size  /* OUT display [space] size */
)
#else
( ws_type, err_ind, disp_size )
    Gint             ws_type;
    Gint             *err_ind;
    Gdisp_space_size *disp_size;
#endif
{
    NGCALLF(gqdsp,GQDSP)(&ws_type,err_ind,&disp_size->dc_units,
                         &disp_size->size_dc.size_x,
                         &disp_size->size_dc.size_y,
                         &disp_size->size_raster.size_x,
                         &disp_size->size_raster.size_y);
}

