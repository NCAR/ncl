/*
 *  $Id: s_gqpxad.c,v 1.4 2000-08-22 15:09:09 haley Exp $
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
 *  Inquire pixel array dimensions  
 */

#include <ncarg/gks.h>

extern void NGCALLF(gqpxad,GQPXAD)(Gint*,Gfloat*,Gfloat*,Gfloat*,Gfloat*,
                                   Gint*,Gint*,Gint*);

void ginq_pixel_array_dims
#ifdef NeedFuncProto
(
    Gint      ws_id,      /* workstation identifier     */
    Grect     *rect,      /* rectangle                  */
    Gint      *err_ind,   /* OUT error indicator        */
    Gint_size *dims       /* OUT pixel array dimensions */
)
#else
(ws_id,rect,err_ind,dims)
    Gint      ws_id;
    Grect     *rect;
    Gint      *err_ind;
    Gint_size *dims;
#endif
{
    NGCALLF(gqpxad,GQPXAD)(&ws_id,&rect->p.x,&rect->p.y,&rect->q.x,&rect->q.y,
                           err_ind,&dims->size_x,&dims->size_y);
}
