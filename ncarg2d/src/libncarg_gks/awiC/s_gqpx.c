/*
 *  $Id: s_gqpx.c,v 1.3 2000-08-01 14:35:53 haley Exp $
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
 *  Inquire pixel  
 */

#include <ncarg/gks.h>

extern void NGCALLF(gqpx,GQPX)(Gint*,const Gfloat*,const Gfloat*,Gint*,Gint*);

void ginq_pixel
#ifdef NeedFuncProto
(
    Gint   ws_id,             /* workstation identifier */
    const  Gpoint *pixel_loc, /* pixel location         */
    Gint   *err_ind,          /* OUT error indicator    */
    Gint   *colr_ind          /* OUT colour index       */
)
#else
( ws_id, pixel_loc, err_ind, colr_ind )
    Gint   ws_id;
    Gpoint *pixel_loc;
    Gint   *err_ind;
    Gint   *colr_ind;
#endif
{
    NGCALLF(gqpx,GQPX)(&ws_id,&pixel_loc->x,&pixel_loc->y,err_ind,colr_ind);
}

