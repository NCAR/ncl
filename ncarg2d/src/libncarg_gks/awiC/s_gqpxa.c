/*
 *  $Id: s_gqpxa.c,v 1.4 2000-08-22 15:09:08 haley Exp $
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
 *  Inquire pixel array  
 */

#include <ncarg/gks.h>

extern void NGCALLF(gqpxa,GQPXA)(Gint*,Gfloat*,Gfloat*,Gint*,
                                 Gint*,Gint*,Gint*,int*,int*,Gint*,
                                 Gpres_inval*,Gint*);

#define min(x,y)   ((x) < (y) ? (x) : (y))

void ginq_pixel_array
#ifdef NeedFuncProto
(
    Gint        ws_id,               /* workstation identifier         */
    Gpoint      *pixel_loc,          /* pixel location                 */
    Gint_size   *dims,               /* pixel array dimensions         */
    Gint        num_elems_appl_list, /* length of application list     */
    Gint        start_pos,           /* starting position              */
    Gint        *err_ind,            /* OUT error indicator            */
    Gpres_inval *pres_inval,         /* OUT presence of invalid values */
    Gint_list   *pixel_array,        /* OUT colour index array         */
    Gint        *length_list         /* OUT length of list in GKS      */
)
#else
(ws_id,pixel_loc,dims,num_elems_appl_list,start_pos,
 err_ind,pres_inval,pixel_array,length_list)
    Gint        ws_id;
    Gpoint      *pixel_loc;
    Gint_size   *dims;
    Gint        num_elems_appl_list;
    Gint        start_pos;
    Gint        *err_ind;
    Gpres_inval *pres_inval;
    Gint_list   *pixel_array;
    Gint        *length_list;
#endif
{
    int idx, idy, istrt, jstrt;
/*
 * It is not possible to interact with the Fortran routine GQPXA,
 * because there is no way to tell how many points in the x and y
 * direction the user wants with "start_pos" and "num_elems_appl_list".
 * So, return the whole array and let the user get what he/she needs.
 */
/*
 * Start with the first element of the array
 */
    istrt = jstrt = 1;
/*
 * And end with the last element of the array
 */
    idx = dims->size_x;
    idy = dims->size_y;

    NGCALLF(gqpxa,GQPXA)(&ws_id,&pixel_loc->x,&pixel_loc->y,&dims->size_x,
                         &dims->size_y,&istrt,&jstrt,&idx,&idy,err_ind,
                         pres_inval,pixel_array->ints);

    pixel_array->num_ints = *length_list = idx * idy;
}

