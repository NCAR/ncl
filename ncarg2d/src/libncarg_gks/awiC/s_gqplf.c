/*
 *  $Id: s_gqplf.c,v 1.2 2000-07-12 17:06:16 haley Exp $
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
 *  Inquire polyline facilities  
 */

#include <ncarg/gks.h>

#define min(x,y)   ((x) < (y) ? (x) : (y))

void ginq_line_facs
#ifdef NeedFuncProto
(
    Gint       ws_type,             /* workstation type                   */
    Gint       num_elems_appl_list, /* length of application list         */
    Gint       start_pos,           /* starting position                  */
    Gint       *err_ind,            /* OUT error indicator                */
    Gline_facs *line_facs,          /* OUT polyline facilities            */
    Gint       *length_list         /* OUT length of linetype list in GKS */
)
#else
(ws_type,num_elems_appl_list,start_pos,err_ind,
                    line_facs,length_list)
    Gint       ws_type;
    Gint       num_elems_appl_list;
    Gint       start_pos;
    Gint       *err_ind;
    Gline_facs *line_facs;
    Gint       *length_list;
#endif
{
    int num, i, j = 0, beg_pos, end_pos;
    Gfloat nomlw, rlwmin, rlwmax;
/*
 * Position "n" in C is position "n+1" in Fortran
 */
    beg_pos = start_pos + 1;
/*
 * Get first element
 */
    NGCALLF(gqplf,GQPLF)(&ws_type,&beg_pos,err_ind,length_list,
                         &line_facs->types.ints[j],
                         &line_facs->num_widths,
                         &nomlw,&rlwmin,&rlwmax,
                         &line_facs->num_pred_inds);
    line_facs->nom_width = (Gdouble)nomlw;
    line_facs->min_width = (Gdouble)rlwmin;
    line_facs->max_width = (Gdouble)rlwmax;
    line_facs->types.num_ints = *length_list;
/*
 * Get rest of elements
 */
    if( ! *err_ind ) {
        line_facs->types.num_ints = 1;
        j++;
        num = min( *length_list - beg_pos + 1, num_elems_appl_list );
        end_pos = beg_pos + num - 1;
        for( i = beg_pos + 1; i <= end_pos; i++ ) {
            NGCALLF(gqplf,GQPLF)(&ws_type,&i,err_ind,length_list,
                                 &line_facs->types.ints[j],
                                 &line_facs->num_widths,
                                 &nomlw,&rlwmin,&rlwmax,
                                 &line_facs->num_pred_inds);
            line_facs->types.num_ints++;
            j++;
        }
    }
}
