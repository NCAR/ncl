/*
 *	$Id: s_gqeci.c,v 1.4 2000-08-22 15:08:54 haley Exp $
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
 *  Inquire list of colour indices
 */

#include <ncarg/gks.h>

extern void NGCALLF(gqeci,GQECI)(Gint*,int*,Gint*,Gint*,Gint*);

#define min(x,y)   ((x) < (y) ? (x) : (y))

void ginq_list_colr_inds
#ifdef NeedFuncProto
(
    Gint      ws_id,              /* workstation identifier             */
    Gint      num_elems_appl_list,/* length of application list         */
    Gint      start_pos,          /* starting position                  */
    Gint      *err_ind,           /* OUT error indicator                */
    Gint_list *def_colr_inds,     /* OUT list of defined colour indices */
    Gint      *length_list        /* OUT length of list in GKS          */
)
#else
(ws_id,num_elems_appl_list,start_pos,err_ind,def_colr_inds,length_list)
    Gint      ws_id;
    Gint      num_elems_appl_list;
    Gint      start_pos;
    Gint      *err_ind;
    Gint_list *def_colr_inds;
    Gint      *length_list;
#endif
{
    int num, i, j = 0, beg_pos, end_pos;
/*
 * Position "n" in C is position "n+1" in Fortran
 */
    beg_pos = start_pos + 1;
/*
 * Get first element
 */
    NGCALLF(gqeci,GQECI)(&ws_id,&beg_pos,err_ind,length_list,
                         &def_colr_inds->ints[j]);
    def_colr_inds->num_ints = *length_list;
/*
 * Get rest of elements
 */
    if( ! *err_ind ) {
        def_colr_inds->num_ints = 1;
        j++;
        num = min( *length_list - beg_pos + 1, num_elems_appl_list );
        end_pos = beg_pos + num - 1;
        for( i = beg_pos + 1; i <= end_pos; i++ ) {
            NGCALLF(gqeci,GQECI)(&ws_id,&i,err_ind,length_list,
                                 &def_colr_inds->ints[j]);
            def_colr_inds->num_ints++;
            j++;
        }
    }
}
