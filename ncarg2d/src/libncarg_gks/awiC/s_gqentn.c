/*
 *  $Id: s_gqentn.c,v 1.3 2000-08-01 14:35:48 haley Exp $
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
 *  Inquire list of normalization transformation numbers  
 */

#include <ncarg/gks.h>

extern void NGCALLF(gqentn,GQENTN)(int*,Gint*,Gint*,Gint*);

#define min(x,y)   ((x) < (y) ? (x) : (y))

void ginq_list_norm_tran_nums
#ifdef NeedFuncProto
(
    Gint num_elems_appl_list,  /* length of application list                */
    Gint start_pos,            /* starting position                         */
    Gint *err_ind,             /* OUT error indicator                       */
    Gint_list *norm_tran_num,  /* OUT list of normalization transformation 
                                  numbers                                   */
    Gint *length_list          /* OUT length of list in GKS                 */
)
#else
(num_elems_appl_list,start_pos,err_ind,
                              norm_tran_num,length_list)
    Gint num_elems_appl_list;
    Gint start_pos;
    Gint *err_ind;
    Gint_list *norm_tran_num;
    Gint *length_list;
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
    NGCALLF(gqentn,GQENTN)(&beg_pos,err_ind,length_list,
                           &norm_tran_num->ints[j]);
    norm_tran_num->num_ints = *length_list;
/*
 * Get rest of elements
 */
    if( ! *err_ind ) {
        norm_tran_num->num_ints = 1;
        j++;
        num = min( *length_list - beg_pos + 1, num_elems_appl_list );
        end_pos = beg_pos + num - 1;
        for( i = beg_pos + 1; i <= end_pos; i++ ) {
            NGCALLF(gqentn,GQENTN)(&i,err_ind,length_list,
                                   &norm_tran_num->ints[j]);
            norm_tran_num->num_ints++;
            j++;
        }
    }
}
