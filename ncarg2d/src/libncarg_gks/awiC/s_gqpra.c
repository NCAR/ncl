/*
 *  $Id: s_gqpra.c,v 1.2 2000-07-12 17:06:17 haley Exp $
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
 *  Inquire current primitive attribute values
 */

#include <ncarg/gks.h>

void ginq_cur_prim_attrs
#ifdef NeedFuncProto
(
    Gint        *err_ind,  /* OUT error indicator                       */
    Gprim_attrs *prim_attr /* OUT current primitive attribute structure */
)
#else
( err_ind, prim_attr )
    Gint        *err_ind;
    Gprim_attrs *prim_attr;
#endif
{
    ginq_line_ind     ( err_ind, &prim_attr->line_ind      );
    ginq_marker_ind   ( err_ind, &prim_attr->marker_ind    );
    ginq_text_ind     ( err_ind, &prim_attr->text_ind      );
    ginq_char_ht      ( err_ind, &prim_attr->char_ht       );
    ginq_char_up_vec  ( err_ind, &prim_attr->char_up_vec   );
    ginq_char_width   ( err_ind, &prim_attr->char_width    );
    ginq_char_base_vec( err_ind, &prim_attr->char_base_vec );
    ginq_text_path    ( err_ind, &prim_attr->text_path     );
    ginq_text_align   ( err_ind, &prim_attr->text_align    );
    ginq_fill_ind     ( err_ind, &prim_attr->fill_ind      );
    ginq_pat_width_vec( err_ind, &prim_attr->pat_width_vec );
    ginq_pat_ht_vec   ( err_ind, &prim_attr->pat_ht_vec    );
    ginq_pat_ref_point( err_ind, &prim_attr->pat_ref_point );
}
