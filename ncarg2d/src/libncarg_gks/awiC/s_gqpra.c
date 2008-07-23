/*
 *  $Id: s_gqpra.c,v 1.4 2008-07-23 17:24:22 haley Exp $
 */
/************************************************************************
*                                                                       *
*                Copyright (C)  2000                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
*    The use of this Software is governed by a License Agreement.       *
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
