/*
 *	$Id: s_gqina.c,v 1.2 2000-07-12 17:06:13 haley Exp $
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
 *  Inquire current individual attribute values
 */

#include <ncarg/gks.h>

void ginq_cur_indiv_attrs
#ifdef NeedFuncProto
(
    Gint *err_ind,           /* OUT error indicator                     */
    Gindiv_attrs *indiv_attr /*  OUT current indiv. attribute structure */
)

#else
( err_ind, indiv_attr )
    Gint *err_ind;
    Gindiv_attrs *indiv_attr;
#endif
{
    ginq_linetype        ( err_ind, &indiv_attr->linetype        );
    ginq_linewidth       ( err_ind, &indiv_attr->linewidth       );
    ginq_line_colr_ind   ( err_ind, &indiv_attr->line_colr_ind   );
    ginq_marker_type     ( err_ind, &indiv_attr->marker_type     );
    ginq_marker_size     ( err_ind, &indiv_attr->marker_size     );
    ginq_marker_colr_ind ( err_ind, &indiv_attr->marker_colr_ind );
    ginq_text_font_prec  ( err_ind, &indiv_attr->text_font_prec  );
    ginq_char_expan      ( err_ind, &indiv_attr->char_expan      );
    ginq_char_space      ( err_ind, &indiv_attr->char_space      );
    ginq_text_colr_ind   ( err_ind, &indiv_attr->text_colr_ind   );
    ginq_fill_int_style  ( err_ind, &indiv_attr->fill_int_style  );
    ginq_fill_style_ind  ( err_ind, &indiv_attr->fill_style_ind  );
    ginq_fill_colr_ind   ( err_ind, &indiv_attr->fill_colr_ind   );
    ginq_asfs            ( err_ind, &indiv_attr->asfs            );
}
