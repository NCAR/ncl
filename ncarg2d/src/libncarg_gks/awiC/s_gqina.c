/*
 *	$Id: s_gqina.c,v 1.4 2008-07-23 17:24:21 haley Exp $
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
