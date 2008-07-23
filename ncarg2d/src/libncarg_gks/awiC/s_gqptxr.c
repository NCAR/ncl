/*
 *  $Id: s_gqptxr.c,v 1.5 2008-07-23 17:24:22 haley Exp $
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
 *  Inquire predefined text representation  
 */

#include <ncarg/gks.h>

extern void NGCALLF(gqptxr,GQPTXR)(Gint*,Gint*,Gint*,Gint*,Gtext_prec*,
                                   Gfloat*,Gfloat*,Gint*);

void ginq_pred_text_rep
#ifdef NeedFuncProto
(
    Gint         ws_type,   /* workstation type         */
    Gint         ind,       /* predefined index         */
    Gint         *err_ind,  /* OUT error indicator      */
    Gtext_bundle *text_rep  /* OUT predefined text rep. */
)
#else
( ws_type, ind, err_ind, text_rep )
    Gint         ws_type;
    Gint         ind;
    Gint         *err_ind;
    Gtext_bundle *text_rep;
#endif
{
    Gfloat charxp, charsp;

    NGCALLF(gqptxr,GQPTXR)(&ws_type,&ind,err_ind,
                           &text_rep->text_font_prec.font,
                           &text_rep->text_font_prec.prec,
                           &charxp,&charsp,&text_rep->colr_ind);

    text_rep->char_expan = (Gdouble)charxp;
    text_rep->char_space = (Gdouble)charsp;
}
