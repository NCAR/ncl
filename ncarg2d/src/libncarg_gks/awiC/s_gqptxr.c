/*
 *  $Id: s_gqptxr.c,v 1.1 1997-03-05 19:13:10 haley Exp $
 */
/*
 *  Inquire predefined text representation  
 */

#include <ncarg/gks.h>

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
