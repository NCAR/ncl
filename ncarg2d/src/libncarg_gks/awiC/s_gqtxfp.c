/*
 *	$Id: s_gqtxfp.c,v 1.1 1997-03-05 19:13:13 haley Exp $
 */
/*
 * Inquire text font and precision  
 */

#include <ncarg/gks.h>

void ginq_text_font_prec
#ifdef NeedFuncProto
(
    Gint            *err_ind,         /* OUT error indicator                 */
    Gtext_font_prec *text_font_prec   /* OUT current text font and precision */
)
#else
( err_ind, text_font_prec )
    Gint            *err_ind;
    Gtext_font_prec *text_font_prec;
#endif
{
    NGCALLF(gqtxfp,GQTXFP)(err_ind,&text_font_prec->font,
                                   &text_font_prec->prec);
}
