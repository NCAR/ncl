/*
 *	$Id: s_gstxfp.c,v 1.1 1997-03-05 19:13:30 haley Exp $
 */
/*
 *  Set text font and precision  
 */

#include <ncarg/gks.h>

void gset_text_font_prec
#ifdef NeedFuncProto
(
    const Gtext_font_prec *text_font_prec  /* text font and precision */
)
#else
( text_font_prec )
    Gtext_font_prec *text_font_prec;
#endif
{
    NGCALLF(gstxfp,GSTXFP)(&text_font_prec->font,&text_font_prec->prec);
}
