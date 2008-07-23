/*
 *	$Id: s_gstxfp.c,v 1.5 2008-07-23 17:24:24 haley Exp $
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
 *  Set text font and precision  
 */

#include <ncarg/gks.h>

extern void NGCALLF(gstxfp,GSTXFP)(const Gint*,const Gtext_prec*);

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
