/*
 *	$Id: s_gqtxfp.c,v 1.5 2008-07-23 17:24:23 haley Exp $
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
 * Inquire text font and precision  
 */

#include <ncarg/gks.h>

extern void NGCALLF(gqtxfp,GQTXFP)(Gint*,Gint*,Gtext_prec*);

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
