/*
 *	$Id: s_gqtxal.c,v 1.1 1997-03-05 19:13:12 haley Exp $
 */
/*
 *  Inquire text alignment  
 */

#include <ncarg/gks.h>

void ginq_text_align
#ifdef NeedFuncProto
(
    Gint        *err_ind,     /* OUT error indicator          */
    Gtext_align *text_align   /* OUT current text alignment   */
)
#else
( err_ind, text_align )
    Gint        *err_ind;
    Gtext_align *text_align;
#endif
{
    NGCALLF(gqtxal,GQTXAL)(err_ind,&text_align->hor,&text_align->vert);
}
