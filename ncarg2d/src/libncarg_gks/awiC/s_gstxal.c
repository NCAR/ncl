/*
 *	$Id: s_gstxal.c,v 1.1 1997-03-05 19:13:29 haley Exp $
 */
/*
 *  Set text alignment  
 */

#include <ncarg/gks.h>

void gset_text_align
#ifdef NeedFuncProto
(
    const Gtext_align *text_align  /* text alignment */
)
#else
( text_align )
    Gtext_align *text_align;
#endif
{
    NGCALLF(gstxal,GSTXAL)(&text_align->hor,&text_align->vert);
}
