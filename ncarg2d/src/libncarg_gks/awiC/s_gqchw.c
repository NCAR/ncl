/*
 *	$Id: s_gqchw.c,v 1.1 1997-03-05 19:12:55 haley Exp $
 */
/*
 *  Inquire character width
 */

#include <ncarg/gks.h>

void ginq_char_width
#ifdef NeedFuncProto
(
    Gint    *err_ind,    /* OUT error indicator          */
    Gdouble *char_width  /* OUT current character width  */
)
#else
( err_ind, char_width )
    Gint    *err_ind;
    Gdouble *char_width;
#endif
{
    Gfloat chw;
    NGCALLF(gqchw,GQCHW)(err_ind, &chw);
    *char_width = (Gdouble) chw;
}
