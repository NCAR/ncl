/*
 *	$Id: s_gqlwsc.c,v 1.1 1997-03-05 19:13:02 haley Exp $
 */
/*
 *  Inquire linewidth scale factor  
 */

#include <ncarg/gks.h>

void ginq_linewidth
#ifdef NeedFuncProto
(
    Gint    *err_ind,    /* OUT error indicator                 */
    Gdouble *linewidth   /* OUT current linewidth scale factor  */
)
#else
( err_ind, linewidth )
    Gint    *err_ind;
    Gdouble *linewidth;
#endif
{
    Gfloat width;
    NGCALLF(gqlwsc,GQLWSC)(err_ind,&width);
    *linewidth = (Gdouble)width;
}
