/*
 *	$Id: s_gslwsc.c,v 1.1 1997-03-05 19:13:25 haley Exp $
 */
/*
 *  Set linewidth scale factor  
 */

#include <ncarg/gks.h>

void gset_linewidth
#ifdef NeedFuncProto
(
    Gdouble linewidth  /* linewidth scale factor  */
)
#else
( linewidth )
    Gdouble linewidth;
#endif
{
    Gfloat width;
    width = (Gfloat)linewidth;
    NGCALLF(gslwsc,GSLWSC)(&width);
}
