/*
 *	$Id: s_gspa.c,v 1.1 1997-03-05 19:13:26 haley Exp $
 */
/*
 *  Set pattern size  
 */

#include <ncarg/gks.h>

void gset_pat_size
#ifdef NeedFuncProto
(
    Gdouble x_size, /* x size */
    Gdouble y_size  /* y size */
)
#else
( x_size, y_size )
    Gdouble x_size;
    Gdouble y_size;
#endif
{
    Gfloat x, y;
    x = (Gfloat) x_size;
    y = (Gfloat) y_size;
    NGCALLF(gspa,GSPA)(&x,&y);
}

