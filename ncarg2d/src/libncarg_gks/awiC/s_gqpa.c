/*
 *	$Id: s_gqpa.c,v 1.1 1997-03-05 19:13:04 haley Exp $
 */
/*
 *  Get pattern size  
 */

#include <ncarg/gks.h>

void ginq_pat_size
#ifdef NeedFuncProto
(
    Gint    *err_ind, /* OUT error indicator */
    Gdouble *x_size,  /* OUT x size          */
    Gdouble *y_size   /* OUT y size          */
)
#else
( err_ind, x_size, y_size )
    Gint    *err_ind;
    Gdouble *x_size;
    Gdouble *y_size;
#endif
{
    Gfloat x, y;
    NGCALLF(gqpa,GQPA)(err_ind,&x,&y);
    *x_size = (Gdouble) x;
    *y_size = (Gdouble) y;
}

