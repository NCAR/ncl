/*
 *	$Id: s_gqpawv.c,v 1.1 1997-03-05 19:13:06 haley Exp $
 */
/*
 *  Inquire pattern width vector  
 */

#include <ncarg/gks.h>

void ginq_pat_width_vec
#ifdef NeedFuncProto
(
    Gint *err_ind,        /* OUT error indicator              */
    Gvec *pat_width_vec   /* OUT current pattern width vector */
)
#else
( err_ind, pat_width_vec )
    Gint *err_ind;
    Gvec *pat_width_vec;
#endif
{
    Gfloat dumx, dumy;
    NGCALLF(gqpa,GQPA)(err_ind,&pat_width_vec->delta_x,&pat_width_vec->delta_y,
                       &dumx,&dumy);
}
