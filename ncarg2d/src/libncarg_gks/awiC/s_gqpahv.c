/*
 *	$Id: s_gqpahv.c,v 1.1 1997-03-05 19:13:05 haley Exp $
 */
/*
 *  Inquire pattern height vector  
 */

#include <ncarg/gks.h>

void ginq_pat_ht_vec
#ifdef NeedFuncProto
(
    Gint *err_ind,     /* OUT error indicator               */
    Gvec *pat_ht_vec   /* OUT current pattern height vector */
)
#else
( err_ind, pat_ht_vec )
    Gint *err_ind;
    Gvec *pat_ht_vec;
#endif
{
    Gfloat dumx, dumy;
    NGCALLF(gqpa,GQPA)(err_ind,&dumx,&dumy,
                       &pat_ht_vec->delta_x,&pat_ht_vec->delta_y);
}
