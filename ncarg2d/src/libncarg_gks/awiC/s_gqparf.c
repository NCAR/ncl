/*
 *  $Id: s_gqparf.c,v 1.1 1997-03-05 19:13:06 haley Exp $
 */
/*
 *  Inquire pattern reference point  
 */

#include <ncarg/gks.h>

void ginq_pat_ref_point
#ifdef NeedFuncProto
(
    Gint   *err_ind,      /* OUT error indicator                 */
    Gpoint *pat_ref_point /* OUT current pattern reference point */
)
#else
( err_ind, pat_ref_point )
    Gint   *err_ind;
    Gpoint *pat_ref_point;
#endif
{
    NGCALLF(gqparf,GQPARF)(err_ind,&pat_ref_point->x,&pat_ref_point->y);
}

