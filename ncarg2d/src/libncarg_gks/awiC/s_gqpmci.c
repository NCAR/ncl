/*
 *	$Id: s_gqpmci.c,v 1.1 1997-03-05 19:13:08 haley Exp $
 */
/*
 *  Inquire polymarker colour index  
 */

#include <ncarg/gks.h>

void ginq_marker_colr_ind
#ifdef NeedFuncProto
(
    Gint *err_ind,          /* OUT error indicator                 */
    Gint *marker_colr_ind   /* OUT current polymarker colour index */
)
#else
( err_ind, marker_colr_ind )
    Gint *err_ind;
    Gint *marker_colr_ind;
#endif
{
    NGCALLF(gqpmci,GQPMCI)(err_ind,marker_colr_ind);
}
