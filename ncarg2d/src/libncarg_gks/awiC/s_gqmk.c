/*
 *	$Id: s_gqmk.c,v 1.1 1997-03-05 19:13:02 haley Exp $
 */
/*
 *  Inquire marker type 
 */

#include <ncarg/gks.h>

void ginq_marker_type
#ifdef NeedFuncProto
(
    Gint *err_ind,      /* OUT error indicator      */
    Gint *marker_type   /* OUT current marker type  */
)
#else
( err_ind, marker_type )
    Gint *err_ind;
    Gint *marker_type;
#endif
{
    NGCALLF(gqmk,GQMK)(err_ind,marker_type);
}
