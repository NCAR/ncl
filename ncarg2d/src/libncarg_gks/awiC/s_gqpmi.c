/*
 *	$Id: s_gqpmi.c,v 1.1 1997-03-05 19:13:09 haley Exp $
 */
/*
 *  Inquire polymarker index  
 */

#include <ncarg/gks.h>

void ginq_marker_ind
#ifdef NeedFuncProto
(
    Gint *err_ind,    /*  OUT error indicator           */
    Gint *marker_ind  /*  OUT current polymarker index  */
)
#else
( err_ind, marker_ind )
    Gint *err_ind;    /*  OUT error indicator           */
    Gint *marker_ind; /*  OUT current polymarker index  */
#endif
{
    NGCALLF(gqpmi,GQPMI)(err_ind,marker_ind);
}
