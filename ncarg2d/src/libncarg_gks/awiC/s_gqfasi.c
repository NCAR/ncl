/*
 *	$Id: s_gqfasi.c,v 1.1 1997-03-05 19:13:00 haley Exp $
 */
/*
 *  Inquire fill area style index  
 */

#include <ncarg/gks.h>

void ginq_fill_style_ind
#ifdef NeedFuncProto
(
    Gint *err_ind,         /* OUT current error indicator       */
    Gint *fill_style_ind   /* OUT current fill area style index */
)
#else
( err_ind, fill_style_ind )
    Gint *err_ind;
    Gint *fill_style_ind;
#endif
{
    NGCALLF(gqfasi,GQFASI)(err_ind,fill_style_ind);
}
