/*
 *	$Id: s_gqfaci.c,v 1.1 1997-03-05 19:12:58 haley Exp $
 */
/*
 *  Inquire fill area colour index  
 */

#include <ncarg/gks.h>

void ginq_fill_colr_ind
#ifdef NeedFuncProto
(
    Gint *err_ind,        /* OUT error indicator                 */
    Gint *fill_colr_ind   /* OUT current fill area colour index  */
)
#else
( err_ind, fill_colr_ind )
    Gint *err_ind;
    Gint *fill_colr_ind;
#endif
{
    NGCALLF(gqfaci,GQFACI)(err_ind,fill_colr_ind);
}
