/*
 *	$Id: s_gqfai.c,v 1.1 1997-03-05 19:12:59 haley Exp $
 */
/*
 *  Inquire fill area index  
 */

#include <ncarg/gks.h>

void ginq_fill_ind
#ifdef NeedFuncProto
(
    Gint *err_ind,  /*  OUT error indicator          */
    Gint *fill_ind  /*  OUT current fill area index  */
)
#else
( err_ind, fill_ind )
    Gint *err_ind;
    Gint *fill_ind;
#endif
{
    NGCALLF(gqfai,GQFAI)(err_ind,fill_ind);
}
