/*
 *	$Id: s_gqpli.c,v 1.1 1997-03-05 19:13:08 haley Exp $
 */
/*
 *  Inquire polyline index  
 */

#include <ncarg/gks.h>

void ginq_line_ind
#ifdef NeedFuncProto
(
    Gint *err_ind, /*  OUT error indicator         */
    Gint *line_ind /*  OUT current polyline index  */
)
#else
( err_ind, line_ind )
    Gint *err_ind;
    Gint *line_ind;
#endif
{
    NGCALLF(gqpli,GQPLI)(err_ind,line_ind);
}
