/*
 *	$Id: s_gqplci.c,v 1.1 1997-03-05 19:13:07 haley Exp $
 */
/* 
 * Inquire polyline colour index  
 */

#include <ncarg/gks.h>

void ginq_line_colr_ind
#ifdef NeedFuncProto
(
    Gint *err_ind,       /* OUT error indicator               */
    Gint *line_colr_ind  /* OUT current polyline colour index */
)
#else
( err_ind, line_colr_ind )
    Gint *err_ind;
    Gint *line_colr_ind;
#endif
{
    NGCALLF(gqplci,GQPLCI)(err_ind,line_colr_ind);
}
