/*
 *	$Id: s_gqfais.c,v 1.1 1997-03-05 19:12:59 haley Exp $
 */
/*
 *  Inquire fill area interior style 
 */

#include <ncarg/gks.h>

void ginq_fill_int_style
#ifdef NeedFuncProto
(
    Gint            *err_ind,         /* OUT current error indicator       */
    Gfill_int_style *fill_int_style   /* OUT current fill area style index */
)
#else
( err_ind, fill_int_style )
    Gint            *err_ind;
    Gfill_int_style *fill_int_style;
#endif
{
    NGCALLF(gqfais,GQFAIS)(err_ind,fill_int_style);
}
