/*
 *	$Id: s_gqmksc.c,v 1.1 1997-03-05 19:13:02 haley Exp $
 */
/* 
 *  Inquire marker size scale factor  
 */

#include <ncarg/gks.h>

void ginq_marker_size
#ifdef NeedFuncProto
(
    Gint    *err_ind,      /* OUT error indicator                  */
    Gdouble *marker_size   /* OUT current marker size scale factor */
)
#else
( err_ind, marker_size )
    Gint    *err_ind;
    Gdouble *marker_size;
#endif
{
    Gfloat size;
    NGCALLF(gqmksc,GQMKSC)(err_ind,&size);
    *marker_size = (Gdouble)size;
}
