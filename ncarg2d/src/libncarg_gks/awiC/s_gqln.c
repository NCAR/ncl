/*
 *	$Id: s_gqln.c,v 1.1 1997-03-05 19:13:01 haley Exp $
 */
/* 
 * Inquire linetype  
 */

#include <ncarg/gks.h>

void ginq_linetype
#ifdef NeedFuncProto
(
    Gint *err_ind,   /* OUT error indicator  */
    Gint *linetype   /* OUT current linetype */
)
#else
( err_ind, linetype )
    Gint *err_ind;
    Gint *linetype;
#endif
{
    NGCALLF(gqln,GQLN)(err_ind,linetype);
}
