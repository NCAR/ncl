/*
 *	$Id: s_gqasf.c,v 1.1 1997-03-05 19:12:53 haley Exp $
 */
/*
 * Inquire aspect source flags 
 */

#include <ncarg/gks.h>

void ginq_asfs
#ifdef NeedFuncProto
(
    Gint  *err_ind,   /* OUT error indicator             */
    Gasfs *list_asf   /* OUT current aspect source flags */
)
#else
( err_ind, list_asf )
    Gint  *err_ind;
    Gasfs *list_asf;
#endif
{
    NGCALLF(gqasf,GQASF)(err_ind,list_asf);
}
