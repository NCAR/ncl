/*
 *	$Id: s_gqtxp.c,v 1.1 1997-03-05 19:13:14 haley Exp $
 */
/*
 * Inquire text path  
 */

#include <ncarg/gks.h>

void ginq_text_path
#ifdef NeedFuncProto
(
    Gint       *err_ind,    /* OUT error indicator   */
    Gtext_path *text_path   /* OUT current text path */
)
#else
( err_ind, text_path )
    Gint       *err_ind;
    Gtext_path *text_path;
#endif
{
    NGCALLF(gqtxp,GQTXP)(err_ind,text_path);
}
