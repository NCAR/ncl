/*
 *	$Id: s_gqtxci.c,v 1.1 1997-03-05 19:13:12 haley Exp $
 */
/* 
 *  Inquire text colour index  
 */

#include <ncarg/gks.h>

void ginq_text_colr_ind
#ifdef NeedFuncProto
(
    Gint *err_ind,        /* OUT error indicator     */
    Gint *text_colr_ind   /* OUT text colour index   */
)
#else
( err_ind, text_colr_ind )
    Gint *err_ind;
    Gint *text_colr_ind;
#endif
{
    NGCALLF(gqtxci,GQTXCI)(err_ind,text_colr_ind);
}
