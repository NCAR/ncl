/*
 *	$Id: s_gqtxi.c,v 1.1 1997-03-05 19:13:13 haley Exp $
 */
/*
 *  Inquire text index  
 */

#include <ncarg/gks.h>

void ginq_text_ind
#ifdef NeedFuncProto
(
    Gint *err_ind,  /*  OUT error indicator     */
    Gint *text_ind  /*  OUT current text index  */
)
#else
( err_ind, text_ind )
    Gint *err_ind;
    Gint *text_ind;
#endif
{
    NGCALLF(gqtxi,GQTXI)(err_ind,text_ind);
}
