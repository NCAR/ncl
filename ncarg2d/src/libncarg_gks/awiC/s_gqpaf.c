/*
 *  $Id: s_gqpaf.c,v 1.1 1997-03-05 19:13:05 haley Exp $
 */
/*
 *  Inquire pattern facilities  
 */

#include <ncarg/gks.h>

void ginq_pat_facs
#ifdef NeedFuncProto
(
    Gint ws_type,       /* workstation type                    */
    Gint *err_ind,      /* OUT error indicator                 */
    Gint *num_pred_inds /* OUT num. of predef. pattern indices */
)
#else
( ws_type, err_ind, num_pred_inds )
    Gint ws_type;
    Gint *err_ind;
    Gint *num_pred_inds;
#endif
{
    NGCALLF(gqpaf,GQPAF)(&ws_type,err_ind,num_pred_inds);
}
