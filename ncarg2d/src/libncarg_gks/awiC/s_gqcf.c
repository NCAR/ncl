/*
 *  $Id: s_gqcf.c,v 1.1 1997-03-05 19:12:53 haley Exp $
 */
/*
 *  Inquire colour facilities  
 */

#include <ncarg/gks.h>

void ginq_colr_facs
#ifdef NeedFuncProto
(
    Gint       ws_type,    /* workstation type      */
    Gint       *err_ind,   /* OUT error indicator   */
    Gcolr_facs *colr_facs  /* OUT colour facilities */
)
#else
( ws_type, err_ind, colr_facs )
    Gint       ws_type;
    Gint       *err_ind;
    Gcolr_facs *colr_facs;
#endif
{
    NGCALLF(gqcf,GQCF)(&ws_type,err_ind,&colr_facs->num_colrs,
                       &colr_facs->colr_avail,&colr_facs->num_pred_inds);
}
