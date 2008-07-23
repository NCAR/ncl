/*
 *  $Id: s_gqcf.c,v 1.5 2008-07-23 17:24:20 haley Exp $
 */
/************************************************************************
*                                                                       *
*                Copyright (C)  2000                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
*    The use of this Software is governed by a License Agreement.       *
*                                                                       *
************************************************************************/

/*
 *  Inquire colour facilities  
 */

#include <ncarg/gks.h>

extern void NGCALLF(gqcf,GQCF)(Gint*,Gint*,Gint*,Gcolr_avail*,Gint*);

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
