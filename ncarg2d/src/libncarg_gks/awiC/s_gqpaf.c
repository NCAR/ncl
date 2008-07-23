/*
 *  $Id: s_gqpaf.c,v 1.5 2008-07-23 17:24:22 haley Exp $
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
 *  Inquire pattern facilities  
 */

#include <ncarg/gks.h>

extern void NGCALLF(gqpaf,GQPAF)(Gint*,Gint*,Gint*);

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
