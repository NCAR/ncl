/*
 *	$Id: s_gqcntn.c,v 1.5 2008-07-23 17:24:20 haley Exp $
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
 *  Inquire current normalization transformation number  
 */

#include <ncarg/gks.h>

extern void NGCALLF(gqcntn,GQCNTN)(Gint*,Gint*);

void ginq_cur_norm_tran_num
#ifdef NeedFuncProto
(
    Gint *err_ind,        /* OUT error indicator        */
    Gint *norm_tran_num   /* OUT current normalization
                             transformation number      */
)
#else
( err_ind, norm_tran_num )
    Gint *err_ind;
    Gint *norm_tran_num;
#endif
{
    NGCALLF(gqcntn,GQCNTN)(err_ind,norm_tran_num);
}
