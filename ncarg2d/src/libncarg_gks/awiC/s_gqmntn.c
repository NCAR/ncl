/*
 *	$Id: s_gqmntn.c,v 1.5 2008-07-23 17:24:21 haley Exp $
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
 *  Inquire maximum normalization transformation number 
 */

#include <ncarg/gks.h>

extern void NGCALLF(gqmntn,GQMNTN)(Gint*,Gint*);

void ginq_max_norm_tran_num
#ifdef NeedFuncProto
(
    Gint *err_ind,           /* OUT error indicator                       */
    Gint *max_norm_tran_num  /* OUT maximum normalization transformation
                             number                                       */
)
#else
( err_ind, max_norm_tran_num )
    Gint *err_ind;
    Gint *max_norm_tran_num;
#endif
{
    NGCALLF(gqmntn,GQMNTN)(err_ind,max_norm_tran_num);
}
