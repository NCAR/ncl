/*
 *	$Id: s_gqmntn.c,v 1.1 1997-03-05 19:13:03 haley Exp $
 */
/* 
 *  Inquire maximum normalization transformation number 
 */

#include <ncarg/gks.h>

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
