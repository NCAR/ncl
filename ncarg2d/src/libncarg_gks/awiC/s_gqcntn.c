/*
 *	$Id: s_gqcntn.c,v 1.1 1997-03-05 19:12:56 haley Exp $
 */
/*
 *  Inquire current normalization transformation number  
 */

#include <ncarg/gks.h>

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
