/*
 *	$Id: s_gselnt.c,v 1.1 1997-03-05 19:13:22 haley Exp $
 */
/*
 *  Select normalization transformation 
 */

#include <ncarg/gks.h>

void gsel_norm_tran
#ifdef NeedFuncProto
(
    Gint tran_num  /* transformation number */
)
#else
( tran_num )
    Gint tran_num;
#endif
{
    NGCALLF(gselnt,GSELNT)(&tran_num);
}
