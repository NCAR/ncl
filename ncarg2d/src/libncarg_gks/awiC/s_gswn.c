/*
 *	$Id: s_gswn.c,v 1.1 1997-03-05 19:13:33 haley Exp $
 */
/*
 *  Set window  
 */

#include <ncarg/gks.h>

void gset_win
#ifdef NeedFuncProto
(
    Gint            tran_num,      /* transformation number  */
    const Glimit    *win_limits    /* window limits          */
)
#else
( tran_num, win_limits )
    Gint      tran_num;
    Glimit    *win_limits;
#endif
{
    NGCALLF(gswn,GSWN)(&tran_num,&win_limits->x_min,&win_limits->x_max,
                                 &win_limits->y_min,&win_limits->y_max);
}
