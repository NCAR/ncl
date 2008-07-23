/*
 *	$Id: s_gswn.c,v 1.5 2008-07-23 17:24:25 haley Exp $
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
 *  Set window  
 */

#include <ncarg/gks.h>

extern void NGCALLF(gswn,GSWN)(Gint*,const Gfloat*,const Gfloat*,
                                     const Gfloat*,const Gfloat*);

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
