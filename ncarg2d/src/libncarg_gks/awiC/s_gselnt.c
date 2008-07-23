/*
 *	$Id: s_gselnt.c,v 1.5 2008-07-23 17:24:24 haley Exp $
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
 *  Select normalization transformation 
 */

#include <ncarg/gks.h>

extern void NGCALLF(gselnt,GSELNT)(Gint*);

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
