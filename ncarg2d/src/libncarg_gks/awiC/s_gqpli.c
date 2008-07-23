/*
 *	$Id: s_gqpli.c,v 1.5 2008-07-23 17:24:22 haley Exp $
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
 *  Inquire polyline index  
 */

#include <ncarg/gks.h>

extern void NGCALLF(gqpli,GQPLI)(Gint*,Gint*);

void ginq_line_ind
#ifdef NeedFuncProto
(
    Gint *err_ind, /*  OUT error indicator         */
    Gint *line_ind /*  OUT current polyline index  */
)
#else
( err_ind, line_ind )
    Gint *err_ind;
    Gint *line_ind;
#endif
{
    NGCALLF(gqpli,GQPLI)(err_ind,line_ind);
}
