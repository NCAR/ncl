/*
 *	$Id: s_gqfais.c,v 1.5 2008-07-23 17:24:21 haley Exp $
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
 *  Inquire fill area interior style 
 */

#include <ncarg/gks.h>

extern void NGCALLF(gqfais,GQFAIS)(Gint*,Gfill_int_style*);

void ginq_fill_int_style
#ifdef NeedFuncProto
(
    Gint            *err_ind,         /* OUT current error indicator       */
    Gfill_int_style *fill_int_style   /* OUT current fill area style index */
)
#else
( err_ind, fill_int_style )
    Gint            *err_ind;
    Gfill_int_style *fill_int_style;
#endif
{
    NGCALLF(gqfais,GQFAIS)(err_ind,fill_int_style);
}
