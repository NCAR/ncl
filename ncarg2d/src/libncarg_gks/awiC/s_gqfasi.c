/*
 *	$Id: s_gqfasi.c,v 1.5 2008-07-23 17:24:21 haley Exp $
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
 *  Inquire fill area style index  
 */

#include <ncarg/gks.h>

extern void NGCALLF(gqfasi,GQFASI)(Gint*,Gint*);

void ginq_fill_style_ind
#ifdef NeedFuncProto
(
    Gint *err_ind,         /* OUT current error indicator       */
    Gint *fill_style_ind   /* OUT current fill area style index */
)
#else
( err_ind, fill_style_ind )
    Gint *err_ind;
    Gint *fill_style_ind;
#endif
{
    NGCALLF(gqfasi,GQFASI)(err_ind,fill_style_ind);
}
