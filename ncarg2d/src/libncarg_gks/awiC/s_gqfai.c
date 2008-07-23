/*
 *	$Id: s_gqfai.c,v 1.5 2008-07-23 17:24:21 haley Exp $
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
 *  Inquire fill area index  
 */

#include <ncarg/gks.h>

extern void NGCALLF(gqfai,GQFAI)(Gint*,Gint*);

void ginq_fill_ind
#ifdef NeedFuncProto
(
    Gint *err_ind,  /*  OUT error indicator          */
    Gint *fill_ind  /*  OUT current fill area index  */
)
#else
( err_ind, fill_ind )
    Gint *err_ind;
    Gint *fill_ind;
#endif
{
    NGCALLF(gqfai,GQFAI)(err_ind,fill_ind);
}
