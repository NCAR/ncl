/*
 *	$Id: s_gqfaci.c,v 1.5 2008-07-23 17:24:21 haley Exp $
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
 *  Inquire fill area colour index  
 */

#include <ncarg/gks.h>

extern void NGCALLF(gqfaci,GQFACI)(Gint*,Gint*);

void ginq_fill_colr_ind
#ifdef NeedFuncProto
(
    Gint *err_ind,        /* OUT error indicator                 */
    Gint *fill_colr_ind   /* OUT current fill area colour index  */
)
#else
( err_ind, fill_colr_ind )
    Gint *err_ind;
    Gint *fill_colr_ind;
#endif
{
    NGCALLF(gqfaci,GQFACI)(err_ind,fill_colr_ind);
}
