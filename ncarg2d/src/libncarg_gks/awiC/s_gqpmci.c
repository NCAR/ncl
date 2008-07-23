/*
 *	$Id: s_gqpmci.c,v 1.5 2008-07-23 17:24:22 haley Exp $
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
 *  Inquire polymarker colour index  
 */

#include <ncarg/gks.h>

extern void NGCALLF(gqpmci,GQPMCI)(Gint*,Gint*);

void ginq_marker_colr_ind
#ifdef NeedFuncProto
(
    Gint *err_ind,          /* OUT error indicator                 */
    Gint *marker_colr_ind   /* OUT current polymarker colour index */
)
#else
( err_ind, marker_colr_ind )
    Gint *err_ind;
    Gint *marker_colr_ind;
#endif
{
    NGCALLF(gqpmci,GQPMCI)(err_ind,marker_colr_ind);
}
