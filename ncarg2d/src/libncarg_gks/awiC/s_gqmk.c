/*
 *	$Id: s_gqmk.c,v 1.5 2008-07-23 17:24:21 haley Exp $
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
 *  Inquire marker type 
 */

#include <ncarg/gks.h>

extern void NGCALLF(gqmk,GQMK)(Gint*,Gint*);

void ginq_marker_type
#ifdef NeedFuncProto
(
    Gint *err_ind,      /* OUT error indicator      */
    Gint *marker_type   /* OUT current marker type  */
)
#else
( err_ind, marker_type )
    Gint *err_ind;
    Gint *marker_type;
#endif
{
    NGCALLF(gqmk,GQMK)(err_ind,marker_type);
}
