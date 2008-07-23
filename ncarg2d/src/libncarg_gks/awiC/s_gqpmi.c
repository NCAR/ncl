/*
 *	$Id: s_gqpmi.c,v 1.5 2008-07-23 17:24:22 haley Exp $
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
 *  Inquire polymarker index  
 */

#include <ncarg/gks.h>

extern void NGCALLF(gqpmi,GQPMI)(Gint*,Gint*);

void ginq_marker_ind
#ifdef NeedFuncProto
(
    Gint *err_ind,    /*  OUT error indicator           */
    Gint *marker_ind  /*  OUT current polymarker index  */
)
#else
( err_ind, marker_ind )
    Gint *err_ind;    /*  OUT error indicator           */
    Gint *marker_ind; /*  OUT current polymarker index  */
#endif
{
    NGCALLF(gqpmi,GQPMI)(err_ind,marker_ind);
}
