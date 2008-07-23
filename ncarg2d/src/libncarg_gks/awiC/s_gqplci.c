/*
 *	$Id: s_gqplci.c,v 1.5 2008-07-23 17:24:22 haley Exp $
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
 * Inquire polyline colour index  
 */

#include <ncarg/gks.h>

extern void NGCALLF(gqplci,GQPLCI)(Gint*,Gint*);

void ginq_line_colr_ind
#ifdef NeedFuncProto
(
    Gint *err_ind,       /* OUT error indicator               */
    Gint *line_colr_ind  /* OUT current polyline colour index */
)
#else
( err_ind, line_colr_ind )
    Gint *err_ind;
    Gint *line_colr_ind;
#endif
{
    NGCALLF(gqplci,GQPLCI)(err_ind,line_colr_ind);
}
