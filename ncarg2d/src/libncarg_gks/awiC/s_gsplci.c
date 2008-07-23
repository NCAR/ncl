/*
 *	$Id: s_gsplci.c,v 1.5 2008-07-23 17:24:24 haley Exp $
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
 *  Set polyline colour index  
 */

#include <ncarg/gks.h>

extern void NGCALLF(gsplci,GSPLCI)(Gint*);

void gset_line_colr_ind
#ifdef NeedFuncProto
(
    Gint line_colr_ind  /* polyline colour index */
)
#else
( line_colr_ind )
    Gint line_colr_ind;
#endif
{
    NGCALLF(gsplci,GSPLCI)(&line_colr_ind);
}
