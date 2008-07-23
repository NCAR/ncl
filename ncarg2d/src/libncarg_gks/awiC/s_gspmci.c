/*
 *	$Id: s_gspmci.c,v 1.5 2008-07-23 17:24:24 haley Exp $
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
 *  Set polymarker colour index  
 */

#include <ncarg/gks.h>

extern void NGCALLF(gspmci,GSPMCI)(Gint*);

void gset_marker_colr_ind
#ifdef NeedFuncProto
(
    Gint marker_colr_ind  /* polymarker colour index */
)
#else
( marker_colr_ind )
    Gint marker_colr_ind;
#endif
{
    NGCALLF(gspmci,GSPMCI)(&marker_colr_ind);
}
