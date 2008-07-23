/*
 *	$Id: s_gspmi.c,v 1.5 2008-07-23 17:24:24 haley Exp $
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
 *  Set polymarker index  
 */

#include <ncarg/gks.h>

extern void NGCALLF(gspmi,GSPMI)(Gint*);

void gset_marker_ind
#ifdef NeedFuncProto
(
    Gint marker_ind  /*  polymarker index  */
)
#else
( marker_ind )
    Gint marker_ind;
#endif
{
    NGCALLF(gspmi,GSPMI)(&marker_ind);
}
