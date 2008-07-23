/*
 *	$Id: s_gsmk.c,v 1.5 2008-07-23 17:24:24 haley Exp $
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
 *  Set marker type  
 */

#include <ncarg/gks.h>

extern void NGCALLF(gsmk,GSMK)(Gint*);

void gset_marker_type
#ifdef NeedFuncProto
(
    Gint marker_type  /* marker type  */
)
#else
( marker_type )
    Gint marker_type;
#endif
{
    NGCALLF(gsmk,GSMK)(&marker_type);
}
