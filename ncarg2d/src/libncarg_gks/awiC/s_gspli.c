/*
 *	$Id: s_gspli.c,v 1.5 2008-07-23 17:24:24 haley Exp $
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
 *  Set polyline index  
 */

#include <ncarg/gks.h>

extern void NGCALLF(gspli,GSPLI)(Gint*);

void gset_line_ind
#ifdef NeedFuncProto
(
    Gint line_ind  /*  polyline index  */
)
#else
( line_ind )
    Gint line_ind;
#endif
{
    NGCALLF(gspli,GSPLI)(&line_ind);
}
