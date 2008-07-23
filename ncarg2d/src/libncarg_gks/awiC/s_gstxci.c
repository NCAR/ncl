/*
 *	$Id: s_gstxci.c,v 1.5 2008-07-23 17:24:24 haley Exp $
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
 *  Set text colour index  
 */

#include <ncarg/gks.h>

extern void NGCALLF(gstxci,GSTXCI)(Gint*);

void gset_text_colr_ind
#ifdef NeedFuncProto
(
    Gint text_colr_ind  /* text colour index */
)
#else
( text_colr_ind )
    Gint text_colr_ind;
#endif
{
    NGCALLF(gstxci,GSTXCI)(&text_colr_ind);
}
