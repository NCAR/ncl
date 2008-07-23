/*
 *	$Id: s_gstxi.c,v 1.5 2008-07-23 17:24:24 haley Exp $
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
 *  Set text index  
 */

#include <ncarg/gks.h>

extern void NGCALLF(gstxi,GSTXI)(Gint*);

void gset_text_ind
#ifdef NeedFuncProto
(
    Gint text_ind  /*  text index  */
)
#else
( text_ind )
    Gint text_ind;
#endif
{
    NGCALLF(gstxi,GSTXI)(&text_ind);
}
