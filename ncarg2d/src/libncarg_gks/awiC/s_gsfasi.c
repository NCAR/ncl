/*
 *	$Id: s_gsfasi.c,v 1.5 2008-07-23 17:24:24 haley Exp $
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
 *  Set fill area style index  
 */

#include <ncarg/gks.h>

extern void NGCALLF(gsfasi,GSFASI)(Gint*);

void gset_fill_style_ind
#ifdef NeedFuncProto
(
    Gint fill_style_ind  /* fill area style index */
)
#else
( fill_style_ind )
    Gint fill_style_ind;
#endif
{
    NGCALLF(gsfasi,GSFASI)(&fill_style_ind);
}
