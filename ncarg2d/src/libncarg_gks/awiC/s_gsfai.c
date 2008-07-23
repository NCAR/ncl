/*
 *	$Id: s_gsfai.c,v 1.5 2008-07-23 17:24:24 haley Exp $
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
 *  Set fill area index  
 */

#include <ncarg/gks.h>

extern void NGCALLF(gsfai,GSFAI)(Gint*);

void gset_fill_ind
#ifdef NeedFuncProto
(
    Gint fill_ind  /*  fill area index  */
)
#else
( fill_ind )
    Gint fill_ind;
#endif
{
    NGCALLF(gsfai,GSFAI)(&fill_ind);
}
