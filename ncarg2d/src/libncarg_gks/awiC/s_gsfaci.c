/*
 *	$Id: s_gsfaci.c,v 1.5 2008-07-23 17:24:24 haley Exp $
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
 *  Set fill area colour index  
 */

#include <ncarg/gks.h>

extern void NGCALLF(gsfaci,GSFACI)(Gint*);

void gset_fill_colr_ind
#ifdef NeedFuncProto
(
    Gint fill_colr_ind  /* fill area colour index  */
)
#else
( fill_colr_ind )
    Gint fill_colr_ind;
#endif
{
    NGCALLF(gsfaci,GSFACI)(&fill_colr_ind);
}
