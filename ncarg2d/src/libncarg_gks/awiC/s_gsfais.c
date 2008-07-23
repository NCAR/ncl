/*
 *	$Id: s_gsfais.c,v 1.5 2008-07-23 17:24:24 haley Exp $
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
 *  Set fill area interior style  
 */

#include <ncarg/gks.h>

extern void NGCALLF(gsfais,GSFAIS)(Gfill_int_style*);

void gset_fill_int_style
#ifdef NeedFuncProto
(
    Gfill_int_style fill_int_style  /* fill area style index */
)
#else
( fill_int_style )
    Gfill_int_style fill_int_style;
#endif
{
    NGCALLF(gsfais,GSFAIS)(&fill_int_style);
}
