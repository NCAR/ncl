/*
 *	$Id: s_gschxp.c,v 1.5 2008-07-23 17:24:23 haley Exp $
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
 *  Set character expansion factor  
 */

#include <ncarg/gks.h>

extern void NGCALLF(gschxp,GSCHXP)(Gfloat*);

void gset_char_expan
#ifdef NeedFuncProto
(
    Gdouble char_expan  /* character expansion factor */
)
#else
( char_expan )
    Gdouble char_expan;
#endif
{
    Gfloat expan;
    expan = (Gfloat)char_expan;
    NGCALLF(gschxp,GSCHXP)(&expan);
}
