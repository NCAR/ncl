/*
 *	$Id: s_gslwsc.c,v 1.5 2008-07-23 17:24:24 haley Exp $
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
 *  Set linewidth scale factor  
 */

#include <ncarg/gks.h>

extern void NGCALLF(gslwsc,GSLWSC)(Gfloat*);

void gset_linewidth
#ifdef NeedFuncProto
(
    Gdouble linewidth  /* linewidth scale factor  */
)
#else
( linewidth )
    Gdouble linewidth;
#endif
{
    Gfloat width;
    width = (Gfloat)linewidth;
    NGCALLF(gslwsc,GSLWSC)(&width);
}
