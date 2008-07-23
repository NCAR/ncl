/*
 *	$Id: s_gspa.c,v 1.5 2008-07-23 17:24:24 haley Exp $
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
 *  Set pattern size  
 */

#include <ncarg/gks.h>

extern void NGCALLF(gspa,GSPA)(Gfloat*,Gfloat*);

void gset_pat_size
#ifdef NeedFuncProto
(
    Gdouble x_size, /* x size */
    Gdouble y_size  /* y size */
)
#else
( x_size, y_size )
    Gdouble x_size;
    Gdouble y_size;
#endif
{
    Gfloat x, y;
    x = (Gfloat) x_size;
    y = (Gfloat) y_size;
    NGCALLF(gspa,GSPA)(&x,&y);
}

