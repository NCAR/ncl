/*
 *	$Id: s_gqlwsc.c,v 1.5 2008-07-23 17:24:21 haley Exp $
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
 *  Inquire linewidth scale factor  
 */

#include <ncarg/gks.h>

extern void NGCALLF(gqlwsc,GQLWSC)(Gint*,Gfloat*);

void ginq_linewidth
#ifdef NeedFuncProto
(
    Gint    *err_ind,    /* OUT error indicator                 */
    Gdouble *linewidth   /* OUT current linewidth scale factor  */
)
#else
( err_ind, linewidth )
    Gint    *err_ind;
    Gdouble *linewidth;
#endif
{
    Gfloat width;
    NGCALLF(gqlwsc,GQLWSC)(err_ind,&width);
    *linewidth = (Gdouble)width;
}
