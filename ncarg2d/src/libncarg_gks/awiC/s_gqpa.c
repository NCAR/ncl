/*
 *	$Id: s_gqpa.c,v 1.5 2008-07-23 17:24:21 haley Exp $
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
 *  Get pattern size  
 */

#include <ncarg/gks.h>

extern void NGCALLF(gqpa,GQPA)(Gint*,Gfloat*,Gfloat*);

void ginq_pat_size
#ifdef NeedFuncProto
(
    Gint    *err_ind, /* OUT error indicator */
    Gdouble *x_size,  /* OUT x size          */
    Gdouble *y_size   /* OUT y size          */
)
#else
( err_ind, x_size, y_size )
    Gint    *err_ind;
    Gdouble *x_size;
    Gdouble *y_size;
#endif
{
    Gfloat x, y;
    NGCALLF(gqpa,GQPA)(err_ind,&x,&y);
    *x_size = (Gdouble) x;
    *y_size = (Gdouble) y;
}

