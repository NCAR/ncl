/*
 *	$Id: s_gqpawv.c,v 1.5 2008-07-23 17:24:22 haley Exp $
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
 *  Inquire pattern width vector  
 */

#include <ncarg/gks.h>

extern void NGCALLF(gqpa,GQPA)(Gint*,Gfloat*,Gfloat*,Gfloat*,Gfloat*);

void ginq_pat_width_vec
#ifdef NeedFuncProto
(
    Gint *err_ind,        /* OUT error indicator              */
    Gvec *pat_width_vec   /* OUT current pattern width vector */
)
#else
( err_ind, pat_width_vec )
    Gint *err_ind;
    Gvec *pat_width_vec;
#endif
{
    Gfloat dumx, dumy;
    NGCALLF(gqpa,GQPA)(err_ind,&pat_width_vec->delta_x,&pat_width_vec->delta_y,
                       &dumx,&dumy);
}
