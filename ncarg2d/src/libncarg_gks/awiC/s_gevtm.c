/*
 *	$Id: s_gevtm.c,v 1.5 2008-07-23 17:24:19 haley Exp $
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
 *  Evaluate Transformation Matrix
 */

#include <ncarg/gks.h>

extern void NGCALLF(gevtm,GEVTM)(const Gfloat*,const Gfloat*,const Gfloat*,
                                 const Gfloat*,Gfloat*,const Gfloat*,
                                 const Gfloat*,
                                 Gcoord_switch*,Gfloat tm2[3][2]);

void geval_tran_matrix
#ifdef NeedFuncProto
(
    const Gpoint   *point,        /* fixed point               */
    const Gvec     *shift,        /* shift vector              */
    Gdouble        angle,         /* rotation angle            */
    const Gvec     *scale,        /* scale factors             */
    Gcoord_switch  coord_switch,  /* coordinate switch         */
    Gtran_matrix   tran_matrix    /* OUT transformation matrix */
)
#else
( point, shift, angle, scale, coord_switch, tran_matrix )
    Gpoint         *point;
    Gvec           *shift;
    Gdouble        angle;
    Gvec           *scale;
    Gcoord_switch  coord_switch;
    Gtran_matrix   tran_matrix;
#endif
{
    Gfloat angle2;
    int i;
    Gfloat tm2[3][2];
/*
 * Transpose 2x3 array
 */
    for( i = 0; i < 3; i++ ) {
        tm2[i][0] = tran_matrix[0][i];
        tm2[i][1] = tran_matrix[1][i];
    }
    angle2 = (Gfloat)angle;
    NGCALLF(gevtm,GEVTM)(&point->x,&point->y,
                         &shift->delta_x,&shift->delta_y,
                         &angle2,
                         &scale->delta_x,&scale->delta_y,
                         &coord_switch,tm2);
/*
 * Transpose 3x2 array
 */
    for( i = 0; i < 3; i++ ) {
        tran_matrix[0][i] = tm2[i][0];
        tran_matrix[1][i] = tm2[i][1];
    }
}
