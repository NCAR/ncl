/*
 *	$Id: s_gactm.c,v 1.2 2000-07-12 17:06:06 haley Exp $
 */
/************************************************************************
*                                                                       *
*                Copyright (C)  2000                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
* This file is free software; you can redistribute it and/or modify     *
* it under the terms of the GNU Lesser General Public License as        *
* published by the Free Software Foundation; either version 2.1 of the  *
* License, or (at your option) any later version.                       *
*                                                                       *
* This software is distributed in the hope that it will be useful, but  *
* WITHOUT ANY WARRANTY; without even the implied warranty of            *
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
* Lesser General Public License for more details.                       *
*                                                                       *
* You should have received a copy of the GNU Lesser General Public      *
* License along with this software; if not, write to the Free Software  *
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307   *
* USA.                                                                  *
*                                                                       *
************************************************************************/

/*
 *  Accumlate Transformation Matrix
 */

#include <ncarg/gks.h>

void gaccum_tran_matrix
#ifdef NeedFuncProto
(
    const Gtran_matrix matrix,        /* transformation matrix     */
    const Gpoint       *point,        /* fixed point               */
    const Gvec         *shift,        /* shift vector              */
    Gdouble            angle,         /* rotation angle            */
    const Gvec         *scale,        /* scale factors             */
    Gcoord_switch      coord_switch,  /* coordinate switch         */
    Gtran_matrix       tran_matrix    /* OUT transformation matrix */
)
#else
( matrix, point, shift, angle, scale, coord_switch, 
                         tran_matrix )
    Gtran_matrix   matrix;
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
    Gfloat m2[3][2], tm2[3][2];
/*
 * Transpose 2x3 array
 */
    for( i = 0; i < 3; i++ ) {
        m2[i][0] = matrix[0][i];
        m2[i][1] = matrix[1][i];
    }
    angle2 = (Gfloat)angle;
    NGCALLF(gactm,GACTM)(m2,&point->x,&point->y,
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
