/*
 *	$Id: s_gevtm.c,v 1.4 2000-08-22 15:08:46 haley Exp $
 */
/************************************************************************
*                                                                       *
*                Copyright (C)  2000                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
* This file is free software; you can redistribute it and/or modify     *
* it under the terms of the GNU General Public License as published     *
* by the Free Software Foundation; either version 2 of the License, or  *
* (at your option) any later version.                                   *
*                                                                       *
* This software is distributed in the hope that it will be useful, but  *
* WITHOUT ANY WARRANTY; without even the implied warranty of            *
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
* General Public License for more details.                              *
*                                                                       *
* You should have received a copy of the GNU General Public License     *
* along with this software; if not, write to the Free Software         *
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307   *
* USA.                                                                  *
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
