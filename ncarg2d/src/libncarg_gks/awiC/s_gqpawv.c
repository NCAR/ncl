/*
 *	$Id: s_gqpawv.c,v 1.3 2000-08-01 14:35:51 haley Exp $
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
