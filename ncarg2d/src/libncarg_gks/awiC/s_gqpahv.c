/*
 *	$Id: s_gqpahv.c,v 1.4 2000-08-22 15:09:03 haley Exp $
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
 *  Inquire pattern height vector  
 */

#include <ncarg/gks.h>

extern void NGCALLF(gqpa,GQPA)(Gint*,Gfloat*,Gfloat*,Gfloat*,Gfloat*);

void ginq_pat_ht_vec
#ifdef NeedFuncProto
(
    Gint *err_ind,     /* OUT error indicator               */
    Gvec *pat_ht_vec   /* OUT current pattern height vector */
)
#else
( err_ind, pat_ht_vec )
    Gint *err_ind;
    Gvec *pat_ht_vec;
#endif
{
    Gfloat dumx, dumy;
    NGCALLF(gqpa,GQPA)(err_ind,&dumx,&dumy,
                       &pat_ht_vec->delta_x,&pat_ht_vec->delta_y);
}
