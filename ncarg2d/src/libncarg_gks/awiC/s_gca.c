/*
 *	$Id: s_gca.c,v 1.4 2000-08-01 14:35:43 haley Exp $
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
 *  Cell array  
 */
#include <stdlib.h>

#include <ncarg/gks.h>

extern void NGCALLF(gca,GCA)(const Gfloat*,const Gfloat*,const Gfloat*,
                             const Gfloat*,const Gint*,const Gint*,int*,
                             int*,int*,int*,int*);

void gcell_array
#ifdef NeedFuncProto
(
    const Grect    *rect,        /* cell rectangle */
    const Gpat_rep *colr_array   /* colour array   */
)
#else
( rect, colr_array )
    Grect    *rect;
    Gpat_rep *colr_array;
#endif
{
    int i, j, k, l, idx, idy, *colia_trans;

    if( colr_array->colr_array != NULL ) {
        idx = colr_array->dims.size_x;
        idy = colr_array->dims.size_y;
        colia_trans = (int *)malloc(idx*idy*sizeof(int));
        if( colia_trans ) {
            l = 0;
            for( j = 0; j < idy; j++ ) {
                for( i = 0; i < idx; i++ ) {
                    k = i * idy + j;
                    colia_trans[l++] = colr_array->colr_array[k];
                }
            }
            i = j = 1;
            NGCALLF(gca,GCA)(&rect->p.x,&rect->p.y,
                             &rect->q.x,&rect->q.y,
                             &colr_array->dims.size_x,
                             &colr_array->dims.size_y,&i,&j,
                             &idx,&idy,colia_trans);
        }
        else {
            fprintf( stderr, "gcell_array: not enough memory to create transposed array\n" );
            return;
        }
    }
    else {
        fprintf( stderr, "gcell_array:  colr_array is a NULL structure\n" );
        return;
    }
}
