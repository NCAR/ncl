/*
 *	$Id: s_gca.c,v 1.6 2004-06-22 23:07:55 fred Exp $
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
    int i, j, k, l, idx, idy;

    if( colr_array->colr_array != NULL ) {
        idx = colr_array->dims.size_x;
        idy = colr_array->dims.size_y;
        i = j = 1;
        NGCALLF(gca,GCA)(&rect->p.x,&rect->p.y,
                         &rect->q.x,&rect->q.y,
                         &colr_array->dims.size_x,
                         &colr_array->dims.size_y,&i,&j,
                         &idx,&idy,colr_array->colr_array);
    }
    else {
        fprintf( stderr, "gcell_array:  colr_array is a NULL structure\n" );
        return;
    }
}
