/*
 *	$Id: s_gssgt.c,v 1.3 2000-08-01 14:36:00 haley Exp $
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
 *  Set Segment Transformation
 */

#include <ncarg/gks.h>

extern void NGCALLF(gssgt,GSSGT)(Gint*_name,Gfloat tm2[3][2]);

void gset_seg_tran
#ifdef NeedFuncProto
(
    Gint seg_name,                 /* segment name          */
    const Gtran_matrix tran_matrix /* transformation matrix */
)
#else
( seg_name, tran_matrix )
    Gint seg_name;
    Gtran_matrix tran_matrix;
#endif
{
    Gfloat tm2[3][2];
    int i;
/*
 * Transpose 2x3 array
 */
    for( i = 0; i < 3; i++ ) {
        tm2[i][0] = tran_matrix[0][i];
        tm2[i][1] = tran_matrix[1][i];
	}
    NGCALLF(gssgt,GSSGT)(&seg_name,tm2);
}
