/*
 *	$Id: s_gpl.c,v 1.3 2000-07-12 17:06:09 haley Exp $
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
 *  Polyline  
 */
#include <stdlib.h>

#include <ncarg/gks.h>

void gpolyline
#ifdef NeedFuncProto
(
    const Gpoint_list *point_list /* list of points */
)
#else
( point_list )
    Gpoint_list *point_list;
#endif
{
    Gfloat *x, *y;
    int i;

    if( point_list->points != NULL ) {
/*
 * Alloc space for x array
 */
        x = (Gfloat *)malloc(point_list->num_points*sizeof(Gfloat));
        if( !x ) {
            fprintf(stderr, "gpolyline:  cannot alloc space for array x\n");
            return;
        }
/*
 *  Alloc space for y array
 */
        y = (Gfloat *)malloc(point_list->num_points*sizeof(Gfloat));
        if( !y ) {
            fprintf(stderr, "gpolyline:  cannot alloc space for array y\n");
            return;
        }
/*
 *  Initialize x and y
 */
        for( i = 0; i < point_list->num_points; i++ ) {
            x[i] = point_list->points[i].x;
            y[i] = point_list->points[i].y;
        }
/*
 *  Call Fortran GKS routine
 */
        NGCALLF(gpl,GPL)(&point_list->num_points,x,y);
/*
 *  Free memory allocated for x and y
 */
        free((Gfloat *) x);
        free((Gfloat *) y);
    }
    else {
        fprintf(stderr, "gpolyline:  point_list->points is a NULL structure\n" );
        return;
    }
}
