/*
 *	$Id: s_gpl.c,v 1.6 2008-07-23 17:24:20 haley Exp $
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
 *  Polyline  
 */
#include <stdlib.h>

#include <ncarg/gks.h>

extern void NGCALLF(gpl,GPL)(const Gint*,Gfloat*,Gfloat*);

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
