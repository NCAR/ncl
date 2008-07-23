/*
 *	$Id: s_gfa.c,v 1.6 2008-07-23 17:24:20 haley Exp $
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
 *  Fill area  
 */
#include <stdlib.h>

#include <ncarg/gks.h>

extern void NGCALLF(gfa,GFA)(const Gint*,Gfloat*,Gfloat*);

void gfill_area
#ifdef NeedFuncProto
(
    const Gpoint_list *point_list    /* list of points */
)
#else
(point_list)
    Gpoint_list *point_list;
#endif
{
    int i;
    Gfloat *x, *y;

    if( point_list->points != NULL ) {
/*
 * Alloc space for x array
 */
        x = (Gfloat *)malloc(point_list->num_points*sizeof(Gfloat));
        if( !x ) {
            fprintf(stderr, "gfill_area:  cannot alloc space for array x\n");
            return;
        }
/*
 *  Alloc space for y array
 */
        y = (Gfloat *)malloc(point_list->num_points*sizeof(Gfloat));
        if( !y ) {
            fprintf(stderr, "gfill_area:  cannot alloc space for array y\n");
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
        NGCALLF(gfa,GFA)(&point_list->num_points,x,y);
/*
 *  Free memory allocated for x and y
 */
        free((Gfloat *) x);
        free((Gfloat *) y);
    }
    else {
        fprintf(stderr, "gfill_area:  point_list->points is a NULL structure\n" );
        return;
    }
}
