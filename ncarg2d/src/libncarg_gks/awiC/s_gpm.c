/*
 *	$Id: s_gpm.c,v 1.1 1997-03-05 19:12:52 haley Exp $
 */
/*
 *  Polymarker  
 */

#include <ncarg/gks.h>

void gpolymarker
#ifdef NeedFuncProto
(
    const Gpoint_list *point_list /* list of points */
)
#else
( point_list )
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
            fprintf(stderr, "gpolymarker:  cannot alloc space for array x\n");
            return;
        }
/*
 *  Alloc space for y array
 */
        y = (Gfloat *)malloc(point_list->num_points*sizeof(Gfloat));
        if( !y ) {
            fprintf(stderr, "gpolymarker:  cannot alloc space for array y\n");
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
        NGCALLF(gpm,GPM)(&point_list->num_points,x,y);
/*
 *  Free memory allocated for x and y
 */
        free((Gfloat *) x);
        free((Gfloat *) y);
    }
    else {
        fprintf(stderr, "gpolymarker:  point_list->points is a NULL structure\n" );
        return;
    }
}
