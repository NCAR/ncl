#include <stdio.h>

#include <ncarg/gks.h>
#include <ncarg/ncargC.h>

main()
{
/*
 *  Illustrate polymarkers.
 */
    int i;
    Gpoint_list line, marker;
    Gcolr_rep rgb[3];
/*
 *  Initialize.
 */
    line.num_points = 2;
    line.points = (Gpoint *)malloc(line.num_points*sizeof(Gpoint));
    if( line.points == NULL ) {
        (void)fprintf( stderr, "nmrkpos:  no memory to create line struct\n" );
        exit(0);
    }
    marker.num_points = 1;
    marker.points = (Gpoint *)malloc(marker.num_points*sizeof(Gpoint));
    if( marker.points == NULL ) {
        (void)fprintf( stderr, "nmrkpos:  no memory to create marker struct\n" );
        exit(0);
    }
    rgb[0].rgb.red = 0.0; rgb[0].rgb.green = 0.0; rgb[0].rgb.blue = 0.6;
    rgb[1].rgb.red = 1.0; rgb[1].rgb.green = 0.0; rgb[1].rgb.blue = 0.0;
    rgb[2].rgb.red = 0.0; rgb[2].rgb.green = 1.0; rgb[2].rgb.blue = 0.0;
/*
 * Open GKS and open and activate workstation.
 */
    gopen_gks ("stdout",0);
    gopen_ws(1, NULL, 8);
    gactivate_ws(1);
/*
 *  define the necessary color indices.
 */
    for( i = 0; i < 3; i++ ) gset_colr_rep(1,i,&rgb[i]);
/*
 *  marker 3, asterisk (make an asterisk from the asterisk markers.)
 */
    marker.points[0].x = .5;
    marker.points[0].y = .5;
    gset_marker_type(4);
    gset_marker_colr_ind(2);
    gpolymarker(&marker);
    line.points[0].x = .5;
    line.points[1].x = .5;
    line.points[0].y = 0.;
    line.points[1].y = 1.;
    gpolyline(&line);
    line.points[0].x = 0.;
    line.points[1].x = 1.;
    line.points[0].y = .5;
    line.points[1].y = .5;
    gpolyline(&line);
    gset_marker_size(4.);
    marker.points[0].x = .7;
    gpolymarker(&marker);
    line.points[0].x = .7;
    line.points[1].x = .7;
    line.points[0].y = 0.;
    line.points[1].y = 1.;
    gpolyline(&line);
    c_frame();
/*
 *  deactivate and close the workstation, close gks.
 */
    gdeactivate_ws (1);
    gclose_ws (1);
    gclose_gks();
}
