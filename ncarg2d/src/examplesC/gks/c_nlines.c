#include <stdio.h>

#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

main()
{
/*
 *  Illustrate polylines with different colors, widths, and types.
 */
    int i;
    float y;
    Gpoint_list line;
    Gcolr_rep rgb[3];
/*
 *  Initialize
 */
    rgb[0].rgb.red = 0.0; rgb[0].rgb.green = 0.0; rgb[0].rgb.blue = 0.6;
    rgb[1].rgb.red = 1.0; rgb[1].rgb.green = 1.0; rgb[1].rgb.blue = 1.0;
    rgb[2].rgb.red = 0.0; rgb[2].rgb.green = 1.0; rgb[2].rgb.blue = 0.0;
    line.num_points = 2;
    line.points = (Gpoint *)malloc(line.num_points*sizeof(Gpoint));
    if( line.points == NULL ) {
        (void)fprintf( stderr, "nlines:  no memory to create line struct\n" );
        exit(0);
    }
/*
 *  Open GKS, open and activate an X workstation.
 */
    gopen_gks("stdout",0);
    gopen_ws( 1, NULL, 8 );
    gactivate_ws (1);
/*
 *  Define color indices, color index 0 defines the background color.
 */
    for( i = 0; i < 3; i++ ) gset_colr_rep(1,i,&rgb[i]);
/*
 *  Draw line types.
 */
    line.points[0].x = 0.;
    line.points[1].x = 1.;
    gset_line_colr_ind(2);
    for( i = 1; i <= 5; i++ ) {
        gset_linetype(i);
        y = 0.2*(float)i-.1;
        line.points[0].y = y;
        line.points[1].y = y;
        gpolyline(&line);
    }
    c_frame();
/*
 *  Deactivate and close the workstation, close GKS.
 */
    gdeactivate_ws (1);
    gclose_ws (1);
    gclose_gks();
}
