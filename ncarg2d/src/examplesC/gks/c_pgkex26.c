#include <math.h>
#include <stdio.h>

#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define WKID   1

/*
 *  Getting a PostScript plot to fill an entire page.
 */

main()
{
    Gcolr_rep rgb;
    Gpoint_list line;
/*
 *  Illustrate creating multiple metafiles in the same job.
 */
/*
 *  Open GKS.
 */
    gopen_gks ("stdout",0);
/*
 *  Open and activate a metafile with the name META01.
 */
    c_ngsetc("ME","META01");
    gopen_ws(WKID, NULL, 1);
    gactivate_ws (WKID);
/*
 *  Draw a single red line in the only frame in META01 .
 */
    rgb.rgb.red = 1.0; rgb.rgb.green =  0.0; rgb.rgb.blue = 0.0;
    gset_colr_rep(WKID,1,&rgb);
    gset_line_colr_ind(1);
    line.num_points = 2;
    line.points = (Gpoint *)malloc(line.num_points*sizeof(Gpoint));
    if( !line.points ) {
        fprintf( stderr, "c_pgkex26: Not enough memory to create line structure\n" );
        gemergency_close_gks();
        exit(1);
    }
    line.points[0].x = line.points[0].y = 0.;
    line.points[1].x = line.points[1].y = 1.;
    gpolyline(&line);
    c_frame();
/*
 *  Deactivate and close the META01 metafile.
 */
    gdeactivate_ws (WKID);
    gclose_ws (WKID);
/*
 *  Open and activate META02.
 */
    c_ngsetc("ME","META02");
    gopen_ws (WKID, NULL, 1);
    gactivate_ws (WKID);
/*
 *  Draw a single green line in the only frame in META02 (all color
 *  table entries have to be redefined).
 */
    rgb.rgb.red = 0.0; rgb.rgb.green =  1.0; rgb.rgb.blue = 0.0;
    gset_colr_rep(WKID,2,&rgb);
    gset_line_colr_ind(2);
    gpolyline(&line);
    c_frame();
/*
 *  Deactivate and close the META02 metafile.
 */
    gdeactivate_ws (WKID);
    gclose_ws (WKID);
/*
 *  Close GKS.
 */
    gclose_gks();
}
