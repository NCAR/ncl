#include <stdio.h>

#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

Gtran_matrix tin = { 1., 0., 0., 0., 1., 0. };

main()
{
    int i, j;
    float x[2],y[2],d2r;
    float centx = 5., centy = .5;
    Gdouble phi;
    Gcoord_switch sw;
    Gcolr_rep rgb;
    Gpoint point;
    Gvec shift, scale;
    Gtran_matrix tm;
    Gpoint_list marker, line;
/*
 * Initialize
 */
    x[0] = y[0] = 0.;
    x[1] = y[1] = 1.;
    rgb.rgb.red = 1.;
    rgb.rgb.green = rgb.rgb.blue = 0.;
    marker.num_points = 1;
    marker.points = (Gpoint *)malloc(marker.num_points*sizeof(Gpoint));
    marker.points[0].x = marker.points[0].y = .5;
    line.num_points = 6;
    line.points = (Gpoint *)malloc(line.num_points*sizeof(Gpoint));
    line.points[0].x = 0.45;
    line.points[1].x = 0.55;
    line.points[2].x = 0.55;
    line.points[3].x = 0.50;
    line.points[4].x = 0.45;
    line.points[5].x = 0.45;
    line.points[0].y = 0.45;
    line.points[1].y = 0.45;
    line.points[2].y = 0.55;
    line.points[3].y = 0.60;
    line.points[4].y = 0.55;
    line.points[5].y = 0.45;
/*
 *  Open GKS.
 */
    gopen_gks("stdout",0);
/*
 *  Open and activate WISS.
 */
    gopen_ws(2,NULL,3);
    gactivate_ws(2);
/*
 *  Create a segment.
 */
    gcreate_seg(9);
/*
 *  Draw a figure and a marker.
 */
    gpolyline(&line);
    gpolymarker(&marker);
/*
 *  Close segment.
 */
    gclose_seg();
/*
 *  Open and activate an X workstation.
 */
    gopen_ws(1,NULL,8);
    gactivate_ws(1);
    gset_colr_rep(1,2,&rgb);
/*
 *  Start with the identity matrix and translate
 */
    d2r = 3.1415926/180.;
    point.x = point.y = 0.;
    shift.delta_x = .05;
    shift.delta_y = .1;
    phi = 0.;
    scale.delta_x = 1.;
    scale.delta_y = 1.;
    sw = 0;
    gaccum_tran_matrix(tin,&point,&shift,phi,&scale,sw,tm);
/*
 *  Rotate 45 degrees about (.55,.6)
 */
    for( i = 0; i < 2; i++ ) {
        for( j = 0; j < 3; j++ ) {
            tin[i][j] = tm[i][j];
        }
    }
    point.x = .55;
    point.y = .6;
    shift.delta_x = 0.;
    shift.delta_y = 0.;
    phi = 45.*d2r;
    scale.delta_x = 1.;
    scale.delta_y = 1.;
    sw = 0.;
    gaccum_tran_matrix(tin,&point,&shift,phi,&scale,sw,tm);
/*
 *  Scale by 2. in the X direction about (.55,.6)
 */
    for( i = 0; i < 2; i++ ) {
        for( j = 0; j < 3; j++ ) {
            tin[i][j] = tm[i][j];
        }
    }
    point.x = .55;
    point.y = .6;
    shift.delta_x = 0.;
    shift.delta_y = 0.;
    phi = 0.;
    scale.delta_x = 2.;
    scale.delta_y = 1.;
    sw = 0.;
    gaccum_tran_matrix(tin,&point,&shift,phi,&scale,sw,tm);
/*
 *  Define the segment transformation matrix.
 */
    gset_seg_tran(9,tm);
/*
 *  Copy the segment to the X workstation.
 */
    gcopy_seg_ws(1,9);
/*
 *  Draw rule lines.
 */
    gset_line_colr_ind(2);
    c_line(0.,centy,1.,centy);
    c_line(centx,0.,centy,1.);
    c_frame();
/*
 *  Deactivate the workstations.
 */
    gdeactivate_ws(1);
    gdeactivate_ws(2);
/*
 *  Close the workstations.
 */
    gclose_ws(1);
    gclose_ws(2);
/*
 *  Close GKS.
 */
    gclose_gks();
}
