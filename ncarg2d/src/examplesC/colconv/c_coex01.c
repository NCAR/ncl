/*
 *  $Id: c_coex01.c,v 1.1 1994-05-13 14:25:28 haley Exp $
 */
#include <stdio.h>
#include <math.h>

/*
 * Include function prototypes
 */
#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

main()
{
/*
 *  This program produces five 9 x 9 color charts.  Blue and green
 *  intensities are varied on each chart, and the red intensity is
 *  varied between charts.  To produce the charts, color indices
 *  1-84 are used.  If plotting on a device with fewer than 84 colors
 *  available, unsatisfactory results will obtain.
 */
    float rgb[3][405];
    int npage;
    extern void colset(), setcol(), cube(), ttlbar(), title();
/*
 *  Open GKS.
 */
    gopen_gks("6",1);
    gopen_ws(1,NULL,1);
    gactivate_ws(1);
/*
 *  Set fill area interior style to solid.
 */
    gset_fill_int_style (GSTYLE_SOLID);
/*
 *  Define all permutations of RGB's needed.
 */
    colset(rgb);
/*
 *  Put out 5 color charts, varying the red value with each frame.
 */
    for( npage=1; npage <= 5; npage++ ) {
        setcol(rgb,npage);
        ttlbar();
        cube();
        title(npage);
        c_frame();
    }
/*
 *  Close GKS.
 */
    gdeactivate_ws(1);
    gclose_ws(1);
    gclose_gks();
}

void colset(rgb)
float rgb[3][405];
{
/*
 *  define the rgb color triples needed.  this is done by filling the
 *  rgb array with all 405 permutations for a 9 x 9 color cube in 5
 *  plots.  all values are normalized to fall in the range 0 to 1.
 */
    int i, j, k, ndx, n, index;
    float clrs[10];

    clrs[1] =    0.;
    clrs[2] =   32.;
    clrs[3] =   64.;
    clrs[4] =   96.;
    clrs[5] =  128.;
    clrs[6] =  160.;
    clrs[7] =  192.;
    clrs[8] =  224.;
    clrs[9] =  255.;

    index = 1;
    for( i=1; i <= 5; i++ ) {
        for( j=1; j <= 9; j++ ) {
            for( k=1; k <= 9; k++ ) {
                rgb [0][index-1] = clrs[2*i-1]/255.0;
                rgb [1][index-1] = clrs[j]/255.0;
                rgb [2][index-1] = clrs[k]/255.0;
                index = index + 1;
            }
        }
    }
}

void setcol(rgb,index)
float rgb[3][405];
int index;
{
    int ndx, n;
    Gcolr_rep color;
/*
 *  define color indices 3-84 to contain the desired colors.
 */
    ndx = 81*(index-1);
    for( n=1; n <= 81; n++ ) {
        color.rgb.red = rgb[0][ndx];
        color.rgb.green = rgb[1][ndx];
        color.rgb.blue = rgb[2][ndx];
        gset_colr_rep(1,n+2,&color);
        ndx = ndx + 1;
    }
}

void cube()
{
/*
 *  draw 81 color squares of size delx x dely.  start at position
 *  (x1,y1).  use blank space of gapx and gapy between the squares.
 */
    float delx, dely, gapx, gapy, x1, y1, x, y;
    int i, l, indcol;
    Gpoint_list fill_area;
/*
 * Create structure to pass to gfill_area
 */
    fill_area.num_points = 4;
    fill_area.points = (Gpoint *)malloc(2*fill_area.num_points*sizeof(Gfloat));
    if( !fill_area.points ) {
        fprintf( stderr, "cube: Not enough memory to create fill area structure\n" );
        gemergency_close_gks();
        exit(1);
    }
    delx = 0.095;
    dely = 0.095;
    gapx = delx-0.003;
    gapy = dely-0.003;
    x1 = 0.13;
    y1 = 0.075;
/*
 *  set the parameters for the starting point.
 */
    x = x1;
    y = y1;
/*
 *  start with color index 3 so that the foreground and background
 *  color indices are not touched.
 */
    indcol = 3;
/*
 *  put out 9 x 9 squares.
 */
    for( i=1; i<=9; i++ ) {
        for( l=1; l<=9; l++ ) {
/*
 *  define square.
 */
            fill_area.points[0].x = x;
            fill_area.points[0].y = y;
            fill_area.points[1].x = x + gapx;
            fill_area.points[1].y = y;
            fill_area.points[2].x = x + gapx;
            fill_area.points[2].y = y + gapy;
            fill_area.points[3].x = x;
            fill_area.points[3].y = y + gapy;
/*
 *  set color to index indcol and draw square.
 */
            gset_fill_colr_ind(indcol);
            gfill_area(&fill_area);
            x = x + delx;
            indcol = indcol + 1;
        }
        y = y + dely;
        x = x1;
    }
    free(fill_area.points);
}

void ttlbar()
{
/*
 *  label the chart with the red intensity.
 */
    float x1, y1, xside, yside;
    Gpoint_list fill_area;

    x1 = 0.70;
    y1 = 0.93;
    xside = 0.28;
    yside = 0.04;
/*
 * Create structure to pass to gfill_area
 */
    fill_area.num_points = 4;
    fill_area.points = (Gpoint *)malloc(2*fill_area.num_points*sizeof(Gfloat));
    if( !fill_area.points ) {
        fprintf( stderr, "ttlbar: Not enough memory to create fill area structure\n" );
        gemergency_close_gks();
        exit(1);
    }
/*
 *  define the fill area.
 */
    fill_area.points[0].x = x1;
    fill_area.points[0].y = y1;
    fill_area.points[1].x = x1 + xside;
    fill_area.points[1].y = y1;
    fill_area.points[2].x = x1 + xside;
    fill_area.points[2].y = y1 + yside;
    fill_area.points[3].x = x1;
    fill_area.points[3].y = y1 + yside;
/*
 *  set the color and draw the fill area.
 */
    gset_fill_colr_ind(3);
    gfill_area(&fill_area);
    free(fill_area.points);
}

void title(npage)
int npage;
{
/*
 *  label the chart using white characters..
 */
    float il[9], rlv, x, y1, y2, delx, dely;
    float rvl;
    char rval[17],cval[4],dval[5], stmp[4];
    int i, j, l, itmp;
    Gpoint_list fill_area1, fill_area2;
    Gcolr_rep color;
/*
 * Create structures to pass to gfill_area
 */
    fill_area1.num_points = 3;
    fill_area2.num_points = 3;
    fill_area1.points = (Gpoint *)malloc(2*fill_area1.num_points*sizeof(Gfloat));
    fill_area2.points = (Gpoint *)malloc(2*fill_area2.num_points*sizeof(Gfloat));
    if( !fill_area1.points || !fill_area2.points) {
        fprintf( stderr, "title: Not enough memory to create fill area structures\n" );
        gemergency_close_gks();
        exit(1);
    }
    fill_area1.points[0].x =  .073;
    fill_area1.points[1].x =  .048;
    fill_area1.points[2].x =  .073;
    fill_area1.points[0].y =  .900;
    fill_area1.points[1].y =  .800;
    fill_area1.points[2].y =  .800;
    fill_area2.points[0].x =  .960;
    fill_area2.points[1].x =  .860;
    fill_area2.points[2].x =  .860;
    fill_area2.points[0].y =  .028;
    fill_area2.points[1].y =  .010;
    fill_area2.points[2].y =  .028;
    delx = 0.095;
    dely = 0.095;
    il[0] = 3;
    il[1] = 2;
    il[2] = 2;
    il[3] = 2;
    il[4] = 1;
    il[5] = 1;
    il[6] = 1;
    il[7] = 1;
    il[8] = 1;
    
    color.rgb.green = color.rgb.red = color.rgb.blue = 1.;
    gset_colr_rep(1,1,&color);
    gset_line_colr_ind(1);
/*
 *  print the title of each axis.
 */
    c_plchhq(0.5,0.007,"Blue Axis" ,0.015,0.,0.);
    c_plchhq(0.05,0.50,"Green Axis",0.015,90.,0.);
/*
 *  draw the arrow-line on each axis.
 */
    c_line(0.073,0.90,0.073,0.078);
    gset_fill_colr_ind(1);
    gfill_area(&fill_area1);

    c_line(0.15,0.028,0.96,0.028);
    gset_fill_colr_ind(1);
    gfill_area(&fill_area2);
/*
 *  print the red value for the frame at hand.
 */
    itmp = 64*(npage-1);
    if (npage == 5) itmp = 255;
    rlv = (float)(itmp)/255.;
    sprintf( rval, "red = %3d = %4.2f", itmp, rlv );
    c_plchhq(.84,.95,rval,.014,0.,0.);
/*
 *  print the green values up the side.
 */
    x = 0.10;
    y1 = 0.125;
    y2 = 0.108;
    for( i=1; i<=9; i++) {
        itmp = 32*(i-1);
        if (i == 9) itmp=255;
        sprintf( cval, "%3d", itmp );
        rvl = (float)(itmp)/255.;
        sprintf( dval, "%4.2f", rvl );
        l = 0;
        for( j = il[i-1]-1; j <= 3; j++ ) stmp[l++] = cval[j];
        c_plchhq(x,y1,stmp,.008,0.,0.);
        c_plchhq(x,y2,dval,.008,0.,0.);
        y1 = y1+dely;
        y2 = y2+dely;
    }
/*
 *  print the blue values across the bottom.
 */
    y1 = 0.060;
    y2 = 0.043;
    x = 0.18;
    for( i=1; i<=9; i++ ) {
        itmp = 32*(i-1);
        if (i == 9) itmp=255;
        sprintf( cval, "%3d", itmp );
        rvl = (float)(itmp)/255.;
        sprintf( dval, "%4.2f", rvl );
        l = 0;
        for( j = il[i-1]-1; j <= 3; j++ ) stmp[l++] = cval[j];
        c_plchhq(x,y1,stmp,.008,0.,0.);
        c_plchhq(x,y2,dval,.008,0.,0.);
        x = x+delx;
    }
    free(fill_area1.points);
    free(fill_area2.points);
}
