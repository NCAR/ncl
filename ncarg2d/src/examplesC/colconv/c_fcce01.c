/*
 *  $Id: c_fcce01.c,v 1.1 1994-07-18 16:20:44 haley Exp $
 */
#include <stdio.h>
#include <math.h>

/*
 * Include function prototypes
 */
#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define WSTYPE SED_WSTYPE
#define WKID   1

main()
{
    Gcolr_rep rgb;
    Gpoint_list line;
    Gpoint pos;
/*
 *  This program provides a demonstration of setting up a color table 
 *  and specifying colors for the GKS primitives: lines, markers, text, 
 *  and filled areas.
 *
 *  Open GKS, open and activate a workstation.
 */
    gopen_gks("6",1);
    gopen_ws( WKID, NULL, WSTYPE);
    gactivate_ws( WKID );
/*
 *  Set up the color table for the CGM workstation:
 *    
 *    Index  Color
 *    -----  -----
 *      0    Black (background color)
 *      1    White (foreground color)
 *      2    Red
 *      3    Green
 *      4    Yellow
 *      5    Cyan
 */
    rgb.rgb.red = 0.0; rgb.rgb.green = 0.0; rgb.rgb.blue = 0.0;
    gset_colr_rep(WKID, 0, &rgb);
    rgb.rgb.red = 1.0; rgb.rgb.green = 1.0; rgb.rgb.blue = 1.0;
    gset_colr_rep(WKID, 1, &rgb);
    rgb.rgb.red = 1.0; rgb.rgb.green = 0.0; rgb.rgb.blue = 0.0;
    gset_colr_rep(WKID, 2, &rgb);
    rgb.rgb.red = 0.0; rgb.rgb.green = 1.0; rgb.rgb.blue = 0.0;
    gset_colr_rep(WKID, 3, &rgb);
    rgb.rgb.red = 1.0; rgb.rgb.green = 1.0; rgb.rgb.blue = 0.0;
    gset_colr_rep(WKID, 4, &rgb);
    rgb.rgb.red = 0.0; rgb.rgb.green = 1.0; rgb.rgb.blue = 1.0;
    gset_colr_rep(WKID, 5, &rgb);
    
    line.points = (Gpoint *)malloc(5*sizeof(Gpoint));
    if( !line.points ) {
        fprintf( stderr, "c_fcce01: Not enough memory to create fill area structure\n" );
        gemergency_close_gks();
        exit(1);
    }
/* 
 * Draw a green rectangle
 */
    gset_line_colr_ind(3);
    line.num_points = 5;

    line.points[0].x = 0.15;
    line.points[1].x = 0.35;
    line.points[2].x = 0.35;
    line.points[3].x = 0.15;
    line.points[4].x = 0.15;
    line.points[0].y = 0.45;
    line.points[1].y = 0.45;
    line.points[2].y = 0.65;
    line.points[3].y = 0.65;
    line.points[4].y = 0.45;
    gpolyline(&line);
/*
 *  Draw an asterisk scaled by 4. in the foreground color.
 */
    gset_marker_size(4.);
    line.num_points = 1;
    line.points[0].x = .5;
    line.points[0].y = .25;
    gpolymarker(&line);
/*
 *  Draw a text string in yellow.
 */
    gset_text_colr_ind(4);
    pos.x = pos.y = .5;
    gtext(&pos,"Text");
/*
 *  Draw a filled rectangle in cyan.
 */
    gset_fill_colr_ind(5);
    gset_fill_int_style(GSTYLE_SOLID);
    line.num_points = 5;
    line.points[0].x = 0.65;
    line.points[1].x = 0.85;
    line.points[2].x = 0.85;
    line.points[3].x = 0.65;
    line.points[4].x = 0.65;
    line.points[0].y = 0.45;
    line.points[1].y = 0.45;
    line.points[2].y = 0.65;
    line.points[3].y = 0.65;
    line.points[4].y = 0.45;
    gfill_area(&line);
/*
 *  Draw a red asterisk.
 */
    gset_marker_colr_ind(2);
    line.num_points = 1;
    line.points[0].x = .5;
    line.points[0].y = .75;
    gpolymarker(&line);
/*
 *  Terminate the picture, deactivate and close the CGM workstation, 
 *  close GKS.
 */
    gdeactivate_ws(WKID);
    gclose_ws(WKID);
    gclose_gks();
}
