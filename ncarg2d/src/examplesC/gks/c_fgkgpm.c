/*
 *	$Id: c_fgkgpm.c,v 1.1 1994-08-02 16:51:57 haley Exp $
 */
#include <stdio.h>
#include <math.h>

#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define WSTYPE SED_WSTYPE
#define WKID   1

#define ID  50

#define max(x,y)   ((x) > (y) ? (x) : (y))
#define pow2(x)    ((x)*(x))

main()
{
    Gpoint_list     markers;
	Gcolr_rep rgb;
	float x0, y0, r, ainc, x, y, xe, p, radinc;
	int j, jl, idx;
/*
 * Illustrate polymarkers.
 */
	float xm1[ID],ym1[ID],xm2[ID],ym2[ID],xm3[ID],ym3[ID];
/*
 * Open gks, open and activate a workstation.
 */
	gopen_gks ("stdout",0);
	gopen_ws (WKID, NULL, WSTYPE);
	gactivate_ws(WKID);
/*
 * Alloc space for markers
 */
	markers.points = (Gpoint *)malloc(sizeof(Gpoint)*50);
	if( !markers.points ) {
		printf( "c_fgkgpm: Not enough memory for markers\n" );
		gemergency_close_gks();
		exit(1);
	}
/*
 * Define the necessary color indices.
 */
	rgb.rgb.red = 1.; rgb.rgb.green = 1.; rgb.rgb.blue = 1.;
	gset_colr_rep(WKID,0,&rgb);
	rgb.rgb.red = 0.; rgb.rgb.green = 0.; rgb.rgb.blue = 0.;
	gset_colr_rep(WKID,1,&rgb);
	rgb.rgb.red = 1.; rgb.rgb.green = 1.; rgb.rgb.blue = 0.;
	gset_colr_rep(WKID,2,&rgb);
	rgb.rgb.red = 0.; rgb.rgb.green = 1.; rgb.rgb.blue = 0.;
	gset_colr_rep(WKID,3,&rgb);
	rgb.rgb.red = 1.; rgb.rgb.green = 1.; rgb.rgb.blue = 0.;
	gset_colr_rep(WKID,4,&rgb);
	rgb.rgb.red = 0.; rgb.rgb.green = 1.; rgb.rgb.blue = 1.;
	gset_colr_rep(WKID,5,&rgb);
	rgb.rgb.red = 1.; rgb.rgb.green = 0.; rgb.rgb.blue = 1.;
	gset_colr_rep(WKID,6,&rgb);
/*
 * Marker 1, dot (fill a large circular dot with markers of type 1).
 *
 * Position and radius of the dot outline.
 */
	x0 = .5;
	y0 = .7;
	r  = .08;
	jl = 8;
	ainc = r/(float)(jl);
	gset_marker_type(1);
	for( j = 0; j <= jl; j++ ) {
		y = y0+(float)(j)*ainc;
		xe = x0+sqrt(max(r*r-pow2(y-y0),0.));
		x = x0;
twenty:
/*
 * Fill the circle with dot markers using symmetries.
 */	
        markers.num_points = 1;
        markers.points[0].x = x;
        markers.points[0].y = y;
        gpolymarker(&markers);
        gset_marker_colr_ind(1);
        markers.points[0].y = 2*y0-y;
        gpolymarker(&markers);
        markers.points[0].x = 2*x0-x;
        markers.points[0].y = 2*y0-y;
        gpolymarker(&markers);
        markers.points[0].x = 2*x0-x;
        markers.points[0].y = y;
        gpolymarker(&markers);
        x = x+ainc;
        if (x <= xe) goto twenty;
    }
/*
 * Label the dot.
 */
    c_pcseti("cd",1);
    gset_line_colr_ind(1);
    c_plchhq(x0,y0+r+.05,"Marker 1 (dots)",.018,0.,0.);
/*
 * Marker 2, plus (make a plus from the plus markers.)
 *
 * Center of big plus.
 */
    x0 = .83;
    y0 = .5;
    r  = .1;
    jl = 7;
    ainc = r/(float)(jl);
    for( j = -jl; j <= jl; j++ ) {
        y = y0+(float)(j)*ainc;
        idx = j+jl+1;
        xm1[idx-1] = x0;
        ym1[idx-1] = y;
        x = x0+(float)(j)*ainc;
        xm2[idx-1] = x;
        ym2[idx-1] = y0;
    }
    gset_marker_type(2);
    gset_marker_colr_ind(1);
/*
 * Put plus markers along the two axes of the big plus.
 */
    markers.num_points = 2*jl+1;
    for( j = 0; j < markers.num_points; j++ ) {
        markers.points[j].x = xm1[j];
        markers.points[j].y = ym1[j];
    }
    gpolymarker(&markers);
    for( j = 0; j < markers.num_points; j++ ) {
        markers.points[j].x = xm2[j];
        markers.points[j].y = ym2[j];
    }
    gpolymarker(&markers);
/*
 * Label the big plus.
 */
    c_pcseti("cd",1);
    gset_line_colr_ind(1);
    c_plchhq(x0,y0+r+.05,"Marker 2 (plus signs)",.018,0.,0.);
/*
 * Marker 3, asterisk (make an asterisk from the asterisk markers.)
 */
    x0 = .7;
    y0 = .15;
    r  = .1;
    jl = 5;
    ainc = r/(float)(jl);
    for( j = -jl; j <= jl; j++ ) {
        y = y0+(float)(j)*ainc;
        idx = j+jl+1;
        xm1[idx-1] = x0;
        ym1[idx-1] = y;
        p = 0.5*sqrt(2.)*(y-y0);
        if (y >= 0.) {
            xm2[idx-1] = x0+p;
            ym2[idx-1] = y0+p;
            xm3[idx-1] = x0-p;
            ym3[idx-1] = y0+p;
        }
        else {
            xm2[idx-1] = x0-p;
            ym2[idx-1] = y0-p;
            xm3[idx-1] = x0+p;
            ym3[idx-1] = y0-p;
        }
    }
    gset_marker_type(3);
    gset_marker_colr_ind(1);
/*
 * Put asterisk markers along the axes of the big asterisk.
 */
    markers.num_points = 2*jl+1;
    for( j = 0; j < markers.num_points; j++ ) {
        markers.points[j].x = xm1[j];
        markers.points[j].y = ym1[j];
    }
    gpolymarker(&markers);
    for( j = 0; j < markers.num_points; j++ ) {
        markers.points[j].x = xm2[j];
        markers.points[j].y = ym2[j];
    }
    gpolymarker(&markers);
    for( j = 0; j < markers.num_points; j++ ) {
        markers.points[j].x = xm3[j];
        markers.points[j].y = ym3[j];
    }
    gpolymarker(&markers);
/*
 * Label the big asterisk.
 */
    c_pcseti("cd",1);
    gset_line_colr_ind(1);
    c_plchhq(x0,y0+r+.05,"Marker 3 (asterisks)",.018,0.,0.);
/*
 * Marker 4, circle (make a big circle from the circle markers.)
 */
    x0 = .3;
    y0 = .15;
    r  = .1;
    jl = 50;
    radinc = 2.*3.1415926/(float)(jl);
    for( j = 1; j <= jl; j++ ) {
        x = x0+r*cos((float)(j)*radinc);
        y = y0+r*sin((float)(j)*radinc);
        xm1[j-1] = x;
        ym1[j-1] = y;
    }
    gset_marker_type(4);
    gset_marker_colr_ind(1);
    gset_marker_size(2.);
    markers.num_points = jl;
    for( j = 0; j < markers.num_points; j++ ) {
        markers.points[j].x = xm1[j];
        markers.points[j].y = ym1[j];
    }
    gpolymarker(&markers);
/*
 * Label the big circle.
 */
    c_pcseti("cd",1);
    gset_line_colr_ind(1);
    c_plchhq(x0,y0+r+.05,"Marker 4 (circles)",.018,0.,0.);
/*
 * Marker 5, cross (make a big cross from the cross markers.)
 */
    x0 = .17;
    y0 = .5;
    r  = .1;
    jl = 5;
    ainc = r/(float)(jl);
    for( j = -jl; j <= jl; j++ ) {
        y = y0+(float)(j)*ainc;
        idx = j+jl+1;
        p = 0.5*sqrt(2.)*(y-y0);
        if (y >= 0.) {
            xm2[idx-1] = x0+p;
            ym2[idx-1] = y0+p;
            xm3[idx-1] = x0-p;
            ym3[idx-1] = y0+p;
        }
        else {
            xm2[idx-1] = x0-p;
            ym2[idx-1] = y0-p;
            xm3[idx-1] = x0+p;
            ym3[idx-1] = y0-p;
        }
    }
    gset_marker_type(5);
    gset_marker_colr_ind(1);
/*
 * Plot cross markers along the axes of the big cross.
 */
    gset_marker_size(1.);
    markers.num_points = 2*jl+1;
    for( j = 0; j < markers.num_points; j++ ) {
        markers.points[j].x = xm2[j];
        markers.points[j].y = ym2[j];
    }
    gpolymarker(&markers);
    for( j = 0; j < markers.num_points; j++ ) {
        markers.points[j].x = xm3[j];
        markers.points[j].y = ym3[j];
    }
    gpolymarker(&markers);
/*
 * Label the big cross.
 */
    c_pcseti("cd",1);
    gset_line_colr_ind(1);
    c_plchhq(x0,y0+r+.05,"Marker 5 (crosses)",.018,0.,0.);
/*
 * Draw a big circle in the center by applying a large marker size
 * scale factor to the circle marker.
 */
    x0 = .5;
    y0 = .46;
    gset_marker_type(4);
    gset_marker_colr_ind(1);
    gset_marker_size(15.);
    markers.num_points = 1;
    markers.points[0].x = x0;
    markers.points[0].y = y0;
    gpolymarker(&markers);
    c_pcseti("cd",1);
    gset_line_colr_ind(1);
    c_plchhq(x0,y0+.028,"Circle",.015,0.,0.);
    c_plchhq(x0,y0     ,"scaled",.015,0.,0.);
    c_plchhq(x0,y0-.028,"by 15.",.015,0.,0.);
/*
 * Label the plot (plotchar strokes its characters with lines, so the
 * plotchar character attributes are controlled by the gks polyline 
 * attributes).
 */
    gset_line_colr_ind(1);
    c_pcseti("cd",1);
    gset_linewidth(2.);
    c_plchhq(.5,.915,"Polymarkers",.025,0.,0.);

    c_frame();
	free(markers.points);
/*
 * Deactivate and close the workstation, close gks.
 */
    gdeactivate_ws(WKID);
    gclose_ws(WKID);
    gclose_gks();
}
