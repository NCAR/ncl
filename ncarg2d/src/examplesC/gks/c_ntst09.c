#include <stdio.h>
#include <math.h>

#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define ILD  121
#define IWTYPE 1


#define WSTYPE SED_WSTYPE
#define WKID   1

main()
{
/*
 *  Illustrate normalization transformations.
 */
	float scale, radc = .0174532;
	int i, j;
    float xl, xr, yb, yt;
    Gcolr_rep rgb[6];
    Gpoint_list polyline;
    Glimit vp_limits, win_limits;
	Gtext_font_prec text_font_prec;
	Gtext_align text_align;
	Gpoint point;
    extern void box(), dline();

    polyline.num_points = ILD;
    polyline.points = (Gpoint *)malloc(ILD*sizeof(Gpoint));
/*
 *  Open GKS, open and activate the workstation.
 */
	gopen_gks("stdout",0);
	gopen_ws( WKID, NULL, WSTYPE);
	gactivate_ws( WKID );
/*
 *  Define necessary color indices, color index 0 defines the 
 *  background color.
 */
	rgb[0].rgb.red = 0.;
    rgb[0].rgb.green = 0.;
    rgb[0].rgb.blue = 0.6;
	rgb[1].rgb.red = 1.;
    rgb[1].rgb.green = 1.;
    rgb[1].rgb.blue = 1.;
	rgb[2].rgb.red = 1.;
    rgb[2].rgb.green = 1.;
    rgb[2].rgb.blue = 0.;
	rgb[3].rgb.red = 1.;
    rgb[3].rgb.green = 1.;
    rgb[3].rgb.blue = 0.;
	rgb[4].rgb.red = 0.;
    rgb[4].rgb.green = 1.;
    rgb[4].rgb.blue = 0.;
	rgb[5].rgb.red = 0.;
    rgb[5].rgb.green = 1.;
    rgb[5].rgb.blue = 1.;
    for( i = 0; i <= 5; i++ ) {
        gset_colr_rep(WKID,i,&rgb[i]);
	}
/*
 *  Create the data for a spiral in the world coordinate square
 *  bounded by (-10.,-10.) and (10.,10.) .
 */
	j = 0;
	for( i = 0; i <= 720; i+=6 ) {
		scale = (float)i/75.;
		polyline.points[j].x = scale*cos((float)(i-1)*radc);
		polyline.points[j].y = scale*sin((float)(i-1)*radc);
		j++;
	}
/*
 *  Define a normalization transformation that does not preserve
 *  aspect ratio.  Draw the transformed spiral with a box bounding 
 *  the viewport.
 */
	xl = -10.;
	xr =  10.;
	yb = -10.;
	yt =  10.;
    vp_limits.x_min = .55;
    vp_limits.x_max = .95;
    vp_limits.y_min = .40;
    vp_limits.y_max = .65;
	gset_vp(1,&vp_limits);
    win_limits.x_min = xl;
    win_limits.x_max = xr;
    win_limits.y_min = yb;
    win_limits.y_max = yt;
	gset_win(1,&win_limits);
	gsel_norm_tran(1);
	gset_line_colr_ind(1);
	gpolyline(&polyline);
	box(xl,xr,yb,yt);
/*
 *  Draw an image representing the window to the left of the viewport.
 */
    vp_limits.x_min = .05;
    vp_limits.x_max = .45;
    vp_limits.y_min = .30;
    vp_limits.y_max = .70;
	gset_vp(1,&vp_limits);
    win_limits.x_min = xl;
    win_limits.x_max = xr;
    win_limits.y_min = yb;
    win_limits.y_max = yt;
	gset_win(1,&win_limits);
	gsel_norm_tran(1);
	gset_line_colr_ind(1);
	gpolyline(&polyline);
    box(xl,xr,yb,yt);
/*
 *  draw dashed lines between the outlines.
 */
	dline(.05,.30,.55,.40);
	dline(.45,.30,.95,.40);
	dline(.05,.70,.55,.65);
	dline(.45,.70,.95,.65);
/*
 *  Label the plot (PLOTCHAR draws lines to plot characters,
 *  so the text color is controlled by the GKS polyline color).
 */
	gset_line_colr_ind(3);
	c_pcseti("CD",1);
	gset_linewidth(2.);
	c_plchhq(.5,.83,"Normalization transformation",.025,0.,0.);
	gset_linewidth(1.);
	gset_line_colr_ind(5);
	c_plchhq(.07,.64,"Window",.015,0.,-1.);
	c_plchhq(.57,.62,"Viewport",.015,0.,-1.);
    text_font_prec.font = -9;
    text_font_prec.prec = GPREC_STROKE;
	gset_text_font_prec(&text_font_prec);
	gset_char_ht (.035);
	gset_text_colr_ind(2);
    text_align.hor = GHOR_CTR;
    text_align.vert = GVERT_HALF;
	gset_text_align(&text_align);
    point.x = .5;
    point.y = .22;
	gtext(&point,"Normalization transformation defined by");
	c_plchhq(.25,.15,"gset_win(1,-10., 10.,-10., 10.)",.015,0.,-1.);
	c_plchhq(.25,.10,"gset_vp(1, .55, .95, .40, .65)",.015,0.,-1.);
	c_frame();
/*
 *  Deactivate and close the workstation, close GKS.
 */
	gdeactivate_ws(WKID);
	gclose_ws(WKID);
    gclose_gks();
}

void box(xl,xr,yb,yt)
float xl,xr,yb,yt;
{
/*
 *  Draw a box with corner points (XL,YB) and (XR,YT).
 */
    Gpoint_list line;

    line.num_points = 5;
    line.points = (Gpoint *)malloc(5*sizeof(Gpoint));
	line.points[0].x = xl;
	line.points[0].y = yb;
	line.points[1].x = xr;
	line.points[1].y = yb;
	line.points[2].x = xr;
	line.points[2].y = yt;
	line.points[3].x = xl;
	line.points[3].y = yt;
	line.points[4].x = line.points[0].x;
    line.points[4].y = line.points[0].y;

	gset_line_colr_ind(1);
    gpolyline(&line);
    return;
}

void dline (x1,y1,x2,y2)
float x1,y1,x2,y2;
{
    Gpoint_list line;

    line.num_points = 2;
    line.points = (Gpoint *)malloc(2*sizeof(Gpoint));
/*
 *  Draw a dashed line with color index 2 between the coordinates
 *  (X1,Y1) and (X2,Y2) .
 */
	gsel_norm_tran(0);
	gset_line_colr_ind(2);
	gset_line_ind(2);
	line.points[0].x = x1;
	line.points[0].y = y1;
	line.points[1].x = x2;
	line.points[1].y = y2;
	gpolyline(&line);
	gset_line_ind(1);
    return;
}

