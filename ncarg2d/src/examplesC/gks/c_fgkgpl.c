/*
 *	$Id: c_fgkgpl.c,v 1.1 1994-07-27 15:55:12 haley Exp $
 */
#include <stdio.h>
#include <math.h>

#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define WSTYPE SED_WSTYPE
#define WKID   1

main()
{
	extern void gplxpl();
/*
 *  Open GKS, open and activate a workstation.
 */
	gopen_gks ("stdout",0);
	gopen_ws (WKID, NULL, WSTYPE);
	gactivate_ws(WKID);
/* 
 * Invoke demo driver
 */
	gplxpl();
/*
 * Deactivate and close workstation, close GKS.
 */
	gdeactivate_ws (WKID);
	gclose_ws (WKID);
	gclose_gks();
}

void gplxpl()
{
	Gcolr_rep rgb;
	float plim1[2], plim2[2], plim3[2], plim4[2];
	int ing;
	float rad, ang;
/*
 * PURPOSE                To provide a simple demonstration of the
 *                        GKS line drawing techniques.
 *
 * USAGE                  CALL GPLXPL (IWKID)
 *
 * Coordinate arrays
 */
	Gpoint_list line;
/*
 * Declare the constant for converting from degrees to radians.
 */
	float dtr = .017453292519943;

    line.num_points = 1500;
    line.points = (Gpoint *)malloc(line.num_points*sizeof(Gpoint));
    if( !line.points ) {
		fprintf( stderr, "gplxpl: Not enough memory to create polyline structure\n" );
		gemergency_close_gks();
		exit(1);
	}
/*
 * Set up a color table
 *
 * White background
 */
	rgb.rgb.red = 1.; rgb.rgb.green = 1.; rgb.rgb.blue = 1.;
	gset_colr_rep (WKID,0,&rgb);
/*
 * Black foreground
 */
	rgb.rgb.red = 0.; rgb.rgb.green = 0.; rgb.rgb.blue = 0.;
	gset_colr_rep (WKID,1,&rgb);
/*
 * Red
 */
	rgb.rgb.red = 1.; rgb.rgb.green = 0.; rgb.rgb.blue = 0.;
	gset_colr_rep (WKID,2,&rgb);
/*
 * Green
 */
	rgb.rgb.red = 0.; rgb.rgb.green = 1.; rgb.rgb.blue = 0.;
	gset_colr_rep (WKID,3,&rgb);
/*
 * Blue
 */
	rgb.rgb.red = 0.; rgb.rgb.green = 0.; rgb.rgb.blue = 1.;
	gset_colr_rep (WKID,4,&rgb);
/*
 * Draw a world map
 *
 * Position the map on the plotter frame
 */
	c_mappos(.01,.99,.01,.99);
/*
 * Define a map projection
 */
	c_maproj("CE",0., -150., 0.);
/*
 * Choose map limits 
 */
	plim1[0] =  60.; plim1[1] = 0.;
	plim2[0] =  30.; plim2[1] = 0.;
	plim3[0] = -60.; plim3[1] = 0.;
	plim4[0] = -30.; plim4[1] = 0.;
	c_mapset("CO", plim1,plim2,plim3,plim4);
/*
 * Choose continental outlines
 */
	c_mapstc ("OU", "CO");
/*
 * Turn off grid lines
 */
	c_mapstr ("GR",0.)      ;
/*
 * Draw it
 */
	c_mapdrw();
/*
 * Create a spiral curve
 *
 *  Set the line color to red
 */
	gset_line_colr_ind (2);
	for( ing=1; ing <= 1500; ing++ ) {
		rad=(float)(ing)/1000.;
		ang=dtr*(float)(ing-1);
		line.points[ing-1].x=.25+.5*rad*cos(ang);
		line.points[ing-1].y=.25+.5*rad*sin(ang);
	}
/*
 * Draw 3 spirals on the map over areas of high hurricane
 * probability.  Draw 2 additional spirals at the bottom
 * of the plot to use as a key.  Dashed spirals indicate
 * relatively low areas of hurricane probability, while solid
 * spirals indicate higher probability.
 *
 * Set the line type to solid (the default)
 */
	gset_linetype(1);
/*
 * Set the position of the spiral
 */
	c_set (.24,.37,.48,.61,-1.,1.,-1.,1.,1);
/*
 * Draw the line
 */
	gpolyline(&line);
/*
 * Set the position of the spiral
 */
	c_set (.03,.10,.43,.50,-1.,1.,-1.,1.,1);
/*
 *  Draw the line
 */
	gpolyline(&line);
/*
 * Set the position of the spiral
 */
	c_set (.62,.75,.47,.60,-1.,1.,-1.,1.,1);
/*
 * Draw the line
 */
	gpolyline(&line);
/*
 * Set the position of the spiral
 */
	c_set (.25,.38,.10,.23,-1.,1.,-1.,1.,1);
/*
 * Draw the line
 */
	gpolyline(&line);
/*
 * Set the position of the spiral
 */
	c_set (.65,.72,.10,.17,-1.,1.,-1.,1.,1);
/*
 * Draw the line
 */
	gpolyline(&line);
/*
 * Reset the plot window
 */
	c_set(0.,1.,0.,1.,0.,1.,0.,1.,1);
/*
 * Set the line color to black
 */
	gset_line_colr_ind (1);
/*
 *  Create a background perimeter 
 */
	c_frstpt( 0.0, 0.0);
	c_vector( 1.0, 0.0);
	c_vector( 1.0, 1.0);
	c_vector( 0.0, 1.0);
	c_vector( 0.0, 0.0);
/*
 *  Label the plot
 */
	c_plchlq(0.5,0.80,"Areas of High Hurricane Probability",25.,0.,0.);

	c_plchlq(0.5,0.25,"Average number of tropical cyclones per 5 degree square per year",15.,0.,0.);

	c_plchlq(0.33,0.10,"> 3",15.,0.,0.);

	c_plchlq(0.70,0.10,"2<n<3",15.,0.,0.);

	c_frame();
	return;
}
