/*
 * $Id: c_fspponts.c,v 1.2 1994-06-21 15:01:31 haley Exp $
 */

#include <stdio.h>
#include <math.h>

#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define NPTS   20

#define WSTYPE SED_WSTYPE
#define WKID   1

main()
{
/*
 * Create a line plot with connected markers using points.
 */
	float xdra[20],ydra[20];
	float x2dra[20],y2dra[20];
	float x3dra[20],y3dra[20];
	int i;
	extern void dfclrs(), bndary();
/*
 * Initialize GKS.
 */
	gopen_gks("stdout",0);
	gopen_ws(WKID, NULL, WSTYPE);
	gactivate_ws(WKID);
/*
 * Set up a color table
 */
	dfclrs();
/*
 * Fill the data arrays.
 */
	for( i = 0; i < NPTS; i++ ) {
        xdra[i]=sin((float)i);
        ydra[i]=cos((float)i);
        x2dra[i]=xdra[i] * .66;
        y2dra[i]=ydra[i] * .66;
        x3dra[i]=xdra[i] * .3;
        y3dra[i]=ydra[i] * .3;
	}
/*
 * Draw a boundary around the edge of the plotter frame.
 */
	bndary();
/*
 * Suppress the frame advance.
 */
	c_agseti ("FRAME.",2);
/*
 * Suppress the drawing of curves by the EZ... routines.
 */
	c_agseti ("SET.",-1);
/*
 * Draw the background, using EZXY.
 */
	c_ezxy (xdra,ydra,NPTS,"POINTS EXAMPLE$");
	c_sflush();
/*
 * Increase the size of the polymarkers
 */
	gset_marker_size(2.0);
/*
 * Set the color of the polymarker
 */
	gset_line_colr_ind(7);
/*
 * Set the line width of the lines connecting the polymarkers.
 */
	gset_linewidth(3.0);
/*
 * Put a plus sign at each of the x-y positions.
 */
	c_points (xdra,ydra,NPTS,-2,1);
/*
 * Set the color of the polymarker
 */
	gset_line_colr_ind(9);
/*
 * Put a circle at each of the x2-y2 positions.
 */
	c_points (x2dra,y2dra,NPTS,-4,1);
/*
 * Put an asterix at each of the x3-y3 positions,
 * and do not connect with lines.
 */
	c_points (x3dra,y3dra,NPTS,-3,0);
/*
 * Advance the frame.
 */
	c_frame();
/*
 * Close GKS.
 */
	gdeactivate_ws(WKID);
	gclose_ws(WKID);
	gclose_gks();

}

void bndary()
{
/*
 * Routine to draw the plotter-frame edge.
 */
	c_plotit (    0,    0,0);
	c_plotit (32767,    0,1);
	c_plotit (32767,32767,1);
	c_plotit (    0,32767,1);
	c_plotit (    0,    0,1);
	return;
}

void dfclrs()
{
	int i;
/*
 * Define a set of RGB color triples for colors 1 through 15.
 */
	Gcolr_rep rgb[16];
/*
 * Define the RGB color triples needed below.
 */
	rgb[0].rgb.red = 1.00;  rgb[0].rgb.green = 1.00;  rgb[0].rgb.blue = 1.00;
	rgb[1].rgb.red = 0.00;  rgb[1].rgb.green = 0.00;  rgb[1].rgb.blue = 0.00;
	rgb[2].rgb.red = 0.70;  rgb[2].rgb.green = 0.70;  rgb[2].rgb.blue = 0.70;
	rgb[3].rgb.red = 0.75;  rgb[3].rgb.green = 0.50;  rgb[3].rgb.blue = 1.00;
	rgb[4].rgb.red = 0.50;  rgb[4].rgb.green = 0.00;  rgb[4].rgb.blue = 1.00;
	rgb[5].rgb.red = 0.00;  rgb[5].rgb.green = 0.00;  rgb[5].rgb.blue = 1.00;
	rgb[6].rgb.red = 0.00;  rgb[6].rgb.green = 0.50;  rgb[6].rgb.blue = 1.00;
	rgb[7].rgb.red = 0.00;  rgb[7].rgb.green = 1.00;  rgb[7].rgb.blue = 1.00;
	rgb[8].rgb.red = 0.00;  rgb[8].rgb.green = 1.00;  rgb[8].rgb.blue = 0.60;
	rgb[9].rgb.red = 0.00;  rgb[9].rgb.green = 1.00;  rgb[9].rgb.blue = 0.00;
	rgb[10].rgb.red = 0.70; rgb[10].rgb.green = 1.00; rgb[10].rgb.blue = 0.00;
	rgb[11].rgb.red = 1.00; rgb[11].rgb.green = 1.00; rgb[11].rgb.blue = 0.00;
	rgb[12].rgb.red = 1.00; rgb[12].rgb.green = 0.75; rgb[12].rgb.blue = 0.00;
	rgb[13].rgb.red = 1.00; rgb[13].rgb.green = 0.38; rgb[13].rgb.blue = 0.38;
	rgb[14].rgb.red = 1.00; rgb[14].rgb.green = 0.00; rgb[14].rgb.blue = 0.38;
	rgb[15].rgb.red = 1.00; rgb[15].rgb.green = 0.00; rgb[15].rgb.blue = 0.00;
/*
 * Define 16 different color indices, for indices 0 through 15.  The
 * color corresponding to index 0 is black and the color corresponding
 * to index 1 is white.
 */
	for( i = 0; i <= 15; i++ ) {
		gset_colr_rep(WKID,i,&rgb[i]);
	}
/*
 * Done.
 */
	return;
}
