/*
 *	$Id: c_class2.c,v 1.1 1994-08-01 22:15:10 haley Exp $
 */
#include <stdio.h>
#include <math.h>

#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define WSTYPE SED_WSTYPE
#define WKID   1

#define NPTS   200
#define NCURVE  4

main()
{
	float ydra[NCURVE][NPTS],xdra[NPTS];
	char string[28];
	int i, j;
	extern void defclr();
/*
 * Generate some data
 */
	for( i = 1; i <= NPTS; i++ ) {
		xdra[i-1]=i*0.1;
		for( j = 1; j <= NCURVE; j++ ) {
            ydra[j-1][i-1] = sin(xdra[i-1]+0.2*j)*exp(-0.01*xdra[i-1]*j*j);
		}
	}
/*
 *  Open gks, open and activate a workstation.
 */
	gopen_gks ("stdout",0);
	gopen_ws (WKID, NULL, WSTYPE);
	gactivate_ws(WKID);
/*
 * Set up a color table
 */
	defclr();
/*
 * Label the axes
 */
	c_plchhq(.5,.05,"time (seconds)",.012,0.,0.);
	c_plchhq(.025,.5,"position (meters)",.012,90.,0.);
/*
 * Set the window for the range of the data and set the viewport to
 * protect the axis labels
 */
	c_set(.1,.9,.1,.9,0.0,20.0,-1.4,1.4,1);
/*
 * Set up tick mark labels
 */
	c_labmod("(i2)","(f4.1)",0,0,8,8,0,0,0);
/*
 * Draw axes and their labels
 */
	c_gridal(10,2,15,2,1,1,5,0.0,0.0);
/*
 * Draw each curve with a different label
 */
	for( i = 1; i <= NCURVE; i++ ) {
		sprintf( string,"'$$$$$$$$$$$$$$$$''curve''%d", i );
		c_dashdc(string,1,1);
		gset_line_colr_ind(i+1);
		gset_text_colr_ind(i+1);
		c_curved(xdra,&ydra[i-1][0],NPTS);
	}
/*
 * Close the frame
 */
	c_frame();
/*
 * Deactivate and close workstation, close gks.
 */
	gdeactivate_ws(WKID);
	gclose_ws(WKID);
	gclose_gks();
}

void defclr()
{
	Gcolr_rep rgb;
/*
 * Define a color table
 *
 * Background color is black
 */
	rgb.rgb.red = 0.0; rgb.rgb.green = 0.0; rgb.rgb.blue = 0.0;
	gset_colr_rep(WKID, 0, &rgb);
/*
 * Default foreground color is white
 */
	rgb.rgb.red = 1.0; rgb.rgb.green = 1.0; rgb.rgb.blue = 1.0;
	gset_colr_rep(WKID, 1, &rgb);
/*
 * red
 */
	rgb.rgb.red = 1.0; rgb.rgb.green = 0.0; rgb.rgb.blue = 0.0;
	gset_colr_rep(WKID, 2, &rgb);
/*
 * green
 */
	rgb.rgb.red = 0.0; rgb.rgb.green = 1.0; rgb.rgb.blue = 0.0;
	gset_colr_rep(WKID, 3, &rgb);
/*
 * blue 
 */
	rgb.rgb.red = 0.4; rgb.rgb.green = 0.7; rgb.rgb.blue = 0.9;
	gset_colr_rep(WKID, 4, &rgb);
/*
 * magenta
 */
	rgb.rgb.red = 0.7; rgb.rgb.green = 0.4; rgb.rgb.blue = 0.7;
	gset_colr_rep(WKID, 5, &rgb);
/*
 * orange
 */
	rgb.rgb.red = 0.9; rgb.rgb.green = 0.7; rgb.rgb.blue = 0.4;
	gset_colr_rep(WKID, 6, &rgb);
/*
 * teal
 */
	rgb.rgb.red = 0.4; rgb.rgb.green = 0.9; rgb.rgb.blue = 0.7;
	gset_colr_rep(WKID, 7, &rgb);
	return;
}
