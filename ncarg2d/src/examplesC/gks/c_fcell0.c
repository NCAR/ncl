/*
 *	$Id: c_fcell0.c,v 1.1 1994-07-27 15:55:09 haley Exp $
 */
#include <stdio.h>
#include <math.h>

#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define WSTYPE SED_WSTYPE
#define WKID   1

#define IDX  2
#define IDY  3

char *label[IDX][IDY] = {"  Red  "," Green "," Blue  "," Cyan  ","Magenta", "Yellow "};

main()
{
/*
 *  Demonstration of the GKS CELL ARRAY entry.
 */
	int i, j, l;
	Grect rect;
	Gcolr_rep rgb;
	Gpat_rep colia;
	float dx, dy, xcent, ycent;
/*
 *  Define the X and Y extents for a rectangle.
 */
	float xl = .1, xr = .9, yb = .25, yt = .75;
/* 
 * Set up cell array structure
 */
    colia.colr_array = (Gint *)malloc(IDX*IDY*sizeof(Gint));
    colia.dims.size_x = IDX;
    colia.dims.size_y = IDY;
/*
 *  Open GKS, open and activate a workstation.
 */
	gopen_gks ("stdout",0);
	gopen_ws (WKID, NULL, WSTYPE);
	gactivate_ws(WKID);
/*
 *  Set up a color table and define the color index array.
 */
	rgb.rgb.red = 1.; rgb.rgb.green = 1.; rgb.rgb.blue = 1.;
	gset_colr_rep(WKID,0,&rgb);
	rgb.rgb.red = 1.; rgb.rgb.green = 1.; rgb.rgb.blue = 1.;
	gset_colr_rep(WKID,1,&rgb);
	rgb.rgb.red = 1.; rgb.rgb.green = 0.; rgb.rgb.blue = 0.;
	gset_colr_rep(WKID,2,&rgb);
	rgb.rgb.red = 0.; rgb.rgb.green = 1.; rgb.rgb.blue = 0.;
	gset_colr_rep(WKID,3,&rgb);
	rgb.rgb.red = 0.; rgb.rgb.green = 0.; rgb.rgb.blue = 1.;
	gset_colr_rep(WKID,4,&rgb);
	rgb.rgb.red = 0.; rgb.rgb.green = 1.; rgb.rgb.blue = 1.;
	gset_colr_rep(WKID,5,&rgb);
	rgb.rgb.red = 1.; rgb.rgb.green = 0.; rgb.rgb.blue = 1.;
	gset_colr_rep(WKID,6,&rgb);
	rgb.rgb.red = 1.; rgb.rgb.green = 1.; rgb.rgb.blue = 0.;
	gset_colr_rep(WKID,7,&rgb);

	l = 0;
	for( i = 1; i <= IDX; i++ ) {
		for( j = 1; j <= IDY; j++ ) {
			colia.colr_array[l++] = IDY*(i-1)+j+1;
		}
	}
/*
 *  Draw the cell array.
 */
	rect.p.x = .1;
	rect.p.y = .25;
	rect.q.x = .9;
	rect.q.y = .75;
	gcell_array (&rect,&colia);
/*
 *  Label the cells using PLOTCHAR font 26 with the foreground color.
 */
	dx = (xr-xl)/IDX;
	dy = (yt-yb)/IDY;
	c_pcseti("FN",26);
	c_pcseti("CC",1);
	for( i = 1; i <= IDX; i++ ) {
        xcent = xl+0.5*dx+(float)(i-1)*dx;
		for( j = 1; j <= IDY; j++ ) {
			ycent = yb+0.5*dy+(float)(j-1)*dy;
			c_plchhq(xcent,ycent,label[i-1][j-1],.033,0.,0.);
		}
	}
/*
 *  Terminate the picture.
 */
	 c_frame();
/*
 * Deactivate and close workstation, close GKS.
 */
	gdeactivate_ws (WKID);
	gclose_ws (WKID);
	gclose_gks();
}
