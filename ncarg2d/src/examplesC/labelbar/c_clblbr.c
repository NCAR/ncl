/*
 *	$Id: c_clblbr.c,v 1.2 1994-11-10 21:19:14 haley Exp $
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

char *lab1[11] = {".","here","go","can",":G:font","any","in","number","or","word","Any"};

char *lab2[13] = {".","boxes","between","lines","the","or","boxes","either","match","can","labels","that","Notice"};

main()
{
	extern void color();
	int ifill1[11] = {11,10,9,8,7,6,5,4,3,2,1};
	int ifill2[14] = {3,4,5,6,7,8,9,10,2,11,12,13,14,15};
/*
 * Open GKS, open and activate a workstation.
 */
	gopen_gks("stdout",0);
	gopen_ws(WKID, NULL, WSTYPE);
	gactivate_ws(WKID);
/*
 * Set up color table
 */
	color();
/*
 * Set color fill to be solid
 */
	gset_fill_int_style(GSTYLE_SOLID);
/*
 * Draw two vertical label bars.
 */
	c_sfsetr("AN",35.);
	c_sfseti("TY",-4);
	c_lblbar(1,.05,.45,.05,.95,11,.3,1.,ifill1,0,lab1,11,1);
	c_sfseti("ty",0);
	c_lblbar(1,.55,.95,.05,.95,14,.3,1.,ifill2,1,lab2,13,2);
/*
** Advance frame
*/
	c_frame();
/*
 * Deactivate and close workstation, close GKS.
 */
	gdeactivate_ws(WKID);
	gclose_ws(WKID);
	gclose_gks();
}

void color()
{
	Gcolr_rep rgb;
/*
 * Background color
 * Black
 */
	rgb.rgb.red = 0.; rgb.rgb.green = 0.; rgb.rgb.blue = 0.;
	gset_colr_rep(WKID,0,&rgb);
/*
 * Foreground colors
 * White
 */
	rgb.rgb.red = 1.0; rgb.rgb.green =  1.0; rgb.rgb.blue =  1.0;
	gset_colr_rep(WKID,  1,&rgb);
/*
 * Aqua
 */
	rgb.rgb.red = 0.0; rgb.rgb.green =  0.9; rgb.rgb.blue =  1.0;
	gset_colr_rep(WKID,  2,&rgb);
/*
 * Red
 */
	rgb.rgb.red = 0.9; rgb.rgb.green =  0.25; rgb.rgb.blue =  0.0;
	gset_colr_rep(WKID,  3,&rgb);
/*
 * OrangeRed
 */
	rgb.rgb.red = 1.0; rgb.rgb.green =  0.0; rgb.rgb.blue =  0.2;
	gset_colr_rep(WKID,  4,&rgb);
/*
 * Orange
 */
	rgb.rgb.red = 1.0; rgb.rgb.green =  0.65; rgb.rgb.blue =  0.0;
	gset_colr_rep(WKID,  5,&rgb);
/*
 * Yellow
 */
	rgb.rgb.red = 1.0; rgb.rgb.green =  1.0; rgb.rgb.blue =  0.0;
	gset_colr_rep(WKID,  6,&rgb);
/*
 * GreenYellow
 */
	rgb.rgb.red = 0.7; rgb.rgb.green =  1.0; rgb.rgb.blue =  0.2;
	gset_colr_rep(WKID,  7,&rgb);
/*
 * Chartreuse
 */
	rgb.rgb.red = 0.5; rgb.rgb.green =  1.0; rgb.rgb.blue =  0.0;
	gset_colr_rep(WKID,  8,&rgb);
/*
 * Celeste
 */
	rgb.rgb.red = 0.2; rgb.rgb.green =  1.0; rgb.rgb.blue =  0.5;
	gset_colr_rep(WKID,  9,&rgb);
/*
 * Green
 */
	rgb.rgb.red = 0.2; rgb.rgb.green =  0.8; rgb.rgb.blue =  0.2;
	gset_colr_rep(WKID, 10,&rgb);
/*
 * DeepSkyBlue
 */
	rgb.rgb.red = 0.0; rgb.rgb.green =  0.75; rgb.rgb.blue =  1.0;
	gset_colr_rep(WKID, 11,&rgb);
/*
 * RoyalBlue
 */
	rgb.rgb.red = 0.25; rgb.rgb.green =  0.45; rgb.rgb.blue =  0.95;
	gset_colr_rep(WKID, 12,&rgb);
/*
 * SlateBlue
 */
	rgb.rgb.red = 0.4; rgb.rgb.green =  0.35; rgb.rgb.blue =  0.8;
	gset_colr_rep(WKID, 13,&rgb);
/*
 * DarkViolet
 */
	rgb.rgb.red = 0.6; rgb.rgb.green =  0.0; rgb.rgb.blue =  0.8;
	gset_colr_rep(WKID, 14,&rgb);
/*
 * Orchid
 */
	rgb.rgb.red = 0.85; rgb.rgb.green =  0.45; rgb.rgb.blue =  0.8;
	gset_colr_rep(WKID, 15,&rgb);
/*
 * Lavender
 */
	rgb.rgb.red = 0.8; rgb.rgb.green =  0.8; rgb.rgb.blue =  1.0;
	gset_colr_rep(WKID, 16,&rgb);
/*
 * Gray
 */
	rgb.rgb.red = 0.7; rgb.rgb.green =  0.7; rgb.rgb.blue =  0.7;
	gset_colr_rep(WKID, 17,&rgb);
/*
 * Done.
 */
	return;
}

