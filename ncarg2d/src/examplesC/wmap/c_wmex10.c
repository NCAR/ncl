#include <stdio.h>
#include <math.h>

/*
 * Function prototypes for NCARG and NCARG-GKS routines
 */
#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define WSTYPE SED_WSTYPE
#define WKID   1

#define X1  0.18
#define X2  0.46
#define X3  0.74

main()
{
	Gcolr_rep rgb;
	float wbao, wbdo, wbs, wbc;
/*
 *  Example of windbarbs at various angles and a chart of wind speeds.
 *
 *  Example 01 - chart of windbarbs for various speeds.
 */
    gopen_gks ("stdout",0);
    gopen_ws (WKID, NULL, WSTYPE);
    gactivate_ws(WKID);

	rgb.rgb.red = 1.; rgb.rgb.green = 1.; rgb.rgb.blue = 1.;
	gset_colr_rep(WKID,0,&rgb);
	rgb.rgb.red = 0.; rgb.rgb.green = 0.; rgb.rgb.blue = 0.;
	gset_colr_rep(WKID,1,&rgb);
	rgb.rgb.red = 0.4; rgb.rgb.green = 0.; rgb.rgb.blue = 0.4;
	gset_colr_rep(WKID,2,&rgb);

/*
 *  Wind barbs at various angles using differing attributes.
 */
    c_plchhq(0.5,0.94,":F26:Wind barb examples",.04,0.,0.);
/*
 *  Set barb color and size.
 */
    c_wmseti("COL",2);
	c_wmsetr("WBS",0.25);
/*
 *  Draw first barb (all defaults with a wind speed of 71 knots).
 */
	c_wmbarb(X1,.5,26.,71.);
/*
 *  Second barb - change the angle of the tick marks and the spacing 
 *  between ticks and draw another barb.
 */
	c_wmgetr("WBA",&wbao);
	c_wmgetr("WBD",&wbdo);
	c_wmsetr("WBA",42.);
	c_wmsetr("WBD",.17);
	c_wmbarb(X2,.5,0.,75.);
	c_wmsetr("WBA",wbao);
	c_wmsetr("WBD",wbdo);
/*
 *  Third barb - draw a sky cover symbol at base of the barb (these 
 *  are drawn automatically when using WMSTNM to plot station model data).
 */
	c_wmgetr("WBS",&wbs);
	c_wmgetr("WBC",&wbc);
	c_wmseti("WBF",1);
	c_wmbarb(X3,.5,-26.,71.);
	c_ngwsym("N",0,X3,.5,wbs*wbc,2,0);
/*
 *  Fourth barb - change the size of the barb and the size of the sky 
 *  cover symbol.
 */
	c_wmsetr("WBS",0.20);
	c_wmsetr("WBC",0.15);
	c_wmbarb(X1+0.1,0.1,-26.,71.);
	c_ngwsym("N",0,X1+0.1,.1,0.20*0.15,2,0);
/*
 *  Fifth barb - reset original values for parameters, change wind speed
 *               to 45 knots.
 */
	c_wmsetr("WBS",wbs);
	c_wmsetr("WBC",wbc);
	c_wmseti("WBF",0);
	c_wmbarb(X2,.1,0.,45.);
/*
 *  Sixth barb - change the tick lengths.
 */
	c_wmsetr("WBT",.6);
	c_wmbarb(X3-0.1,.1,15.4,42.2);

	c_frame();

	gdeactivate_ws (WKID);
	gclose_ws (WKID);
	gclose_gks();

}

