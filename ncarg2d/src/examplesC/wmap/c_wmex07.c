#include <stdio.h>
#include <math.h>

/*
 * Function prototypes for NCARG and NCARG-GKS routines
 */
#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define WSTYPE SED_WSTYPE
#define WKID   1

#define XL1  0.20
#define XL2  0.50
#define XL3  0.80
#define Y1   0.83
#define Y2   0.62
#define Y3   0.40
#define Y4   0.20
#define YL1  0.74
#define YL2  0.52
#define YL3  0.32
#define YL4  0.07
#define SIZLAB  .02

main()
{
	int icold;
	Gcolr_rep rgb;
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
	rgb.rgb.red = 0.; rgb.rgb.green = 0.; rgb.rgb.blue = 1.;
	gset_colr_rep(WKID,2,&rgb);
	rgb.rgb.red = 0.; rgb.rgb.green = 1.; rgb.rgb.blue = 0.;
	gset_colr_rep(WKID,3,&rgb);
	rgb.rgb.red = 1.; rgb.rgb.green = 0.; rgb.rgb.blue = 0.;
	gset_colr_rep(WKID,4,&rgb);
	rgb.rgb.red = 1.; rgb.rgb.green = 1.; rgb.rgb.blue = 0.;
	gset_colr_rep(WKID,5,&rgb);
	rgb.rgb.red = 1.; rgb.rgb.green = 0.65; rgb.rgb.blue = 0.;
	gset_colr_rep(WKID,6,&rgb);
	rgb.rgb.red = 0.85; rgb.rgb.green = 0.85; rgb.rgb.blue = 0.85;
	gset_colr_rep(WKID,7,&rgb);
/*
 *  Plot title.
 */
    c_plchhq(0.50,0.95,":F26:Icons for daily weather",0.033,0.,0.);
/*
 *  Cloudy.
 */
	c_wmseti("CC1 - cloud interior color ",7);
	c_wmseti("CC2 - cloud outline color",1);
	c_wmseti("CC3 - cloud shadow color",2);
	c_wmsetr("SHT - size scale",0.013);
	c_wmlabs(XL1,Y2-0.025,"C");
	c_plchhq(XL1,YL2,":F22:Cloudy",SIZLAB,0.,0.);
/*
 *  Rain.
 */
	c_wmgeti("COL",&icold);
	c_wmseti("COL - rain color",1);
	c_wmsetr("SHT - size scale",0.013);
	c_wmlabs(XL2,Y2,"R");
	c_wmseti("COL - restore default color",icold);
	c_plchhq(XL2,YL2,":F22:Rain",SIZLAB,0.,0.);
/*
 *  T-storms.
 */
	c_wmseti("LC1 - color of interior of lightening bolt ",5);
	c_wmseti("LC2 - outline color of lightening bolt",1);
	c_wmseti("LC3 - shadow color for lightening bolt",1);
	c_wmsetr("SHT - size scale",0.013);
	c_wmlabs(XL2,Y4,"T");
	c_plchhq(XL2+.02,YL4,":F22:T-storms",SIZLAB,0.,0.);
/*
 *  Snow.
 */
	c_wmsetr("SHT - size scale",0.013);
	c_wmlabs(XL3,Y2,"SN");
	c_plchhq(XL3,YL2,":F22:Snow",SIZLAB,0.,0.);
/*
 *  Rain and snow.
 */
	c_wmsetr("SHT - size scale",0.013);
	c_wmlabs(XL3,Y3+0.01,"RS");
	c_plchhq(XL3,YL3,":F22:Rain and snow",SIZLAB,0.,0.);
/*
 *  Wind.
 */
	c_wmseti("COL",2);
	c_wmsetr("SHT - size scale",0.016);
	gset_linewidth(2.);
	c_wmlabs(XL2,Y1,"WIND");
	c_plchhq(XL2,YL1,":F22:Windy",SIZLAB,0.,0.);
	c_wmseti("COL",1);
	gset_linewidth(1.);
/*
 *  Mostly cloudy.
 */
	c_wmseti("SC1 - color of the sun center",5);
	c_wmseti("SC2 - color of the sun star points",6);
	c_wmseti("SC3 - color of the sun outlines",1);
	c_wmseti("SC4 - sun shadow color",1);
	c_wmsetr("SHT - size scale",0.013);
	c_wmlabs(XL1,Y3,"MC");
	c_plchhq(XL1,YL3,":F22:Mostly cloudy",SIZLAB,0.,0.);
/*
 *  Sunny.
 */
	c_wmsetr("SHT - size scale",0.013);
	c_wmlabs(XL1,Y1,"SU");
	c_plchhq(XL1,YL1,":F22:Sunny",SIZLAB,0.,0.);
/*
 *  Mostly sunny.
 */
	c_wmsetr("SHT - size scale",0.013);
	c_wmlabs(XL2,Y3,"MS");
	c_plchhq(XL2,YL3,":F22:Mostly sunny",SIZLAB,0.,0.);
/*
 *  Intermittent showers.
 */
	c_wmsetr("SHT - size scale",0.012);
	c_wmlabs(XL3,Y4+.01,"IS");
	c_plchhq(XL3+0.02,YL4+0.017,":F22:Intermittent",SIZLAB,0.,0.);
	c_plchhq(XL3+0.02,YL4-0.017,":F22:showers",SIZLAB,0.,0.);
/*
 *  Sun, possible T-storms.
 */
	c_wmsetr("SHT - size scale",0.012);
	c_wmlabs(XL1,Y4+.01,"IT");
	c_plchhq(XL1+0.02,YL4+0.017,":F22:Sun, possible",SIZLAB,0.,0.);
	c_plchhq(XL1+0.02,YL4-0.017,":F22:T-storms",SIZLAB,0.,0.);;
/*
 *  Ice.
 */
	c_wmsetr("SHT - size scale",0.013);
	c_wmlabs(XL3,Y1,"IC");
	c_plchhq(XL3,YL1,":F22:Ice",SIZLAB,0.,0.);

	c_frame();

	gdeactivate_ws (WKID);
	gclose_ws (WKID);
	gclose_gks();

}

