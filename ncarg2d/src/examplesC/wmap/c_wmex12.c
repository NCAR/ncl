#include <stdio.h>
#include <math.h>

/*
 * Function prototypes for NCARG and NCARG-GKS routines
 */
#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define WSTYPE SED_WSTYPE
#define WKID   1

#define NUMC   27

main()
{
	int i, ieodf;
	float xo, yo, p1[2], p2[2], p3[2], p4[2];
	Gcolr_rep rgb;
/*
 *  Example of using station model data.
 *
 *  City names and locations, Station model data.
 *
 *   NUMC    - the number of cities.
 *   cityux  - X user coordinates for city locations.
 *   cityuy  - Y user coordinates for city locations.
 */
	float cityux[NUMC] = {40.0, 47.6, 37.8, 34.1, 45.8, 31.8, 29.8, 
                          39.1, 45.0, 41.9, 42.3, 33.8, 25.8, 40.8,
                          44.1, 43.6, 40.7, 33.5, 35.1, 46.7, 36.0,
                          32.8, 34.7, 38.1, 35.2, 36.8, 44.8};

	float cityuy[NUMC] = {-105.0, -122.3, -122.4, -118.3, -108.5, -106.5,
                          -095.3, -094.1, -093.8, -087.6, -083.1, -084.4,
                          -080.2, -074.0, -123.1, -116.2, -111.9, -112.1,
                          -106.6, -100.8, -096.0, -096.8, -092.3, -084.1,
                          -080.8, -076.3, -068.8};

/*
 *  Data on city locations and daily temperature labels.
 */
    char  imdat[NUMC][10][5] = {
      "11000","00000","11260","21360","30000",       
      "49550","54054","60000","77570","87712",
      "11103","11101","11040","21080","30000",
      "49590","55050","60051","70430","80369",
      "11206","21003","11020","21040","30000",
      "49630","56046","60151","70840","81470",
      "11309","31106","10000","21020","30000",
      "49670","57042","60201","71250","82581",
      "11412","41209","10020","21010","30000",
      "49710","58038","60251","71660","83592",
      "11515","51312","10040","20000","30000",
      "49750","50034","60301","72070","84703",
      "11618","61415","10060","20030","30000",
      "49790","51030","60350","72480","85814",
      "11721","71518","10080","20050","30000",
      "49830","52026","60400","72890","86925",
      "11824","81621","10090","20070","30000",
      "49870","53022","60450","73230","87036",
      "11927","91724","10110","20110","30000",
      "49910","54018","60501","73640","88147",
      "11030","01827","10130","20130","30000",
      "49950","55014","60551","74050","89258",
      "11133","11930","10150","20170","30000",
      "49990","56010","60601","74460","80369",
      "11236","22033","10170","20200","30000",
      "40000","57006","60651","74870","81470",
      "11339","32136","10190","20230","30000",
      "40040","58002","60701","75280","82581",
      "11442","42239","10210","20250","30000",
      "40080","50000","60751","75690","83692",
      "11545","52342","10230","20270","30000",
      "40120","51040","60801","76030","84703",
      "11648","62445","10250","20290","30000",
      "40170","52008","60851","76440","85814",
      "11751","72548","10270","20310","30000",
      "40210","53012","60901","76850","86925",
      "11854","82651","10290","20330","30000",
      "40250","54016","60950","77260","87036",
      "11958","92754","10310","20360","30000",
      "40290","55018","61000","77670","88147",
      "11060","02857","10330","20380","30000",
      "40330","56030","61050","78080","89258",
      "11163","12960","10350","20410","30000",
      "40370","57034","61100","78490","80369",
      "11266","23063","10370","20430","30000",
      "40410","58043","61150","78830","81470",
      "11369","33166","10390","20470","30000",
      "40450","50041","61200","79240","82581",
      "11472","43269","10410","20500","30000",
      "40480","51025","61250","79650","83692",
      "11575","51172","10430","20530","30000",
      "40510","52022","61350","79960","84703",
      "11678","63475","10480","21580","30000",
      "40550","53013","61400","73370","85814"};
/*
 * Open GKS.
 */
    gopen_gks ("stdout",0);
/*
 *  Calls to position the output (applicable only to PostScript output).
 */
	c_ngseti("LX",-90);
	c_ngseti("UX",710);
	c_ngseti("LY",-15);
	c_ngseti("UY",785);
    gopen_ws (WKID, NULL, WSTYPE);
    gactivate_ws(WKID);

	rgb.rgb.red = 1.; rgb.rgb.green = 1.; rgb.rgb.blue = 1.;
	gset_colr_rep(WKID,0,&rgb);
	rgb.rgb.red = 0.; rgb.rgb.green = 0.; rgb.rgb.blue = 0.;
	gset_colr_rep(WKID,1,&rgb);
	rgb.rgb.red = 1.; rgb.rgb.green = 0.; rgb.rgb.blue = 0.;
	gset_colr_rep(WKID,2,&rgb);
	rgb.rgb.red = 0.; rgb.rgb.green = 0.; rgb.rgb.blue = 1.;
	gset_colr_rep(WKID,3,&rgb);
	rgb.rgb.red = .85; rgb.rgb.green = .85; rgb.rgb.blue = .85;
	gset_colr_rep(WKID,4,&rgb);
	rgb.rgb.red = .60; rgb.rgb.green = .60; rgb.rgb.blue = .60;
	gset_colr_rep(WKID,5,&rgb);

	gset_fill_int_style(GSTYLE_SOLID);
	gset_fill_colr_ind(2);

    c_pcseti("CC",1);
    c_plchhq(0.5,0.82,":F21:Sample Plotted Station Model Data", 0.02,0.,0.);
    c_plchhq(0.5,0.77,":F21:(Data are not from actual readings)", 0.02,0.,0.);
/*
 *  Set up the parameters for drawing the U.S. state outlines.
 *
 *   Position the plot.
 */
    c_mappos(0.05, 0.95, 0.00, 0.90);
/*
 *   Specify U.S. state outlines.
 */
    c_mapstc("OU","US");
/*
 *   Choose Lambert conformal projection with two standard parallels.
 */
    c_maproj("LC",30.,-100.,45.);
/*
 *   Reduce the value of "MV" to make what MAPDRW produces match what
 *   comes out of MAPBLA/ARSCAM.
 */
    c_mapsti ("MV",1);
/*
 *   Specify the corner points of the plot as lat/lon pairs.
 */
	p1[0] = 22.6;
	p2[0] = -120.0;
	p3[0] = 46.9;
	p4[0] = -64.2;
    c_mapset("CO",p1,p2,p3,p4);
/*
 *   Initialize the transformations.
 */
    c_mapint();
/*
 *  Set the flag for MAPEOD to just consider boundary lines.
 */
    ieodf = 1;
    gset_line_colr_ind(5);
    c_maplot();
/*  
 *--------------------------------------
 *  Station models at selected cities  |
 *--------------------------------------
 *
 *   NUMC    - the number of cities.
 *   IDLYTS  - Daily hi/low labels for cities.
 *   cityux  - X user coordinates for city locations.
 *   cityuy  - Y user coordinates for city locations.
 */
    for( i = 1; i <= NUMC; i++ ) {
        c_wmsetr("WBS - Wind barb size",0.035);
        if (i == 10 || i == 21 || i == 22 || i == 23 || i == 25) {
            c_wmsetr("wbs - wind barb size",0.028);
        }
        c_maptrn(cityux[i-1],cityuy[i-1],&xo,&yo);
        c_wmstnm(xo,yo,imdat[i-1][0]);
	}

	c_frame();

	gdeactivate_ws(WKID);
	gclose_ws(WKID);
	gclose_gks();
}

