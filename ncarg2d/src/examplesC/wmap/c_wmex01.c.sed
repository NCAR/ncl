#include <stdio.h>
#include <math.h>

/*
 * Function prototypes for NCARG and NCARG-GKS routines
 */
#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define WSTYPE SED_WSTYPE
#define WKID   1
#define ICDIM  6000
#define NUMF   4
#define NUMFP  6
#define NUMR   13
#define NUMRP   35
#define NUMWL   5
#define NUML   5
#define NUMC   14
#define LAMA   200000

float xtmp[9999], ytmp[9999];
int ndum;

/*
 *  Front data.
 */
	float frnsux[NUMF][NUMFP] = {
		41.0,   42.5,   44.5,   49.0,   00.0,   00.0,
		49.0,   47.0,   45.0,   42.0,   38.8,   38.5,
		38.5,   39.0,   37.5,   00.0,   00.0,   00.0,
		38.5,   43.0,   49.0,   00.0,   00.0,   00.0};
	
	float frnsuy[NUMF][NUMFP] = {
		-121.5, -111.0, -104.0, -098.0,   00.0,   00.0,
		-098.0, -096.0, -095.0, -093.0, -088.3, -080.5,
		-080.5, -077.0, -071.0,   00.0,   00.0,   00.0,
		-080.5, -077.5, -077.0,   00.0,   00.0,   00.0};
/*
 *  Data on city locations and daily temperature labels.
 */
	float cityux[NUMC] = {40.0, 47.6, 37.8, 34.1, 45.8, 31.8, 29.8, 
						  39.1, 45.0, 41.9, 42.3, 33.8, 25.8, 40.8};
	float cityuy[NUMC] = {-105.0, -122.3, -122.4, -118.3, -108.5, -106.5,
                          -095.3, -094.6, -093.8, -087.6, -083.1, -084.4,
                          -080.2, -074.0};
	float tempux[NUMC] = {38.8, 46.9, 38.6, 35.0, 46.0, 32.2, 28.8, 
						  37.7, 46.0, 40.7, 43.7, 32.4, 27.2, 40.5};
	float tempuy[NUMC] = {-103.2, -119.6, -120.3, -116.0, -105.6, -104.2,
                          -097.4, -092.4, -091.2, -085.2, -082.8, -084.6,
                          -081.3, -071.5};
	char icitys[NUMC][14] = {
		"NCAR", "Seattle", "San Francisco", "Los Angeles", "Billings",
        "El Paso", "Houston", "Kansas City", "Minneapolis", "Chicago",
        "Detroit", "Atlanta", "Miami",   "New York"};
	char idlyts[NUMC][6] = {"92/58", "80/58", "68/54", "80/64", "91/58",
                            "96/70", "98/76", "88/70", "84/67", "84/66",
                            "84/63", "90/72", "92/80", "83/70"};
    float rgnsux[NUMR][NUMRP] = {
          32.7,   34.5,   38.0,   42.0,   45.0,   47.5,   46.0,   
          43.0,   39.5,   38.3,   40.0,   37.5,   35.5,   32.7,   
          36.0,   41.0,   42.0,   44.5,   47.0,   48.0,   46.0,   
          42.0,   39.0,   36.0,   37.3,   33.0,   37.5,   29.0,   
          24.0,   20.0,   29.0,   32.7,   00.0,   00.0,   00.0,
          35.0,   33.0,   31.5,   33.0,   35.0,   00.0,   00.0,   
          00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   
          00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   
          00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   
          00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
          35.0,   36.0,   38.0,   40.0,   43.0,   46.5,   48.5, 
          47.0,   44.0,   44.5,   46.0,   47.0,   49.0,   53.0, 
          48.0,   46.0,   42.8,   40.0,   37.0,   35.5,   35.0, 
          33.0,   28.0,   30.0,   32.7,   35.0,   00.0,   00.0,
          00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
          49.0,   45.5,   46.0,   50.0,   53.0,   51.5,   49.0, 
          00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
          00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
          00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
          00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
          45.0,   43.2,   41.5,   43.0,   47.0,   49.0,   45.0, 
          00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
          00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
          00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
          00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
          32.7,   35.0,   36.0,   35.3,   33.0,   31.3,   30.0, 
          32.7,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
          00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
          00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
          00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
          33.5,   35.5,   37.2,   38.5,   39.5,   39.5,   38.0,
          36.0,   34.5,   33.0,   30.0,   30.0,   31.0,   33.5,
          00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
          00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
          00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
          49.0,   50.5,   49.0,   47.0,   45.0,   45.0,   47.0,
          49.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
          00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
          00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
          00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
          39.0,   38.0,   35.3,   37.0,   39.0,   00.0,   00.0,
          00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
          00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
          00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
          00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
          50.0,   49.0,   48.0,   47.0,   46.7,   48.0,   50.0,
          50.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
          00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
          00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
          00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
          36.7,   35.0,   32.0,   31.0,   31.6,   32.2,   32.8,
          32.2,   32.2,   34.0,   36.0,   36.7,   00.0,   00.0,
          00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
          00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
          00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
          35.0,   36.5,   36.5,   37.7,   38.5,   41.0,   45.0,
          47.0,   49.0,   48.5,   47.0,   44.0,   42.8,   41.5,
          40.0,   38.0,   36.0,   34.5,   34.5,   35.0,   00.0,
          00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
          00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
          49.5,   49.0,   47.0,   44.0,   44.0,   47.0,   48.5,
          49.5,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
          00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
          00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
          00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0};
    float rgnsuy[NUMR][NUMRP] = {
        -116.0, -117.0, -120.5, -122.7, -122.0, -118.0, -115.3, 
        -113.0, -107.5, -109.0, -115.0, -114.0, -111.5, -109.0, 
        -106.0, -105.0, -104.0, -107.0, -109.7, -104.0, -098.0, 
        -095.5, -096.0, -093.0, -088.5, -083.0, -076.0, -075.0, 
        -080.0, -097.0, -115.0, -116.0,   00.0,   00.0,   00.0,
        -097.5, -096.5, -097.5, -098.5, -097.5,   00.0,  000.0, 
        -080.0, -097.0, -115.0, -116.0,   00.0,   00.0,   00.0,
          00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
          00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
          00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
        -118.7, -120.0, -121.5, -122.7, -123.1, -121.8, -118.0, 
        -114.5, -112.0, -110.5, -112.0, -113.2, -114.0, -122.0, 
        -123.5, -123.0, -123.5, -123.0, -121.5, -120.5, -120.9, 
        -122.0, -120.0, -116.0, -116.5, -118.7,   00.0,   00.0,
          00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
        -096.0, -090.5, -084.0, -082.0, -089.0, -095.0, -096.0, 
          00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
          00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
          00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
          00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
        -073.3, -072.5, -070.5, -066.0, -065.0, -071.0, -073.3, 
          00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
          00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
          00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
          00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
        -115.0, -116.0, -115.3, -114.0, -111.0, -111.0, -113.0, 
        -115.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
          00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
          00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
          00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
        -110.2, -111.5, -113.0, -111.5, -108.5, -106.3, -105.5,
        -106.0, -107.5, -108.0, -108.3, -109.6, -110.2, -110.2,
          00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
          00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
          00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
        -097.0, -094.0, -090.0, -089.3, -091.5, -093.5, -096.0,
        -097.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
          00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
          00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
          00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
        -120.0, -118.0, -116.5, -119.0, -120.0,   00.0,   00.0,
          00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
          00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
          00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
          00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
        -105.0, -105.0, -106.0, -108.0, -110.7, -110.0, -106.5,
        -105.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
          00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
          00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
          00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
        -076.0, -078.0, -081.5, -084.0, -088.0, -089.5, -088.0,
        -085.0, -083.0, -080.4, -078.0, -076.0,   00.0,   00.0,
          00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
          00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
          00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
        -094.0, -092.0, -089.0, -084.0, -079.0, -077.0, -076.0,
        -076.0, -075.7, -074.5, -073.3, -071.5, -070.3, -070.5,
        -074.0, -077.5, -081.0, -087.0, -092.0, -094.0,   00.0,
          00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
          00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
        -073.8, -071.0, -069.5, -069.0, -071.0, -073.0, -074.0,
        -073.8,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
          00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
          00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
          00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0};
main()
{
	int i, j, ic80s, ic90s, indxmx, ll, mm;
    extern void tstext(), wmtint();
    float xext, yext;
    int no;
	float xo, yo;
	char stmp1[256],stmp2[256];
	Gcolr_rep rgb;
    Gpoint_list fill_area;
/*
 *  An example of using the tools in the weather map library to
 *  create a grayscale weather map.
 *
 *  Set up all of the coordinate data for the fronts, temperature regions,
 *  cities, etc.
 */
	float usx[ICDIM],usy[ICDIM];
/*
 *  Weather front arrays.
 *
 *   NUMF    - the number of fronts.
 *   NUMFP   - the maximum number of control points for any front.
 *   LIMITF  - the defined number of control points for each region.
 *   FRNSUX  - X user coordinates for fronts.
 *   frnsuy  - Y user coordinates for fronts.
 */
	int limitf[NUMFP];
    float frnswx[NUMFP],frnswy[NUMFP];
/*
 *  Temperature and weather region arrays.
 *
 *   NUMR    - the number of regions
 *   NUMRP   - the maximum number of control points for any region.
 *   LIMITS  - the defined number of control points for each region.
 *   IRTYPE  - the type of region.
 *   RGNSUX  - X user coordinates for regions.
 *   RGNSUY  - Y user coordinates for regions.
 */
	char irtype[NUMR][7];
	int limits[NUMR];
	float rgnswx[NUMRP],rgnswy[NUMRP];
/*
 *  Regional weather labels.
 *
 *   NUMWL   - the number of labels.
 *   CNDSUX  - X user coordinates for labels.
 *   CNDSUY  - Y user coordinates for labels.
 *   ICNDSL  - Labels.
 */
	float cndsux[NUMWL] = {45.5,  41.5,  31.5,  33.0,  45.5};
	float cndsuy[NUMWL] = {-113.0,-099.0,-095.5,-113.2,-082.0};
	char *icndsl[NUMWL] = {"BREEZY","HOT","STEAMY","HUMID","NICE"};
/*
 *  LOW/HIGH arrays.
 *
 *   NUML    - the number of lows and highs to plot.
 *   frnsux  - X user coordinates for symbols.
 *   frnsuy  - Y user coordinates for symbols.
 *   LOWHI   - Array of Low/High flags (0=low; 1=hi).
 */
	float rlohux[NUML] = {49.0,  38.5,  47.5,  44.0,  26.5};
	float rlohuy[NUML] = {-099.0,-080.5,-127.0,-089.0,-088.0};
	int lowhi[NUML] = {0, 0, 1, 1, 1};
/*
 *  City names, locations, daily hi/low labels.
 *
 *   NUMC    - the number of cities.
 *   icitys  - City names.
 *   IDLYTS  - Daily hi/low labels for cities.
 *   CITYUX  - X user coordinates for city locations.
 *   CITYUY  - Y user coordinates for city locations.
 *   TEMPUX  - X user coordinates for daily hi/low locations.
 *   TEMPUY  - Y user coordinates for daily hi/low locations.
 */
/*
 *  Front from Calif. to N. Dakota.
 */
	limitf[0] = 4;
/*
 *  Front from N. Dakota to Virginia.
 */
	limitf[1] = 6;
/*
 *  Front from Virginia out into the Atlantic.
 */
	limitf[2] = 3;
/*
 *  Front from Virginia into Canada.
 */
	limitf[3] = 3;
/*
 *  Region data.
 *
 *  90s in the south and west.
 */
	limits[0] = 32;
	strcpy( irtype[0], "INDEX4" );
/*
 * 100s in Okla./Texas
 */
	limits[1] = 5;
	strcpy( irtype[1],"INDEX0");
/*
 *  70s in Calif. and NW.
 */
	limits[2] = 25;
	strcpy(irtype[2],"INDEX4");
/*
 *  70s in Gt. Lakes area
 */
	limits[3] = 7;
	strcpy(irtype[3],"INDEX4");
/*
 *  70s in New England.
 */
	limits[4] = 7;
	strcpy(irtype[4],"INDEX4");
/*
 *  100s in Calif./Arizona
 */
	limits[5] = 8;
	strcpy(irtype[5],"INDEX0");
/*
 *  T-storms in the southwest.
 */
	limits[6] = 14;
	strcpy(irtype[6],"THUNDE");
/*
 *  T-storms in N. Minn.
 */
	limits[7] = 8;
	strcpy(irtype[7],"THUNDE");
/*
 *  T-storms in California.
 */
	limits[8] = 5;
	strcpy(irtype[8],"THUNDE");
/*
 *  Showers in Montana.
 */
	limits[9] = 8;
	strcpy(irtype[9],"SHOWER");
/*
 *  T-storms in southeast
 */
	limits[10] = 12;
	strcpy(irtype[10],"THUNDE");
/*
 *  T-storms from Missouri to New York.
 */
	limits[11] = 20;
	strcpy(irtype[11],"THUNDE");
/*
 *  Showers in extreme northeast
 */
	limits[12] = 8;
	strcpy(irtype[12],"SHOWER");
/*
 *  Set up color indices to use for temperature regions.
 */
	ic80s = 0; ic90s = 4;
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
/*
 *  Get world coordinates for the U.S. continental boundary and store
 *  the U.S. state map in flash buffer 1.  This takes some execution
 *  time.
 */
    gopen_ws(9,NULL,3);
	wmtint(1,ICDIM,usx,usy,&no);

    gset_fill_int_style(GSTYLE_SOLID);
    gset_fill_colr_ind(5);
/*
 *  Get world coordinate extents, and plot a shaded offset of the
 *  continental U.S. boundary.
 */
    tstext(&xext,&yext);
    for( i = 0; i < no; i++ ) {
        xtmp[i] = usx[i]-.01*xext;
        ytmp[i] = usy[i]-.012*yext;
    }
/*
 * Create structure to pass to gfill_area
 */
	fill_area.num_points = no;
	fill_area.points = (Gpoint *) malloc(fill_area.num_points*sizeof(Gpoint));
	if( !fill_area.points ) {
		fprintf( stderr, "c_wmex01: Not enough memory to create fill area structure\n" );
		gemergency_close_gks();
		exit(1);
	}
	for( i = 0; i < no; i++ ) {
		fill_area.points[i].x = xtmp[i];
		fill_area.points[i].y = ytmp[i];
	}
    gfill_area(&fill_area);
    gset_fill_colr_ind(0);
	for( i = 0; i < no; i++ ) {
		fill_area.points[i].x = usx[i];
		fill_area.points[i].y = usy[i];
	}
    gfill_area(&fill_area);
	free(fill_area.points);
/*
 *-------------------------------
 *  Plot temperature regions.   |
 *-------------------------------
 */
    for( i=0; i < NUMR; i++ ) {
        indxmx = limits[i];
        for( j = 0; j < indxmx; j++ ) {
            c_maptrn(rgnsux[i][j],rgnsuy[i][j],&rgnswx[j],&rgnswy[j]);
        }
        c_wmdrrg(limits[i],rgnswx,rgnswy,irtype[i],no,usx,usy);
    }
/*
 *------------------
 *   U.S. Map      |
 *------------------
 */
    c_gflas3(1);
/*
 *----------------------------
 *  Plot the weather fronts. |
 *----------------------------
 */
    for( i = 0; i < limitf[0]; i++ ) {
        c_maptrn(frnsux[0][i],frnsuy[0][i],&frnswx[i],&frnswy[i]);
    }
    c_wmsetc("FRO","COLD");
    c_wmsetr("END",.040);
    c_wmdrft(limitf[0],frnswx,frnswy);
    c_wmdflt();
/* 
 *   Convert to world coordinates.
 */
    for( i = 0; i < limitf[1]; i++ ) {
        c_maptrn(frnsux[1][i],frnsuy[1][i],&frnswx[i],&frnswy[i]);
	}
/*
 *   Define the type and direction of each symbol.
 */
	c_wmseti("PAI", 1);
	c_wmseti("STY",-2);
	c_wmseti("PAI", 2);
	c_wmseti("STY",-2);
	c_wmseti("PAI", 3);
	c_wmseti("STY", 1);
	c_wmseti("PAI", 4);
	c_wmseti("STY",-2);
	c_wmseti("PAI", 5);
	c_wmseti("STY", 1);
/*
 *   Define spacings.
 */
	c_wmsetr("BEG",.03);
	c_wmsetr("END",.035);
	c_wmsetr("BET",.04);
/*
 *   Draw front.
 */
	c_wmdrft(limitf[1],frnswx,frnswy);
/*
 *   Reset parameters to default values.
 */
	c_wmdflt();

	for( i = 0; i < limitf[2]; i++ ) {
        c_maptrn(frnsux[2][i],frnsuy[2][i],&frnswx[i],&frnswy[i]);
	}
	c_wmsetc("FRO","STA");
	c_wmsetr("BEG",.040);
	c_wmseti("REV",1);
	c_wmdrft(limitf[2],frnswx,frnswy);
	c_wmdflt();

	for( i = 0; i < limitf[3]; i++ ) {
        c_maptrn(frnsux[3][i],frnsuy[3][i],&frnswx[i],&frnswy[i]);
	}
	c_wmsetc("FRO","COLD");
	c_wmsetr("BEG",.040);
	c_wmsetr("BET",.030);
	c_wmdrft(limitf[3],frnswx,frnswy);
	c_wmdflt();
/*
 *----------------
 *  LOs and HIs  |
 *----------------
 */
	for( i = 0; i < NUML; i++ ) {
        c_maptrn(rlohux[i],rlohuy[i],&xo,&yo);
		if (lowhi[i] == 0) {
			c_wmlabs(xo,yo,"LOW");
		}
        else {
			c_wmlabs(xo,yo,"HI");
		}
	}
/*
 *-------------------------------
 *  Regional condition labels.  |
 *-------------------------------
 */
	for( i = 0; i < NUMWL; i++ ) {
        c_maptrn(cndsux[i],cndsuy[i],&xo,&yo);
        ll = c_wmgtln(icndsl[i],strlen(icndsl[i]),0);
		strncpy( stmp1, icndsl[i], ll );
		stmp1[ll] = '\0';
        c_wmlabw(xo,yo,stmp1);
    }
/*  
 *-----------------------------
 *  Cities and temperatures.  |
 *-----------------------------
 *
 *   NUMC    - the number of cities.
 *   icitys  - City names.
 *   idlyts  - Daily hi/low labels for cities.
 *   cityux  - X user coordinates for city locations.
 *   cityuy  - Y user coordinates for city locations.
 *   tempux  - X user coordinates for daily hi/low locations.
 *   tempuy  - Y user coordinates for daily hi/low locations.
 */
	for( i = 0; i < NUMC; i++ ) {
        c_maptrn(cityux[i],cityuy[i],&xo,&yo);
        c_wmlabs(xo,yo,"D");
        c_maptrn(tempux[i],tempuy[i],&xo,&yo);
        ll = c_wmgtln(icitys[i],strlen(icitys[i]),0);
        mm = c_wmgtln(idlyts[i],strlen(idlyts[i]),0);
		strncpy( stmp1, icitys[i], ll );
		stmp1[ll] = '\0';
		strncpy( stmp2, idlyts[i], mm );
		stmp2[mm] = '\0';
        c_wmlabc(xo,yo,stmp1,stmp2);
    }
/*
 *---------------------------------
 *  Regional temperature labels.  |
 *---------------------------------
 *
 *   Texas
 */
    c_maptrn(31.25,-100.50,&xo,&yo);
    c_wmlabt(xo,yo,"90s",0,ic90s);
/*
 *   S. of Okla.
 */
    c_maptrn(34.3,-97.5,&xo,&yo);
    c_wmlabt(xo,yo,"100s",11,ic90s);
/*
 *   S. Ariz.
 */
    c_maptrn(32.,-112.,&xo,&yo);
    c_wmlabt(xo,yo,"100s",2,-1000);
/*
 *   S. Calif.
 */
    c_wmsetr("ARD",65.);
    c_wmsetr("ARL",1.2);
    c_maptrn(34.9,-120.,&xo,&yo);
    c_wmlabs(xo,yo,"ARROW");
    c_maptrn(32.8,-116.9,&xo,&yo);
    c_wmdflt();
    c_wmlabt(xo,yo,"70s",1,-1000);
/*
 *   N. Ariz./New Mexico
 */
    c_maptrn(36.0,-109.0,&xo,&yo);
    c_wmlabt(xo,yo,"80s",0,ic80s);
/*
 *   Oregon
 */
    c_maptrn(43.7,-120.5,&xo,&yo);
    c_wmlabt(xo,yo,"90s",0,-1000);
/*
 *   Oregon Pacific coast.
 */
    c_maptrn(43.,-123.9,&xo,&yo);
    c_wmlabt(xo,yo,"60s",11,-1000);
/*
 *   Colo/Utah
 */
    c_maptrn(40.0,-110.6,&xo,&yo);
    c_wmlabt(xo,yo,"90s",0,ic90s);
/*
 *   N. Montana.
 */
    c_maptrn(48.0,-111.,&xo,&yo);
    c_wmlabt(xo,yo,"80s",0,ic80s);
/*
 *   Idaho
 */
    c_maptrn(48.25,-114.75,&xo,&yo);
    c_wmlabt(xo,yo,"70s",8,-1000);
/*
 *   S. Dakota
 */
    c_maptrn(44.5,-100.0,&xo,&yo);
    c_wmlabt(xo,yo,"90s",0,-1000);
/*
 *   Iowa/Ill.
 */
    c_maptrn(41.5,-89.6,&xo,&yo);
    c_wmlabt(xo,yo,"80s",0,ic80s);
/*
 *   Miss.
 */
    c_maptrn(33.4,-89.6,&xo,&yo);
    c_wmlabt(xo,yo,"90s",0,-1000);
/*
 *   Gt. Lakes
 */
    c_maptrn(47.7,-87.,&xo,&yo);
    c_wmlabt(xo,yo,"70s",8,-1000);
/*
 *   Tenn.
 */
    c_maptrn(35.7,-83.,&xo,&yo);
    c_wmlabt(xo,yo,"80s",0,ic80s);
/*
 *   N. Carolina
 */
    c_maptrn(35.0,-78.,&xo,&yo);
    c_wmlabt(xo,yo,"90s",6,-1000);
/*
 *   Maine
 */
    c_maptrn(46.5,-68.5,&xo,&yo);
    c_wmlabt(xo,yo,"70s",9,-1000);
/*
 *   New york
 */
    c_maptrn(42.7,-75.0,&xo,&yo);
    c_wmlabt(xo,yo,"80s",0,ic80s);
/*
 *-----------------
 *   Main title.  |
 *-----------------
 */
    c_maptrn(53.,-98.0,&xo,&yo);
    c_wmlabt(xo,yo,"July 18, 1994",0,-1000);
/*
 *--------------
 *   Legends.  |
 *--------------
 */
	gsel_norm_tran(0);
	c_wmlgnd(.05,0.16,1,6,1);
	c_wmlgnd(.45,0.15,3,0,0);
	c_wmlgnd(.90,0.15,2,0,0);

	c_frame();
/*
 * Close GKS.
 */
	gdeactivate_ws (WKID);
	gclose_ws(9);
	gclose_ws (WKID);
	gclose_gks();
}

int ieodf;

void wmtint(ibnum,n,usx,usy,no)
int ibnum, n, *no;
float *usx, *usy;
{
/*
 *  Does some initialization.  Stores a map of the U.S. state outlines
 *  in Flash buffer number IBNUM, and returns world coordinate values
 *  for the boundary of the U.S. in (usx(I),usy(I),I=1,NO).  usx and
 *  usy are dimensioned for N and NO is returned as the actual number
 *  of coordinates in the dataset.  N should be at least 6000.
 *
 *  This routine also initializes the EZMAP parameters.
 *
 *  Define space for the area map that is used to obtain the
 *  coordinates for the boundary of the continental U.S.  Set up some 
 *  arrays to be used by areas in obtaining the U.S. continental outline.
 *  The merged polygons from Areas are stored in common MERGCM.
 *  RTPTAR is a subroutine used by Areas to process the areas.
 */
	int i, iama[LAMA];
	float xcra[10000],ycra[10000];
	float p1[2], p2[2], p3[2], p4[2];
	int iaai[10],iagi[10];
	extern int rtptar();
/*
 *  Set up the parameters for drawing the U.S. state outlines.
 *
 *   Position the plot.
 */
	c_mappos(0.05, 0.95, 0.05, 0.95);
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
	c_gflas1(ibnum);
	ieodf = 1;
	c_maplot();
/*
 * Initialize the area map, and put selected outlines in the area map.
 * (A version of MAPEOD is supplied which causes anything not part of 
 * the outer boundary of the U.S. to be omitted).
 */
	c_arinam (iama,LAMA);
	c_mapbla (iama);
	ndum=0;
/*
 * Scan the area map using RTPTAR to fill all the polygons representing 
 * the U.S.  Merge the polygons into a single polygon in MERGCM.
 */
	c_arscam (iama,xcra,ycra,10000,iaai,iagi,10,rtptar);
/*
 * Convert to world coordinates.
 */
	for( i = 0; i < ndum; i++ ) {
		usx[i] = c_cfux(xtmp[i]);
		usy[i] = c_cfuy(ytmp[i]);
	}
	ieodf = 0;
	c_maplot();
	c_gflas2();
	*no = ndum;

	return;
}

void mapeod_(nout,nseg,idls,idrs,npts,pnts)
int *nout, *nseg, *idls, *idrs, *npts;
float *pnts;
{
/*
 *  This version of MAPEOD omits all parts of the "US" outline dataset
 *  which are strictly internal, saving only those which are part of
 *  the external boundary.  (The "border" has area identifier "223";
 *  we omit anything that is not on the "border".)
 */
    if (ieodf == 1) {
        if (*idls != 223 && *idrs != 223) *npts=0;
    }
    return;
}

int rtptar (xcra,ycra,ncra,iaai,iagi,nofg)
float *xcra, *ycra;
int *ncra, *iaai, *iagi, *nofg;
{
    int i, iag1;
    extern void mergpo();
/*
 *  Calls MERGPO to merge all such polygons into one polygon.
 *
 *  Find the area identifier of the area relative to group 1.
 */
    iag1 = -1;

    for( i = 0; i < *nofg; i++ ) {
        if (iagi[i] == 1) iag1=iaai[i];
    }
/*
 *  If the index of the area relative to group 1 is positive and not 223,
 *  fill it.
 */
    if (iag1 > 0 && iag1 != 223) {
        mergpo (xcra,ycra,*ncra);
    }

    return(0);
}

void mergpo (xcra,ycra,ncra)
float *xcra,*ycra;
int ncra;
{
	int ntmp, icra;
/*
 *  This routine merges polygons into a single polygon.
 *
 *  Copy the coordinates of the latest polygon into the merge polygon
 *  coordinate arrays and, if the polygon is not the first of the group,
 *  repeat the first point of the first polygon.  (Actually, the code
 *  below does something a little more complicated: if necessary, it
 *  interpolates points to ensure that the connecting lines between
 *  polygons consist of horizontal and/or vertical steps; this tends
 *  to prevent problems caused by deficiencies in the fill algorithms
 *  on some devices.)
 *
 *  The following statement is a quick-and-dirty patch to leave out all
 *  the off-shore islands, since they seem to cause some problems.
 */
	if (ncra < 100) return;

	ntmp=ndum;

	if (ntmp+ncra+4 <= 9999) {
        if (ndum != 0) {
			if (xtmp[ntmp-1] != xcra[0] && ytmp[ntmp-1] != ycra[0]) {
				if (ytmp[ntmp-1] < ycra[0]) {
					ntmp=ntmp+1;
					xtmp[ntmp-1]=xcra[0];
					ytmp[ntmp-1]=ytmp[ntmp-2];
				}
				else {
					ntmp=ntmp+1;
					xtmp[ntmp-1]=xtmp[ntmp-2];
					ytmp[ntmp-1]=ycra[0];
				}
			}
			ntmp=ntmp+1;
			xtmp[ntmp-1]=xcra[0];
			ytmp[ntmp-1]=ycra[0];
        }
        for( icra=1; icra <= ncra; icra++ ) {
			xtmp[ntmp+icra]=xcra[icra];
			ytmp[ntmp+icra]=ycra[icra];
		}
        ntmp=ntmp+ncra;
        if (ndum != 0) {
			if (xtmp[ntmp-1] != xtmp[0] && ytmp[ntmp-1] != ytmp[0]) {
				if (ytmp[ntmp-1] < ytmp[0]) {
					ntmp=ntmp+1;
					xtmp[ntmp-1]=xtmp[0];
					ytmp[ntmp-1]=ytmp[ntmp-2];
				}
				else {
					ntmp=ntmp+1;
					xtmp[ntmp-1]=xtmp[ntmp-2];
					ytmp[ntmp-1]=ytmp[0];
				}
			}
			ntmp=ntmp+1;
			xtmp[ntmp-1]=xtmp[0];
			ytmp[ntmp-1]=ytmp[0];
        }
    }
    else {
        ntmp=10000;
    }
    
    ndum=ntmp;
    return;
}

void tstext(xext,yext)
float *xext, *yext;
{
/*
 *  Calculate the world coordinate extents.
 */
	Gint ier, ntr;
	float window[4],viewpt[4];
	Gtran limit;

	ginq_cur_norm_tran_num(&ier,&ntr);
	ginq_norm_tran(ntr,&ier,&limit);
	*xext = limit.win.x_max - limit.win.x_min;
	*yext = limit.win.y_max - limit.win.y_min;
	return;
}

