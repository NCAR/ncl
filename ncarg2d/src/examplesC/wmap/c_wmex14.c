/*
** an example of using the tools in the weather map library to
** create a color weather map.
*/

#include <stdio.h>
#include <strings.h>
#include <math.h>

#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

/*
** define fortran unit number, and workstation type,
** and workstation id.
*/
#define LUNIT "gmeta"  
#define IWTYPE SED_WSTYPE  
#define IWKID 1 

#define ICDIM 6000 
#define NUMF 4
#define NUMFP 6 
#define NUMR 14
#define NUMRP 35 
#define NUMWL 5
#define NUML 5
#define NUMC 14
 
float XTmp[9999],YTmp[9999];
int NDum;

int main()
{
        int ic60s, ic70s, ic80s, ic90s, ic100s;
	int no;
	int i,j;
	int indxmx;
	float xext, yext;
	float xo, yo;
	int ll, mm;
        Gcolr_rep color;   
        Gpoint_list fill_area;
	extern void tstext();
	extern void wmtint();

/*
** set up all of the coordinate data for the fronts, temperature regions,
** cities, etc.
*/
        float usx[ICDIM];
	float usy[ICDIM];
/*
** weather front arrays.
**
**  NUMF    - the number of fronts.
**  NUMFP   - the maximum number of control points for any front.
**  limitf  - the defined number of control points for each region.
**  frnsux  - x user coordinates for fronts.
**  frnsuy  - y user coordinates for fronts.
*/
        int limitf[NUMFP];

        float frnsux[NUMF][NUMFP];
	float frnsuy[NUMF][NUMFP];
        float frnswx[NUMFP];
	float frnswy[NUMFP];
/*
** temperature and weather region arrays.
**
**  NUMR    - the number of regions
**  NUMRP   - uhe maximum number of control points for any region.
**  limits  - the defined number of control points for each region.
**  irtype  - the type of region.
**  rgnsux  - x user coordinates for regions.
**  rgnsuy  - y user coordinates for regions.
*/
        char irtype[NUMR][7];  /* need to allocate 6 chars each*/
        int limits[NUMR];
	float rgnsux[NUMR][NUMRP];
	float rgnsuy[NUMR][NUMRP];
        float rgnswx[NUMRP];
	float rgnswy[NUMRP];
/*
** regional weather labels.
**
**  NUMWL   - the number of labels.
**  cndsux  - x user coordinates for labels.
**  cndsuy  - y user coordinates for labels.
**  icndsl  - labels.
*/
        float cndsux[NUMWL];
	float cndsuy[NUMWL];
        char icndsl[NUMWL][7];
/*
** low/high arrays.
**
**  NUML    - the number of lows and highs to plot.
**  frnsux  - x user coordinates for symbols.
**  frnsuy  - y user coordinates for symbols.
**  lowhi   - array of low/high flags (0=low; 1=hi).
*/
        float rlohux[NUML];
	float rlohuy[NUML];
	int lowhi[NUML];
/*
** city names, locations, daily hi/low labels.
**
**  NUMC    - the number of cities.
**  icitys  - city names.
**  idlyts  - daily hi/low labels for cities.
**  cityux  - x user coordinates for city locations.
**  cityuy  - y user coordinates for city locations.
**  tempux  - x user coordinates for daily hi/low locations.
**  tempuy  - y user coordinates for daily hi/low locations.
*/
        float cityux[NUMC];
	float cityuy[NUMC];
	float tempux[NUMC];
	float tempuy[NUMC];
        char icitys[NUMC][14];
        char idlyts[NUMC][8];
/*
** front data.
**
** front from calif. to n. dakota.
*/
        limitf[0]=4;
      
        frnsux[0][0]=41.0; frnsux[0][1]=42.5; frnsux[0][2]=44.5; 
	frnsux[0][3]=49.0; frnsux[0][4]=00.0; frnsux[0][5]=00.0;

        frnsuy[0][0]= -121.5; frnsuy[0][1]= -111.0; frnsuy[0][2]= -104.0;
	frnsuy[0][3]=  -98.0; frnsuy[0][4]=   00.0; frnsuy[0][5]=   00.0;
/*
** front from n. dakota to virginia.
*/
        limitf[1]=6;

        frnsux[1][0]=49.0; frnsux[1][1]=47.0; frnsux[1][2]=45.0; 
	frnsux[1][3]=42.0; frnsux[1][4]=38.8; frnsux[1][5]=38.5;

        frnsuy[1][0]= -98.0; frnsuy[1][1]= -96.0; frnsuy[1][2]= -95.0;
	frnsuy[1][3]= -93.0; frnsuy[1][4]= -88.3; frnsuy[1][5]= -80.5;
/*
** front from virginia out into the atlantic.
*/
        limitf[2]=3;

        frnsux[2][0]=38.5; frnsux[2][1]=39.0; frnsux[2][2]=37.5; 
	frnsux[2][3]=00.0; frnsux[2][4]=00.0; frnsux[2][5]=00.0;

        frnsuy[2][0]= -80.5; frnsuy[2][1]= -77.0; frnsuy[2][2]= -71.0;
	frnsuy[2][3]=   0.0; frnsuy[2][4]=   0.0; frnsuy[2][5]=   0.0;
/*
** front from virginia into canada.
*/
        limitf[3]=3;

        frnsux[3][0]=38.5; frnsux[3][1]=43.0; frnsux[3][2]=49.0; 
	frnsux[3][3]= 0.0; frnsux[3][4]= 0.0; frnsux[3][5]= 0.0;

        frnsuy[3][0]= -80.5; frnsuy[3][1]= -77.5; frnsuy[3][2]= -77.0;
	frnsuy[3][3]=   0.0; frnsuy[3][4]=   0.0; frnsuy[3][5]=   0.0;
/*
** region data.
**
** 60s in calif. (must be plotted before the 70s region in calif.)
*/
        limits[0]=14;
        strcpy(irtype[0],"index6");

        rgnsux[0][0]= 32.0; rgnsux[0][1]= 34.5;  rgnsux[0][2]= 35.5; 
        rgnsux[0][3]= 37.0; rgnsux[0][4]= 40.0;  rgnsux[0][5]= 42.8; 
        rgnsux[0][6]= 46.0; rgnsux[0][7]= 49.0;  rgnsux[0][8]= 50.0; 
        rgnsux[0][9]= 49.0; rgnsux[0][10]= 45.0; rgnsux[0][11]=40.0; 
        rgnsux[0][12]=35.0; rgnsux[0][13]= 32.0; rgnsux[0][14]=00.0; 
        rgnsux[0][15]=00.0; rgnsux[0][16]= 00.0; rgnsux[0][17]=00.0; 
        rgnsux[0][18]=00.0; rgnsux[0][19]= 00.0; rgnsux[0][20]=00.0;
        rgnsux[0][21]=00.0; rgnsux[0][22]= 00.0; rgnsux[0][23]=00.0; 
        rgnsux[0][24]=00.0; rgnsux[0][25]= 00.0; rgnsux[0][26]=00.0; 
        rgnsux[0][27]=00.0; rgnsux[0][28]= 00.0; rgnsux[0][29]=00.0; 
        rgnsux[0][30]=00.0; rgnsux[0][31]= 00.0; rgnsux[0][32]=00.0; 
        rgnsux[0][33]=00.0; rgnsux[0][34]= 00.0;

        rgnsuy[0][0]=  -120.0; rgnsuy[0][1]=  -120.0; rgnsuy[0][2]=  -120.1; 
        rgnsuy[0][3]=  -121.0; rgnsuy[0][4]=  -122.8; rgnsuy[0][5]=  -123.4; 
        rgnsuy[0][6]=  -122.5; rgnsuy[0][7]=  -122.0; rgnsuy[0][8]=  -124.0; 
        rgnsuy[0][9]=  -127.0; rgnsuy[0][10]= -126.0; rgnsuy[0][11]= -125.0; 
        rgnsuy[0][12]= -123.0; rgnsuy[0][13]= -120.0; rgnsuy[0][14]=   00.0;
        rgnsuy[0][15]=   00.0; rgnsuy[0][16]=   00.0; rgnsuy[0][17]=   00.0; 
        rgnsuy[0][18]=   00.0; rgnsuy[0][19]=   00.0; rgnsuy[0][20]=   00.0; 
        rgnsuy[0][21]=   00.0; rgnsuy[0][22]=   00.0; rgnsuy[0][23]=   00.0;
        rgnsuy[0][24]=   00.0; rgnsuy[0][25]=   00.0; rgnsuy[0][26]=   00.0; 
        rgnsuy[0][27]=   00.0; rgnsuy[0][28]=   00.0; rgnsuy[0][29]=   00.0; 
        rgnsuy[0][30]=   00.0; rgnsuy[0][31]=   00.0; rgnsuy[0][32]=   00.0;
        rgnsuy[0][33]=   00.0; rgnsuy[0][34]=   00.0;

/*
** 90s in the south and west.
*/
        limits[1]=32;
        strcpy(irtype[1],"index3");

        rgnsux[1][0]=  32.7; rgnsux[1][1]=  34.5; rgnsux[1][2]=  38.0; 
        rgnsux[1][3]=  42.0; rgnsux[1][4]=  45.0; rgnsux[1][5]=  47.5; 
        rgnsux[1][6]=  46.0; rgnsux[1][7]=  43.0; rgnsux[1][8]=  39.5; 
        rgnsux[1][9]=  38.3; rgnsux[1][10]= 40.0; rgnsux[1][11]= 37.5; 
        rgnsux[1][12]= 35.5; rgnsux[1][13]= 32.7; rgnsux[1][14]= 36.0; 
        rgnsux[1][15]= 41.0; rgnsux[1][16]= 42.0; rgnsux[1][17]= 44.5; 
        rgnsux[1][18]= 47.0; rgnsux[1][19]= 48.0; rgnsux[1][20]= 46.0;
        rgnsux[1][21]= 42.0; rgnsux[1][22]= 39.0; rgnsux[1][23]= 36.0; 
        rgnsux[1][24]= 37.3; rgnsux[1][25]= 33.0; rgnsux[1][26]= 37.5; 
        rgnsux[1][27]= 29.0; rgnsux[1][28]= 24.0; rgnsux[1][29]= 20.0; 
        rgnsux[1][30]= 29.0; rgnsux[1][31]= 32.7; rgnsux[1][32]= 00.0; 
        rgnsux[1][33]= 00.0; rgnsux[1][34]= 00.0;

        rgnsuy[1][0]=  -116.0; rgnsuy[1][1]=  -117.0; rgnsuy[1][2]=  -120.5; 
        rgnsuy[1][3]=  -122.7; rgnsuy[1][4]=  -122.0; rgnsuy[1][5]=  -118.0; 
        rgnsuy[1][6]=  -115.3; rgnsuy[1][7]=  -113.0; rgnsuy[1][8]=  -107.5; 
        rgnsuy[1][9]=  -109.0; rgnsuy[1][10]= -115.0; rgnsuy[1][11]= -114.0; 
        rgnsuy[1][12]= -111.5; rgnsuy[1][13]= -109.0; rgnsuy[1][14]= -106.0; 
        rgnsuy[1][15]= -105.0; rgnsuy[1][16]= -104.0; rgnsuy[1][17]= -107.0; 
        rgnsuy[1][18]= -109.7; rgnsuy[1][19]= -104.0; rgnsuy[1][20]=  -98.0;
        rgnsuy[1][21]=  -95.5; rgnsuy[1][22]=  -96.0; rgnsuy[1][23]=  -93.0; 
        rgnsuy[1][24]=  -88.5; rgnsuy[1][25]=  -83.0; rgnsuy[1][26]=  -76.0; 
        rgnsuy[1][27]=  -75.0; rgnsuy[1][28]=  -80.0; rgnsuy[1][29]=  -97.0; 
        rgnsuy[1][30]= -115.0; rgnsuy[1][31]= -116.0; rgnsuy[1][32]=   00.0; 
        rgnsuy[1][33]=   00.0; rgnsuy[1][34]=   00.0;

/*
** 70s in calif. and nw.
*/
        limits[2]=25;
        strcpy(irtype[2],"index5");

        rgnsux[2][0]=  35.0; rgnsux[2][1]=  36.0; rgnsux[2][2]= 38.0; 
        rgnsux[2][3]=  40.0; rgnsux[2][4]=  43.0; rgnsux[2][5]= 46.5; 
        rgnsux[2][6]=  48.5; rgnsux[2][7]=  47.0; rgnsux[2][8]= 44.0; 
        rgnsux[2][9]=  44.5; rgnsux[2][10]= 46.0; rgnsux[2][11]= 47.0; 
        rgnsux[2][12]= 49.0; rgnsux[2][13]= 53.0; rgnsux[2][14]= 48.0; 
        rgnsux[2][15]= 46.0; rgnsux[2][16]= 42.8; rgnsux[2][17]= 40.0; 
        rgnsux[2][18]= 37.0; rgnsux[2][19]= 35.5; rgnsux[2][20]= 35.0;
        rgnsux[2][21]= 33.0; rgnsux[2][22]= 28.0; rgnsux[2][23]= 30.0; 
        rgnsux[2][24]= 32.7; rgnsux[2][25]= 35.0; rgnsux[2][26]= 00.0; 
        rgnsux[2][27]= 00.0; rgnsux[2][28]= 00.0; rgnsux[2][29]= 00.0; 
        rgnsux[2][30]= 00.0; rgnsux[2][31]= 00.0; rgnsux[2][32]= 00.0; 
        rgnsux[2][33]= 00.0; rgnsux[2][34]= 00.0;

        rgnsuy[2][0]=  -118.7; rgnsuy[2][1]=  -120.0; rgnsuy[2][2]= -121.5; 
        rgnsuy[2][3]=  -122.7; rgnsuy[2][4]=  -123.1; rgnsuy[2][5]= -121.8; 
        rgnsuy[2][6]=  -118.0; rgnsuy[2][7]=  -114.5; rgnsuy[2][8]= -112.0; 
        rgnsuy[2][9]=  -110.5; rgnsuy[2][10]= -112.0; rgnsuy[2][11]= -113.2; 
        rgnsuy[2][12]= -114.0; rgnsuy[2][13]= -122.0; rgnsuy[2][14]= -123.5; 
        rgnsuy[2][15]= -123.0; rgnsuy[2][16]= -123.5; rgnsuy[2][17]= -123.0; 
        rgnsuy[2][18]= -121.5; rgnsuy[2][19]= -120.5; rgnsuy[2][20]= -120.9;
        rgnsuy[2][21]= -122.0; rgnsuy[2][22]= -120.0; rgnsuy[2][23]= -116.0; 
        rgnsuy[2][24]= -116.5; rgnsuy[2][25]= -118.7; rgnsuy[2][26]=   00.0; 
        rgnsuy[2][27]=   00.0; rgnsuy[2][28]=   00.0; rgnsuy[2][29]=   00.0; 
        rgnsuy[2][30]=   00.0; rgnsuy[2][31]=   00.0; rgnsuy[2][32]=   00.0; 
        rgnsuy[2][33]=   00.0; rgnsuy[2][34]=   00.0;

/*
** 70s in gt. lakes area
*/
        limits[3]=7;
        strcpy(irtype[3],"index5");

        rgnsux[3][0]=  49.0; rgnsux[3][1]=  45.5; rgnsux[3][2]=  46.0; 
        rgnsux[3][3]=  50.0; rgnsux[3][4]=  53.0; rgnsux[3][5]=  51.5; 
        rgnsux[3][6]=  49.0; rgnsux[3][7]=  00.0; rgnsux[3][8]=  00.0; 
        rgnsux[3][9]=  00.0; rgnsux[3][10]= 00.0; rgnsux[3][11]= 00.0; 
        rgnsux[3][12]= 00.0; rgnsux[3][13]= 00.0; rgnsux[3][14]= 00.0; 
        rgnsux[3][15]= 00.0; rgnsux[3][16]= 00.0; rgnsux[3][17]= 00.0; 
        rgnsux[3][18]= 00.0; rgnsux[3][19]= 00.0; rgnsux[3][20]= 00.0;
        rgnsux[3][21]= 00.0; rgnsux[3][22]= 00.0; rgnsux[3][23]= 00.0; 
        rgnsux[3][24]= 00.0; rgnsux[3][25]= 00.0; rgnsux[3][26]= 00.0; 
        rgnsux[3][27]= 00.0; rgnsux[3][28]= 00.0; rgnsux[3][29]= 00.0; 
        rgnsux[3][30]= 00.0; rgnsux[3][31]= 00.0; rgnsux[3][32]= 00.0; 
        rgnsux[3][33]= 00.0; rgnsux[3][34]= 00.0;

        rgnsuy[3][0]= -96.0; rgnsuy[3][1]= -90.5; rgnsuy[3][2]= -84.0; 
        rgnsuy[3][3]= -82.0; rgnsuy[3][4]= -89.0; rgnsuy[3][5]= -95.0; 
        rgnsuy[3][6]= -96.0; rgnsuy[3][7]=  00.0; rgnsuy[3][8]=  00.0; 
        rgnsuy[3][9]=  00.0; rgnsuy[3][10]= 00.0; rgnsuy[3][11]= 00.0; 
        rgnsuy[3][12]= 00.0; rgnsuy[3][13]= 00.0; rgnsuy[3][14]= 00.0; 
        rgnsuy[3][15]= 00.0; rgnsuy[3][16]= 00.0; rgnsuy[3][17]= 00.0; 
        rgnsuy[3][18]= 00.0; rgnsuy[3][19]= 00.0; rgnsuy[3][20]= 00.0;
        rgnsuy[3][21]= 00.0; rgnsuy[3][22]= 00.0; rgnsuy[3][23]= 00.0; 
        rgnsuy[3][24]= 00.0; rgnsuy[3][25]= 00.0; rgnsuy[3][26]= 00.0; 
        rgnsuy[3][27]= 00.0; rgnsuy[3][28]= 00.0; rgnsuy[3][29]= 00.0; 
        rgnsuy[3][30]= 00.0; rgnsuy[3][31]= 00.0; rgnsuy[3][32]= 00.0; 
        rgnsuy[3][33]= 00.0; rgnsuy[3][34]= 00.0;

/*
** 70s in new england.
*/
        limits[4]=7;
        strcpy(irtype[4],"index5");

        rgnsux[4][0]= 45.0;  rgnsux[4][1]= 43.2;  rgnsux[4][2]= 41.5; 
        rgnsux[4][3]= 43.0;  rgnsux[4][4]= 47.0;  rgnsux[4][5]= 49.0; 
        rgnsux[4][6]= 45.0;  rgnsux[4][7]= 00.0;  rgnsux[4][8]= 00.0; 
        rgnsux[4][9]= 00.0;  rgnsux[4][10]= 00.0; rgnsux[4][11]= 00.0; 
        rgnsux[4][12]= 00.0; rgnsux[4][13]= 00.0; rgnsux[4][14]= 00.0; 
        rgnsux[4][15]= 00.0; rgnsux[4][16]= 00.0; rgnsux[4][17]= 00.0; 
        rgnsux[4][18]= 00.0; rgnsux[4][19]= 00.0; rgnsux[4][20]= 00.0;
        rgnsux[4][21]= 00.0; rgnsux[4][22]= 00.0; rgnsux[4][23]= 00.0; 
        rgnsux[4][24]= 00.0; rgnsux[4][25]= 00.0; rgnsux[4][26]= 00.0; 
        rgnsux[4][27]= 00.0; rgnsux[4][28]= 00.0; rgnsux[4][29]= 00.0; 
        rgnsux[4][30]= 00.0; rgnsux[4][31]= 00.0; rgnsux[4][32]= 00.0; 
        rgnsux[4][33]= 00.0; rgnsux[4][34]= 00.0;

        rgnsuy[4][0]= -73.3; rgnsuy[4][1]= -72.5; rgnsuy[4][2]= -70.5; 
        rgnsuy[4][3]= -66.0; rgnsuy[4][4]= -65.0; rgnsuy[4][5]= -71.0; 
        rgnsuy[4][6]= -73.3; rgnsuy[4][7]=  00.0; rgnsuy[4][8]=  00.0; 
        rgnsuy[4][9]=  00.0; rgnsuy[4][10]= 00.0; rgnsuy[4][11]= 00.0; 
        rgnsuy[4][12]= 00.0; rgnsuy[4][13]= 00.0; rgnsuy[4][14]= 00.0; 
        rgnsuy[4][15]= 00.0; rgnsuy[4][16]= 00.0; rgnsuy[4][17]= 00.0; 
        rgnsuy[4][18]= 00.0; rgnsuy[4][19]= 00.0; rgnsuy[4][20]= 00.0;
        rgnsuy[4][21]= 00.0; rgnsuy[4][22]= 00.0; rgnsuy[4][23]= 00.0; 
        rgnsuy[4][24]= 00.0; rgnsuy[4][25]= 00.0; rgnsuy[4][26]= 00.0; 
        rgnsuy[4][27]= 00.0; rgnsuy[4][28]= 00.0; rgnsuy[4][29]= 00.0; 
        rgnsuy[4][30]= 00.0; rgnsuy[4][31]= 00.0; rgnsuy[4][32]= 00.0; 
        rgnsuy[4][33]= 00.0; rgnsuy[4][34]= 00.0;

/*
** 100s in calif./arizona (must be plotted after 90s in southwest)
*/
        limits[5]=8;
        strcpy(irtype[5],"index2");

        rgnsux[5][0]=  32.7; rgnsux[5][1]=  35.0; rgnsux[5][2]=  36.0; 
        rgnsux[5][3]=  35.3; rgnsux[5][4]=  33.0; rgnsux[5][5]=  31.3; 
        rgnsux[5][6]=  30.0; rgnsux[5][7]=  00.0; rgnsux[5][8]=  00.0; 
        rgnsux[5][9]=  00.0; rgnsux[5][10]= 00.0; rgnsux[5][11]= 00.0; 
        rgnsux[5][12]= 00.0; rgnsux[5][13]= 00.0; rgnsux[5][14]= 00.0; 
        rgnsux[5][15]= 00.0; rgnsux[5][16]= 00.0; rgnsux[5][17]= 00.0; 
        rgnsux[5][18]= 00.0; rgnsux[5][19]= 00.0; rgnsux[5][20]= 00.0;
        rgnsux[5][21]= 00.0; rgnsux[5][22]= 00.0; rgnsux[5][23]= 00.0; 
        rgnsux[5][24]= 00.0; rgnsux[5][25]= 00.0; rgnsux[5][26]= 00.0; 
        rgnsux[5][27]= 00.0; rgnsux[5][28]= 00.0; rgnsux[5][29]= 00.0; 
        rgnsux[5][30]= 00.0; rgnsux[5][31]= 00.0; rgnsux[5][32]= 00.0; 
        rgnsux[5][33]= 00.0; rgnsux[5][34]= 00.0;

        rgnsuy[5][0]= -115.0; rgnsuy[5][1]= -116.0; rgnsuy[5][2]= -115.3; 
        rgnsuy[5][3]= -114.0; rgnsuy[5][4]= -111.0; rgnsuy[5][5]= -111.0; 
        rgnsuy[5][6]= -113.0; rgnsuy[5][7]= -115.0; rgnsuy[5][8]=   00.0; 
        rgnsuy[5][9]=   00.0; rgnsuy[5][10]=  00.0; rgnsuy[5][11]=  00.0; 
        rgnsuy[5][12]=  00.0; rgnsuy[5][13]=  00.0; rgnsuy[5][14]=  00.0; 
        rgnsuy[5][15]=  00.0; rgnsuy[5][16]=  00.0; rgnsuy[5][17]=  00.0; 
        rgnsuy[5][18]=  00.0; rgnsuy[5][19]=  00.0; rgnsuy[5][20]=  00.0;
        rgnsuy[5][21]=  00.0; rgnsuy[5][22]=  00.0; rgnsuy[5][23]=  00.0; 
        rgnsuy[5][24]=  00.0; rgnsuy[5][25]=  00.0; rgnsuy[5][26]=  00.0; 
        rgnsuy[5][27]=  00.0; rgnsuy[5][28]=  00.0; rgnsuy[5][29]=  00.0; 
        rgnsuy[5][30]=  00.0; rgnsuy[5][31]=  00.0; rgnsuy[5][32]=  00.0; 
        rgnsuy[5][33]=  00.0; rgnsuy[5][34]=  00.0;

/*
** 100s in okla./texas (must be plotted after 90s in okla./texas)
*/
        limits[6]=5;
        strcpy(irtype[6],"index2");

        rgnsux[6][0]=  35.0; rgnsux[6][1]=  33.0; rgnsux[6][2]=  31.5; 
        rgnsux[6][3]=  33.0; rgnsux[6][4]=  35.0; rgnsux[6][5]=  00.0; 
        rgnsux[6][6]=  00.0; rgnsux[6][7]=  00.0; rgnsux[6][8]=  00.0; 
        rgnsux[6][9]=  00.0; rgnsux[6][10]= 00.0; rgnsux[6][11]= 00.0; 
        rgnsux[6][12]= 00.0; rgnsux[6][13]= 00.0; rgnsux[6][14]= 00.0; 
        rgnsux[6][15]= 00.0; rgnsux[6][16]= 00.0; rgnsux[6][17]= 00.0; 
        rgnsux[6][18]= 00.0; rgnsux[6][19]= 00.0; rgnsux[6][20]= 00.0;
        rgnsux[6][21]= 00.0; rgnsux[6][22]= 00.0; rgnsux[6][23]= 00.0; 
        rgnsux[6][24]= 00.0; rgnsux[6][25]= 00.0; rgnsux[6][26]= 00.0; 
        rgnsux[6][27]= 00.0; rgnsux[6][28]= 00.0; rgnsux[6][29]= 00.0; 
        rgnsux[6][30]= 00.0; rgnsux[6][31]= 00.0; rgnsux[6][32]= 00.0; 
        rgnsux[6][33]= 00.0; rgnsux[6][34]= 00.0;

        rgnsuy[6][0]=  -97.5; rgnsuy[6][1]=   -96.5; rgnsuy[6][2]= -97.5; 
        rgnsuy[6][3]=  -98.5; rgnsuy[6][4]=   -97.5; rgnsuy[6][5]=  00.0; 
        rgnsuy[6][6]=   00.0; rgnsuy[6][7]=   -80.0; rgnsuy[6][8]= -97.0; 
        rgnsuy[6][9]= -115.0; rgnsuy[6][10]= -116.0; rgnsuy[6][11]= 00.0; 
        rgnsuy[6][12]=  00.0; rgnsuy[6][13]=   00.0; rgnsuy[6][14]= 00.0; 
        rgnsuy[6][15]=  00.0; rgnsuy[6][16]=   00.0; rgnsuy[6][17]= 00.0; 
        rgnsuy[6][18]=  00.0; rgnsuy[6][19]=   00.0; rgnsuy[6][20]= 00.0;
        rgnsuy[6][21]=  00.0; rgnsuy[6][22]=   00.0; rgnsuy[6][23]= 00.0; 
        rgnsuy[6][24]=  00.0; rgnsuy[6][25]=   00.0; rgnsuy[6][26]= 00.0; 
        rgnsuy[6][27]=  00.0; rgnsuy[6][28]=   00.0; rgnsuy[6][29]= 00.0; 
        rgnsuy[6][30]=  00.0; rgnsuy[6][31]=   00.0; rgnsuy[6][32]= 00.0; 
        rgnsuy[6][33]=  00.0; rgnsuy[6][34]=   00.0;

/*
** t-storms in the southwest.
*/
        limits[7]=14;
        strcpy(irtype[7],"thunde");

        rgnsux[7][0]=  33.5; rgnsux[7][1]=  35.5; rgnsux[7][2]=  37.2; 
        rgnsux[7][3]=  38.5; rgnsux[7][4]=  39.5; rgnsux[7][5]=  39.5; 
        rgnsux[7][6]=  38.0; rgnsux[7][7]=  36.0; rgnsux[7][8]=  34.5; 
        rgnsux[7][9]=  33.0; rgnsux[7][10]= 33.0; rgnsux[7][11]= 33.0; 
        rgnsux[7][12]= 31.0; rgnsux[7][13]= 33.5; rgnsux[7][14]= 00.0; 
        rgnsux[7][15]= 00.0; rgnsux[7][16]= 00.0; rgnsux[7][17]= 00.0; 
        rgnsux[7][18]= 00.0; rgnsux[7][19]= 00.0; rgnsux[7][20]= 00.0;
        rgnsux[7][21]= 00.0; rgnsux[7][22]= 00.0; rgnsux[7][23]= 00.0; 
        rgnsux[7][24]= 00.0; rgnsux[7][25]= 00.0; rgnsux[7][26]= 00.0; 
        rgnsux[7][27]= 00.0; rgnsux[7][28]= 00.0; rgnsux[7][29]= 00.0; 
        rgnsux[7][30]= 00.0; rgnsux[7][31]= 00.0; rgnsux[7][32]= 00.0; 
        rgnsux[7][33]= 00.0; rgnsux[7][34]= 00.0;

        rgnsuy[7][0]=  -110.2; rgnsuy[7][1]=  -111.5; rgnsuy[7][2]=  -113.0; 
        rgnsuy[7][3]=  -111.5; rgnsuy[7][4]=  -108.5; rgnsuy[7][5]=  -106.3; 
        rgnsuy[7][6]=  -105.5; rgnsuy[7][7]=  -106.0; rgnsuy[7][8]=  -107.5; 
        rgnsuy[7][9]=  -108.0; rgnsuy[7][10]= -108.3; rgnsuy[7][11]= -109.6; 
        rgnsuy[7][12]= -110.2; rgnsuy[7][13]= -110.2; rgnsuy[7][14]=   00.0; 
        rgnsuy[7][15]=   00.0; rgnsuy[7][16]=   00.0; rgnsuy[7][17]=   00.0; 
        rgnsuy[7][18]=   00.0; rgnsuy[7][19]=   00.0; rgnsuy[7][20]=   00.0;
        rgnsuy[7][21]=   00.0; rgnsuy[7][22]=   00.0; rgnsuy[7][23]=   00.0; 
        rgnsuy[7][24]=   00.0; rgnsuy[7][25]=   00.0; rgnsuy[7][27]=   00.0; 
        rgnsuy[7][27]=   00.0; rgnsuy[7][28]=   00.0; rgnsuy[7][29]=   00.0; 
        rgnsuy[7][30]=   00.0; rgnsuy[7][31]=   00.0; rgnsuy[7][32]=   00.0; 
        rgnsuy[7][33]=   00.0; rgnsuy[7][34]=   00.0;

/*
** t-storms in n. minn.
*/
        limits[8]=8;
        strcpy(irtype[8],"thunde");

        rgnsux[8][0]=  49.0; rgnsux[8][1]=  50.5; rgnsux[8][2]=  49.0; 
        rgnsux[8][3]=  47.0; rgnsux[8][4]=  45.0; rgnsux[8][5]=  45.0; 
        rgnsux[8][6]=  47.0; rgnsux[8][7]=  49.0; rgnsux[8][8]=  00.0; 
        rgnsux[8][9]=  00.0; rgnsux[8][10]= 00.0; rgnsux[8][11]= 00.0; 
        rgnsux[8][12]= 00.0; rgnsux[8][13]= 00.0; rgnsux[8][14]= 00.0; 
        rgnsux[8][15]= 00.0; rgnsux[8][16]= 00.0; rgnsux[8][17]= 00.0; 
        rgnsux[8][18]= 00.0; rgnsux[8][19]= 00.0; rgnsux[8][20]= 00.0;
        rgnsux[8][21]= 00.0; rgnsux[8][22]= 00.0; rgnsux[8][23]= 00.0; 
        rgnsux[8][24]= 00.0; rgnsux[8][25]= 00.0; rgnsux[8][26]= 00.0; 
        rgnsux[8][27]= 00.0; rgnsux[8][28]= 00.0; rgnsux[8][29]= 00.0; 
        rgnsux[8][30]= 00.0; rgnsux[8][31]= 00.0; rgnsux[8][32]= 00.0; 
        rgnsux[8][33]= 00.0; rgnsux[8][34]= 00.0;

        rgnsuy[8][0]= -97.0; rgnsuy[8][1]= -94.0; rgnsuy[8][2]= -90.0;
        rgnsuy[8][3]= -89.3; rgnsuy[8][4]= -91.5; rgnsuy[8][5]= -93.5;
        rgnsuy[8][6]= -96.0; rgnsuy[8][7]= -97.0; rgnsuy[8][8]=  00.0;
        rgnsuy[8][9]=  00.0; rgnsuy[8][10]= 00.0; rgnsuy[8][11]= 00.0;
        rgnsuy[8][12]= 00.0; rgnsuy[8][13]= 00.0; rgnsuy[8][14]= 00.0;
        rgnsuy[8][15]= 00.0; rgnsuy[8][16]= 00.0; rgnsuy[8][17]= 00.0;
        rgnsuy[8][18]= 00.0; rgnsuy[8][19]= 00.0; rgnsuy[8][20]= 00.0;
        rgnsuy[8][21]= 00.0; rgnsuy[8][22]= 00.0; rgnsuy[8][23]= 00.0;
        rgnsuy[8][24]= 00.0; rgnsuy[8][25]= 00.0; rgnsuy[8][26]= 00.0;
        rgnsuy[8][27]= 00.0; rgnsuy[8][28]= 00.0; rgnsuy[8][29]= 00.0;
        rgnsuy[8][30]= 00.0; rgnsuy[8][31]= 00.0; rgnsuy[8][32]= 00.0;
        rgnsuy[8][33]= 00.0; rgnsuy[8][34]= 00.0;

/*
** t-storms in california.
*/
        limits[9]=5;
        strcpy(irtype[9],"thunde");

        rgnsux[9][0]=  39.0; rgnsux[9][1]=  38.0; rgnsux[9][2]=  35.3;
        rgnsux[9][3]=  37.0; rgnsux[9][4]=  39.0; rgnsux[9][5]=  00.0;
        rgnsux[9][6]=  00.0; rgnsux[9][7]=  00.0; rgnsux[9][8]=  00.0;
        rgnsux[9][9]=  00.0; rgnsux[9][10]= 00.0; rgnsux[9][11]= 00.0;
        rgnsux[9][12]= 00.0; rgnsux[9][13]= 00.0; rgnsux[9][14]= 00.0;
        rgnsux[9][15]= 00.0; rgnsux[9][16]= 00.0; rgnsux[9][17]= 00.0;
        rgnsux[9][18]= 00.0; rgnsux[9][19]= 00.0; rgnsux[9][20]= 00.0;
        rgnsux[9][21]= 00.0; rgnsux[9][22]= 00.0; rgnsux[9][23]= 00.0;
        rgnsux[9][24]= 00.0; rgnsux[9][25]= 00.0; rgnsux[9][26]= 00.0;
        rgnsux[9][27]= 00.0; rgnsux[9][28]= 00.0; rgnsux[9][29]= 00.0;
        rgnsux[9][30]= 00.0; rgnsux[9][31]= 00.0; rgnsux[9][32]= 00.0;
        rgnsux[9][33]= 00.0; rgnsux[9][34]= 00.0;

        rgnsuy[9][0]= -120.0; rgnsuy[9][1]= -118.0; rgnsuy[9][2]= -116.5;
        rgnsuy[9][3]= -119.0; rgnsuy[9][4]= -120.0; rgnsuy[9][5]=   00.0;
        rgnsuy[9][6]=   00.0; rgnsuy[9][7]=   00.0; rgnsuy[9][8]=   00.0;
        rgnsuy[9][9]=   00.0; rgnsuy[9][10]=  00.0; rgnsuy[9][11]=  00.0;
        rgnsuy[9][12]=  00.0; rgnsuy[9][13]=  00.0; rgnsuy[9][14]=  00.0;
        rgnsuy[9][15]=  00.0; rgnsuy[9][16]=  00.0; rgnsuy[9][17]=  00.0;
        rgnsuy[9][18]=  00.0; rgnsuy[9][19]=  00.0; rgnsuy[9][20]=  00.0;
        rgnsuy[9][21]=  00.0; rgnsuy[9][22]=  00.0; rgnsuy[9][23]=  00.0;
        rgnsuy[9][24]=  00.0; rgnsuy[9][25]=  00.0; rgnsuy[9][26]=  00.0;
        rgnsuy[9][27]=  00.0; rgnsuy[9][28]=  00.0; rgnsuy[9][29]=  00.0;
        rgnsuy[9][30]=  00.0; rgnsuy[9][31]=  00.0; rgnsuy[9][32]=  00.0;
        rgnsuy[9][33]=  00.0; rgnsuy[9][34]=  00.0;

/*
** showers in montana.
*/
        limits[10]=8;
        strcpy(irtype[10],"shower");

        rgnsux[10][0]=  50.0; rgnsux[10][1]=  49.0; rgnsux[10][2]=  48.0;
        rgnsux[10][3]=  47.0; rgnsux[10][4]=  46.7; rgnsux[10][5]=  48.0;
        rgnsux[10][6]=  50.0; rgnsux[10][7]=  50.0; rgnsux[10][8]=  00.0;
        rgnsux[10][9]=  00.0; rgnsux[10][10]= 00.0; rgnsux[10][11]= 00.0;
        rgnsux[10][12]= 00.0; rgnsux[10][13]= 00.0; rgnsux[10][14]= 00.0;
        rgnsux[10][15]= 00.0; rgnsux[10][16]= 00.0; rgnsux[10][17]= 00.0;
        rgnsux[10][18]= 00.0; rgnsux[10][19]= 00.0; rgnsux[10][20]= 00.0;
        rgnsux[10][21]= 00.0; rgnsux[10][22]= 00.0; rgnsux[10][23]= 00.0;
        rgnsux[10][24]= 00.0; rgnsux[10][25]= 00.0; rgnsux[10][26]= 00.0;
        rgnsux[10][27]= 00.0; rgnsux[10][28]= 00.0; rgnsux[10][29]= 00.0;
        rgnsux[10][30]= 00.0; rgnsux[10][31]= 00.0; rgnsux[10][32]= 00.0;
        rgnsux[10][33]= 00.0; rgnsux[10][34]= 00.0;

        rgnsuy[10][0]= -105.0; rgnsuy[10][1]= -105.0; rgnsuy[10][2]= -106.0;
        rgnsuy[10][3]= -108.0; rgnsuy[10][4]= -110.7; rgnsuy[10][5]= -110.0;
        rgnsuy[10][6]= -106.5; rgnsuy[10][7]= -105.0; rgnsuy[10][8]=  00.0;
        rgnsuy[10][9]=  00.0;  rgnsuy[10][10]= 00.0;  rgnsuy[10][11]= 00.0;
        rgnsuy[10][12]= 00.0;  rgnsuy[10][13]= 00.0;  rgnsuy[10][14]= 00.0;
        rgnsuy[10][15]= 00.0;  rgnsuy[10][16]= 00.0;  rgnsuy[10][17]= 00.0;
        rgnsuy[10][18]= 00.0;  rgnsuy[10][19]= 00.0;  rgnsuy[10][20]= 00.0;
        rgnsuy[10][21]= 00.0;  rgnsuy[10][22]= 00.0;  rgnsuy[10][23]= 00.0;
        rgnsuy[10][24]= 00.0;  rgnsuy[10][25]= 00.0;  rgnsuy[10][26]= 00.0;
        rgnsuy[10][27]= 00.0;  rgnsuy[10][28]= 00.0;  rgnsuy[10][29]= 00.0;
        rgnsuy[10][30]= 00.0;  rgnsuy[10][31]= 00.0;  rgnsuy[10][32]= 00.0;
        rgnsuy[10][33]= 00.0;  rgnsuy[10][34]= 00.0;

/*
** t-storms in southeast
*/
        limits[11]=12;
        strcpy(irtype[11],"thunde");

        rgnsux[11][0]=  36.7; rgnsux[11][1]=  35.0; rgnsux[11][2]=  32.0;
        rgnsux[11][3]=  31.0; rgnsux[11][4]=  31.6; rgnsux[11][5]=  32.2;
        rgnsux[11][6]=  32.8; rgnsux[11][7]=  32.2; rgnsux[11][8]=  32.2;
        rgnsux[11][9]=  34.0; rgnsux[11][10]= 36.0; rgnsux[11][11]= 36.7;
        rgnsux[11][12]= 00.0; rgnsux[11][13]= 00.0; rgnsux[11][14]= 00.0;
        rgnsux[11][15]= 00.0; rgnsux[11][16]= 00.0; rgnsux[11][17]= 00.0;
        rgnsux[11][18]= 00.0; rgnsux[11][19]= 00.0; rgnsux[11][20]= 00.0;
        rgnsux[11][21]= 00.0; rgnsux[11][22]= 00.0; rgnsux[11][23]= 00.0;
        rgnsux[11][24]= 00.0; rgnsux[11][25]= 00.0; rgnsux[11][26]= 00.0;
        rgnsux[11][27]= 00.0; rgnsux[11][28]= 00.0; rgnsux[11][29]= 00.0;
        rgnsux[11][30]= 00.0; rgnsux[11][31]= 00.0; rgnsux[11][32]= 00.0;
        rgnsux[11][33]= 00.0; rgnsux[11][34]= 00.0;

        rgnsuy[11][0]= -76.0; rgnsuy[11][1]=  -78.0; rgnsuy[11][2]=  -81.5;
        rgnsuy[11][3]= -84.0; rgnsuy[11][4]=  -88.0; rgnsuy[11][5]=  -89.5;
        rgnsuy[11][6]= -88.0; rgnsuy[11][7]=  -85.0; rgnsuy[11][8]=  -83.0;
        rgnsuy[11][9]= -80.4; rgnsuy[11][10]= -78.0; rgnsuy[11][11]= -76.0;
        rgnsuy[11][12]= 00.0; rgnsuy[11][13]=  00.0; rgnsuy[11][14]=  00.0;
        rgnsuy[11][15]= 00.0; rgnsuy[11][16]=  00.0; rgnsuy[11][17]=  00.0;
        rgnsuy[11][18]= 00.0; rgnsuy[11][19]=  00.0; rgnsuy[11][20]=  00.0;
        rgnsuy[11][21]= 00.0; rgnsuy[11][22]=  00.0; rgnsuy[11][23]=  00.0;
        rgnsuy[11][24]= 00.0; rgnsuy[11][25]=  00.0; rgnsuy[11][26]=  00.0;
        rgnsuy[11][27]= 00.0; rgnsuy[11][28]=  00.0; rgnsuy[11][29]=  00.0;
        rgnsuy[11][30]= 00.0; rgnsuy[11][31]=  00.0; rgnsuy[11][32]=  00.0;
        rgnsuy[11][33]= 00.0; rgnsuy[11][34]=  00.0;

/*
** t-storms from missouri to new york.
*/
        limits[12]=20;
        strcpy(irtype[12],"thunde");

        rgnsux[12][0]=  35.0; rgnsux[12][1]=  36.5; rgnsux[12][2]=  36.5;
        rgnsux[12][3]=  37.7; rgnsux[12][4]=  38.5; rgnsux[12][5]=  41.0;
        rgnsux[12][6]=  45.0; rgnsux[12][7]=  47.0; rgnsux[12][8]=  49.0;
        rgnsux[12][9]=  48.5; rgnsux[12][10]= 47.0; rgnsux[12][11]= 44.0;
        rgnsux[12][12]= 42.8; rgnsux[12][13]= 41.5; rgnsux[12][14]= 40.0;
        rgnsux[12][15]= 38.0; rgnsux[12][16]= 36.0; rgnsux[12][17]= 34.5;
        rgnsux[12][18]= 34.5; rgnsux[12][19]= 35.0; rgnsux[12][20]= 00.0;
        rgnsux[12][21]= 00.0; rgnsux[12][22]= 00.0; rgnsux[12][23]= 00.0;
        rgnsux[12][24]= 00.0; rgnsux[12][25]= 00.0; rgnsux[12][26]= 00.0;
        rgnsux[12][27]= 00.0; rgnsux[12][28]= 00.0; rgnsux[12][29]= 00.0;
        rgnsux[12][30]= 00.0; rgnsux[12][31]= 00.0; rgnsux[12][32]= 00.0;
        rgnsux[12][33]= 00.0; rgnsux[12][34]= 00.0;

        rgnsuy[12][0]=  -94.0; rgnsuy[12][1]=  -92.0; rgnsuy[12][2]=  -89.0;
        rgnsuy[12][3]=  -84.0; rgnsuy[12][4]=  -79.0; rgnsuy[12][5]=  -77.0;
        rgnsuy[12][6]=  -76.0; rgnsuy[12][7]=  -76.0; rgnsuy[12][8]=  -75.7;
        rgnsuy[12][9]=  -74.5; rgnsuy[12][10]= -73.3; rgnsuy[12][11]= -71.5;
        rgnsuy[12][12]= -70.3; rgnsuy[12][13]= -70.5; rgnsuy[12][14]= -74.0;
        rgnsuy[12][15]= -77.5; rgnsuy[12][16]= -81.0; rgnsuy[12][17]= -87.0; 
        rgnsuy[12][18]= -92.0; rgnsuy[12][19]= -94.0; rgnsuy[12][20]=  00.0;
        rgnsuy[12][21]=  00.0; rgnsuy[12][22]=  00.0; rgnsuy[12][23]=  00.0;
        rgnsuy[12][24]=  00.0; rgnsuy[12][25]=  00.0; rgnsuy[12][26]=  00.0;
        rgnsuy[12][27]=  00.0; rgnsuy[12][28]=  00.0; rgnsuy[12][29]=  00.0;
        rgnsuy[12][30]=  00.0; rgnsuy[12][31]=  00.0; rgnsuy[12][32]=  00.0;

/*
** showers in extreme northeast
*/
        limits[13]=8;
        strcpy(irtype[13],"shower");

        rgnsux[13][0]=  49.5; rgnsux[13][1]=  49.0; rgnsux[13][2]=  47.0;
        rgnsux[13][3]=  44.0; rgnsux[13][4]=  44.0; rgnsux[13][5]=  47.0;
        rgnsux[13][6]=  48.5; rgnsux[13][7]=  49.5; rgnsux[13][8]=  00.0;
        rgnsux[13][9]=  00.0; rgnsux[13][10]= 00.0; rgnsux[13][11]= 00.0;
        rgnsux[13][12]= 00.0; rgnsux[13][13]= 00.0; rgnsux[13][14]= 00.0;
        rgnsux[13][15]= 00.0; rgnsux[13][16]= 00.0; rgnsux[13][17]= 00.0;
        rgnsux[13][18]= 00.0; rgnsux[13][19]= 00.0; rgnsux[13][20]= 00.0;
        rgnsux[13][21]= 00.0; rgnsux[13][22]= 00.0; rgnsux[13][23]= 00.0;
        rgnsux[13][24]= 00.0; rgnsux[13][25]= 00.0; rgnsux[13][26]= 00.0;
        rgnsux[13][27]= 00.0; rgnsux[13][28]= 00.0; rgnsux[13][29]= 00.0;
        rgnsux[13][30]= 00.0; rgnsux[13][31]= 00.0; rgnsux[13][32]= 00.0;
        rgnsux[13][33]= 00.0; rgnsux[13][34]= 00.0;

        rgnsuy[13][0]= -73.8; rgnsuy[13][1]= -71.0; rgnsuy[13][2]= -69.5;
        rgnsuy[13][3]= -69.0; rgnsuy[13][4]= -71.0; rgnsuy[13][5]= -73.0;
        rgnsuy[13][6]= -74.0; rgnsuy[13][7]= -73.8; rgnsuy[13][8]=  00.0;
        rgnsuy[13][9]=  00.0; rgnsuy[13][10]= 00.0; rgnsuy[13][11]= 00.0;
        rgnsuy[13][12]= 00.0; rgnsuy[13][13]= 00.0; rgnsuy[13][14]= 00.0;
        rgnsuy[13][15]= 00.0; rgnsuy[13][16]= 00.0; rgnsuy[13][17]= 00.0;
        rgnsuy[13][18]= 00.0; rgnsuy[13][19]= 00.0; rgnsuy[13][20]= 00.0;
        rgnsuy[13][21]= 00.0; rgnsuy[13][22]= 00.0; rgnsuy[13][23]= 00.0;
        rgnsuy[13][24]= 00.0; rgnsuy[13][25]= 00.0; rgnsuy[13][26]= 00.0;
        rgnsuy[13][27]= 00.0; rgnsuy[13][28]= 00.0; rgnsuy[13][29]= 00.0;
        rgnsuy[13][30]= 00.0; rgnsuy[13][31]= 00.0; rgnsuy[13][32]= 00.0;
        rgnsuy[13][33]= 00.0; rgnsuy[13][34]= 00.0;

/*
** lows and his.
*/
        rlohux[0]=49.0; rlohux[1]=38.5; rlohux[2]=47.5;  
        rlohux[3]=44.0; rlohux[4]=26.5;

        rlohuy[0]= -99.0; rlohuy[1]= -80.5; rlohuy[2]= -127.0; 
        rlohuy[3]= -89.0; rlohuy[4]= -88.0;       

        lowhi[0]=0; lowhi[1]=0; lowhi[2]=1; lowhi[3]=1; lowhi[4]=1;
/*
** labels of regional conditions.
*/
        cndsux[0]= 45.5; cndsux[1]= 41.5; cndsux[2]= 31.5; 
        cndsux[3]= 33.0; cndsux[4]= 45.5;

        cndsuy[0]= -113.0; cndsuy[1]= -99.0; cndsuy[2]= -95.5; 
        cndsuy[3]= -113.2; cndsuy[4]= -82.0;       

        strcpy(icndsl[0], "BREEZY");
        strcpy(icndsl[1], "HOT");
        strcpy(icndsl[2], "STEAMY");
        strcpy(icndsl[3], "HUMID");
        strcpy(icndsl[4], "NICE");
/*
** data on city locations and daily temperature labels.
*/
        strcpy(icitys[0], "NCAR");
        strcpy(icitys[1], "Seattle");
        strcpy(icitys[2], "San Francisco");
        strcpy(icitys[3], "Los Angeles");
        strcpy(icitys[4], "Billings");
        strcpy(icitys[5], "El Paso");
        strcpy(icitys[6], "Houston");
        strcpy(icitys[7], "Kansas City");
        strcpy(icitys[8], "Minneapolis");
        strcpy(icitys[9], "Chicago");
        strcpy(icitys[10], "Detroit");
        strcpy(icitys[11], "Atlanta");
        strcpy(icitys[12], "Miami");
        strcpy(icitys[13], "New York");

        cityux[0]= 40.0; cityux[1]= 47.6; cityux[2]= 37.8; cityux[3]= 34.1; 
        cityux[4]= 45.8; cityux[5]= 31.8; cityux[6]= 29.8; cityux[7]= 39.1; 
        cityux[8]= 45.0; cityux[9]= 41.9; cityux[10]=42.3; cityux[11]=33.8; 
        cityux[12]=25.8; cityux[13]=40.8;

        cityuy[0]= -105.0; cityuy[1]= -122.3; cityuy[2]= -122.4; 
        cityuy[3]= -118.3; cityuy[4]= -108.5; cityuy[5]= -106.5; 
        cityuy[6]=  -95.3; cityuy[7]=  -94.6; cityuy[8]=  -93.8; 
        cityuy[9]=  -87.6; cityuy[10]= -83.1; cityuy[11]= -84.4; 
        cityuy[12]= -80.2; cityuy[13]= -74.0;

        tempux[0]= 38.8; tempux[1]= 46.9; tempux[2]= 38.6; tempux[3]= 35.0; 
        tempux[4]= 46.0; tempux[5]= 32.2; tempux[6]= 28.8; tempux[7]= 37.7; 
        tempux[8]= 46.0; tempux[9]= 40.7; tempux[10]=43.7; tempux[11]=32.4; 
        tempux[12]=27.2; tempux[13]=40.5;

        tempuy[0]=  -103.2; tempuy[1]= -119.6; tempuy[2]=  -120.3; 
        tempuy[3]=  -116.0; tempuy[4]= -105.6; tempuy[5]=  -104.2; 
        tempuy[6]=  -97.4;  tempuy[7]=  -92.4; tempuy[8]=  -91.2; 
        tempuy[9]=  -85.2;  tempuy[10]= -82.8; tempuy[11]= -84.6; 
        tempuy[12]= -81.3;  tempuy[13]= -71.5;

        strcpy(idlyts[0], "92/58");
        strcpy(idlyts[1], "80/58");
        strcpy(idlyts[2], "68/54");
        strcpy(idlyts[3], "80/64");
        strcpy(idlyts[4], "91/58");
        strcpy(idlyts[5], "96/70");
        strcpy(idlyts[6], "98/76");
        strcpy(idlyts[7], "88/70");
        strcpy(idlyts[8], "84/67");
        strcpy(idlyts[9], "84/66");
        strcpy(idlyts[10], "84/63");
        strcpy(idlyts[11], "90/72");
        strcpy(idlyts[12], "92/80");
        strcpy(idlyts[13], "83/70");
/*
** set up color indices to use for temperature regions.
*/
        ic60s= 6; ic70s= 5; ic80s= 4; ic90s= 3; ic100s= 2;
/*
** open gks.
*/
      gopen_gks("stdout",0);
/*
** calls to position the output (applicable only to postscript output).
*/
      c_ngseti("lx",-90);
      c_ngseti("ux",710);
      c_ngseti("ly",-15);
      c_ngseti("uy",785);
      gopen_ws(IWKID, LUNIT, IWTYPE);
      gactivate_ws(IWKID);
 
      color.rgb.red = 1.0; 
      color.rgb.green = 1.0; 
      color.rgb.blue = 1.0;
      gset_colr_rep(IWKID, 0, &color);

      color.rgb.red = 0.0; 
      color.rgb.green = 0.0; 
      color.rgb.blue = 0.0;
      gset_colr_rep(IWKID, 1, &color);
/*
** color for 100 degree temperature regions.
*/
      color.rgb.red = 1.0; 
      color.rgb.green = 0.25; 
      color.rgb.blue = 0.0;
      gset_colr_rep(IWKID, 2, &color);
/*
** color for 90 degree temperature regions.
*/
      color.rgb.red = 1.0; 
      color.rgb.green = 0.5; 
      color.rgb.blue = 0.0;
      gset_colr_rep(IWKID, 3, &color);
/*
** color for 80 degree temperature regions.
*/
      color.rgb.red = 1.0; 
      color.rgb.green = 0.75; 
      color.rgb.blue = 0.0;
      gset_colr_rep(IWKID, 4, &color);
/*
** color for 70 degree temperature regions.
*/
      color.rgb.red = 1.0; 
      color.rgb.green = 1.0; 
      color.rgb.blue = 0.0;
      gset_colr_rep(IWKID, 5, &color);
/*
** color for 60 degree temperature regions.
*/
      color.rgb.red = 0.25; 
      color.rgb.green = 1.0; 
      color.rgb.blue = 0.5;
      gset_colr_rep(IWKID, 6, &color);
 
      color.rgb.red = 0.5;
      color.rgb.green = 0.5;
      color.rgb.blue = 0.5;
      gset_colr_rep(IWKID, 7, &color);

      color.rgb.red = 0.0;
      color.rgb.green = 0.0;
      color.rgb.blue = 1.0;
      gset_colr_rep(IWKID, 8, &color);

      color.rgb.red = 0.0;
      color.rgb.green = 1.0;
      color.rgb.blue = 1.0;
      gset_colr_rep(IWKID, 9, &color);

      color.rgb.red = 0.4;
      color.rgb.green = 0.0;
      color.rgb.blue = 0.4;
      gset_colr_rep(IWKID, 10, &color);
/*
** get world coordinates for the u.s. continental boundary and store
** the u.s. state map in flash buffer 1.  this takes some execution
** time.
*/
      gopen_ws(9, "8", 3);
      wmtint(1,ICDIM,usx,usy,&no);
 
      gset_fill_int_style(1);
      gset_fill_colr_ind(7);
/*
** get world coordinate extents, and plot a shaded offset of the
** continental u.s. boundary.
*/    
      tstext(&xext,&yext);
      for(i=0; i<no; ++i)
      {
        XTmp[i] = usx[i] - .01*xext;
        YTmp[i] = usy[i] - .012*yext;
      }
/*
** create structure to pass to gfill_area
*/
      fill_area.num_points = no;
      fill_area.points = (Gpoint*)malloc(fill_area.num_points*sizeof(Gpoint));
      if( !fill_area.points ) 
      {
            fprintf( stderr, "colram: not enough memory to create");
	    fprintf( stderr, " fill area structure\n" );
            gemergency_close_gks();
            exit(1);
      }
      for( i=0; i<no; i++ ) 
      {
            fill_area.points[i].x = XTmp[i];
            fill_area.points[i].y = YTmp[i];
      }
/*
** fill area
*/
      gfill_area (&fill_area);
      free(fill_area.points);

      gset_fill_colr_ind(4);
/*
** create structure to pass to gfill_area
*/
      fill_area.num_points = no;
      fill_area.points = (Gpoint*)malloc(fill_area.num_points*sizeof(Gpoint));
      if( !fill_area.points )
      {
            fprintf( stderr, "colram: not enough memory to create");
            fprintf( stderr, " fill area structure\n" );
            gemergency_close_gks();
            exit(1);
      }
      for( i=0; i<no; i++ )
      {
            fill_area.points[i].x = usx[i];
            fill_area.points[i].y = usy[i];
      }
/*
** fill area
*/
      gfill_area (&fill_area);
      free(fill_area.points);

/*
**-----------------------------
**| plot temperature regions. |
**-----------------------------
*/
	for(i=0; i<NUMR; ++i)
	{
        	indxmx = limits[i];
		for(j=0; j<indxmx; ++j)
		{
          	      c_maptrn(rgnsux[i][j],rgnsuy[i][j],&rgnswx[j],&rgnswy[j]);
		}
		c_wmdrrg(limits[i],rgnswx,rgnswy,irtype[i],no,usx,usy);
	}
/*
**------------------
**|  u.s. map      |
**------------------
*/
      c_gflas3(1);
/*
**----------------------------
**| plot the weather fronts. |
**----------------------------
*/
      for(i=0; i<limitf[0]; ++i)
      {
       	c_maptrn(frnsux[0][i],frnsuy[0][i],&frnswx[i],&frnswy[i]);
      }
      c_wmsetc("fro","cold");
      c_wmsetr("end",.040);
      c_wmdrft(limitf[0],frnswx,frnswy);
      c_wmdflt();
/*
**  convert to world coordinates.
*/
      for(i=0; i<limitf[1]; ++i)
      {
        c_maptrn(frnsux[1][i],frnsuy[1][i],&frnswx[i],&frnswy[i]);
      }
/*
**  define the type and direction of each symbol.
*/
      c_wmseti("pai", 1);
      c_wmseti("sty",-2);
      c_wmseti("pai", 2);
      c_wmseti("sty",-2);
      c_wmseti("pai", 3);
      c_wmseti("sty", 1);
      c_wmseti("pai", 4);
      c_wmseti("sty",-2);
      c_wmseti("pai", 5);
      c_wmseti("sty", 1);
/*
**  define spacings.
*/
      c_wmsetr("beg",.03);
      c_wmsetr("end",.035);
      c_wmsetr("bet",.04);
/*
**  draw front.
*/
      c_wmdrft(limitf[1],frnswx,frnswy);
/*
**  reset parameters to default values.
*/
      c_wmdflt();
 
      for(i=0; i<limitf[2]; ++i)
      {
        c_maptrn(frnsux[2][i],frnsuy[2][i],&frnswx[i],&frnswy[i]);
      }
      c_wmsetc("fro","sta");
      c_wmsetr("beg",.040);
      c_wmseti("rev",1);
      c_wmdrft(limitf[2],frnswx,frnswy);
      c_wmdflt();
 
      for(i=0; i<limitf[3]; ++i)
      {
        c_maptrn(frnsux[3][i],frnsuy[3][i],&frnswx[i],&frnswy[i]);
      }
      c_wmsetc("fro","cold");
      c_wmsetr("beg",.040);
      c_wmsetr("bet",.030);
      c_wmdrft(limitf[3],frnswx,frnswy);
      c_wmdflt();
/*
**----------------
**| los and his  |
**----------------
*/
      for(i=0; i<NUML; ++i)
      {
        	c_maptrn(rlohux[i],rlohuy[i],&xo,&yo);
        	if (lowhi[i] == 0)
		{
          		c_wmlabs(xo,yo,"LOW");
		}
        	else
		{
          		c_wmlabs(xo,yo,"HI");
		}
     }
/*
**-------------------------------
**| regional condition labels.  |
**-------------------------------
*/ 
	for(i=0; i<NUMWL; ++i)
	{
        	c_maptrn(cndsux[i],cndsuy[i],&xo,&yo);
        	ll = c_wmgtln(icndsl[i],strlen(icndsl[i]),0);
        	c_wmlabw(xo,yo,icndsl[i]);
	}
/* 
**-----------------------------
**| cities and temperatures.  |
**-----------------------------
**
**  NUMC    - the number of cities.
**  icitys  - city names.
**  idlyts  - daily hi/low labels for cities.
**  cityux  - x user coordinates for city locations.
**  cityuy  - y user coordinates for city locations.
**  tempux  - x user coordinates for daily hi/low locations.
**  tempuy  - y user coordinates for daily hi/low locations.
*/

        c_wmseti("dtc",8);
	for(i=0; i<NUMC; ++i)
	{
        	c_maptrn(cityux[i],cityuy[i],&xo,&yo);
        	c_wmlabs(xo,yo,"D");
        	c_maptrn(tempux[i],tempuy[i],&xo,&yo);
        	ll = c_wmgtln(icitys[i],strlen(icitys[i]),0);
        	mm = c_wmgtln(idlyts[i],strlen(idlyts[i]),0);
        	c_wmlabc(xo,yo,&icitys[i][0],&idlyts[i][0]);
	}
/*
**---------------------------------
**| regional temperature labels.  |
**---------------------------------
**
**  s. ariz.
*/
      c_maptrn(32.,-112.,&xo,&yo);
      c_wmlabt(xo,yo,"100s",2);
/*
**  s. calif.
*/
      c_wmsetr("ard",65.);
      c_wmsetr("arl",1.2);
      c_maptrn(34.9,-120.,&xo,&yo);
      c_wmlabs(xo,yo,"arrow");
      c_maptrn(32.8,-116.9,&xo,&yo);
      c_wmdflt();
      c_wmlabt(xo,yo,"70s",1);
/*
**  oregon pacific coast.
*/
      c_maptrn(43.,-123.9,&xo,&yo);
      c_wmlabt(xo,yo,"60s",11);
/*
**  idaho
*/
      c_maptrn(48.25,-114.75,&xo,&yo);
      c_wmlabt(xo,yo,"70s",8);
/*
**  gt. lakes
*/
      c_maptrn(47.7,-87.,&xo,&yo);
      c_wmlabt(xo,yo,"70s",8);
/*
**  n. carolina
*/
      c_maptrn(35.0,-78.,&xo,&yo);
      c_wmlabt(xo,yo,"90s",6);
/*
**  maine
*/
      c_maptrn(46.5,-68.5,&xo,&yo);
      c_wmlabt(xo,yo,"70s",9);
/*
**  texas
*/
      c_wmseti("rfc",0);
      c_maptrn(31.25,-100.50,&xo,&yo);
      c_wmlabt(xo,yo,"90s",0);
/*
**  s. of okla.
*/
      c_maptrn(34.3,-97.5,&xo,&yo);
      c_wmlabt(xo,yo,"100s",11);
/*
**  n. ariz./new mexico
*/
      c_maptrn(36.0,-109.0,&xo,&yo);
      c_wmlabt(xo,yo,"80s",0);
/*
**  oregon
*/
      c_maptrn(43.7,-120.5,&xo,&yo);
      c_wmlabt(xo,yo,"90s",0);
/*
**  utah
*/
      c_maptrn(40.0,-110.6,&xo,&yo);
      c_wmlabt(xo,yo,"90s",0);
/*
**  n. montana.
*/
      c_maptrn(48.0,-111.,&xo,&yo);
      c_wmlabt(xo,yo,"80s",0);
/*
**  s. dakota
*/
      c_maptrn(44.5,-100.0,&xo,&yo);
      c_wmlabt(xo,yo,"90s",0);
/*
**  iowa/ill.
*/
      c_maptrn(41.5,-89.6,&xo,&yo);
      c_wmlabt(xo,yo,"80s",0);
/*
**  miss.
*/
      c_maptrn(33.4,-89.6,&xo,&yo);
      c_wmlabt(xo,yo,"90s",0);
/*
**  tenn.
*/
      c_maptrn(35.7,-83.,&xo,&yo);
      c_wmlabt(xo,yo,"80s",0);
/*
**  new york
*/
      c_maptrn(42.7,-75.0,&xo,&yo);
      c_wmlabt(xo,yo,"80s",0);
/*
**-----------------
**|  main title.  |
**-----------------
*/
      c_maptrn(53.,-98.0,&xo,&yo);
      c_wmseti("rfc",1);
      c_wmlabt(xo,yo,"July 18, 1994",0);
/*
**--------------
**|  legends.  |
**--------------
*/
      gsel_norm_tran(0);
      c_wmlgnd(.05,0.16,1,6,1);
      c_wmlgnd(.45,0.15,3,0,0);
      c_wmlgnd(.90,0.15,2,0,0);
 
      c_frame();
 
      gdeactivate_ws(IWKID);
      gclose_ws(9);
      gclose_ws(IWKID);
      gclose_gks();
      return 0;
}


#define LAMA 100000 
int IEodf;

void wmtint(ibnum,n,usx,usy,no)
int ibnum, n;
float *usx, *usy;
int* no;
{
/*
**  does some initialization.  stores a map of the u.s. state outlines
**  in flash buffer number ibnum, and returns world coordinate values
**  for the boundary of the u.s. in (usx(i),usy(i),i=1,no).  usx and
**  usy are dimensioned for n and no is returned as the actual number
**  of coordinates in the dataset.  n should be at least 6000.
**
**  this routine also initializes the ezmap parameters.
**
**  define space for the area map that is used to obtain the
**  coordinates for the boundary of the continental u.s.  set up some
**  arrays to be used by areas in obtaining the u.s. continental outline.
**  the merged polygons from areas are stored in common mergcm.
**  rtptar is a subroutine used by areas to process the areas.
*/
        int i, iama[LAMA];
        float xcra[10000],ycra[10000];
        float p1[2], p2[2], p3[2], p4[2];
        int iaai[10],iagi[10];
        extern int rtptar();
/*
**  set up the parameters for drawing the u.s. state outlines.
**
**   position the plot.
*/
        c_mappos(0.05, 0.95, 0.05, 0.95);
/*
**   specify u.s. state outlines.
*/
        c_mapstc("ou","us");
/*
**   choose lambert conformal projection with two standard parallels.
*/
        c_maproj("lc",30.,-100.,45.);
/*
**   reduce the value of "mv" to make what mapdrw produces match what
**   comes out of mapbla/arscam.
*/
        c_mapsti ("mv",1);
/*
**   specify the corner points of the plot as lat/lon pairs.
*/
        p1[0] = 22.6;
        p2[0] = -120.0;
        p3[0] = 46.9;
        p4[0] = -64.2;
        c_mapset("co",p1,p2,p3,p4);
/*
**   initialize the transformations.
*/
        c_mapint();
/*
**  set the flag for mapeod to just consider boundary lines.
*/
        c_gflas1(ibnum);
        IEodf = 1;
        c_maplot();
/*
** initialize the area map, and put selected outlines in the area map.
** (a version of mapeod is supplied which causes anything not part of
** the outer boundary of the u.s. to be omitted).
*/
        c_arinam (iama,LAMA);
        c_mapbla (iama);
        NDum=0;
/*
** scan the area map using rtptar to fill all the polygons representing
** the u.s.  merge the polygons into a single polygon in mergcm.
*/
        c_arscam (iama,xcra,ycra,10000,iaai,iagi,10,rtptar);
/*
** convert to world coordinates.
*/
        for( i = 0; i < NDum; i++ ) {
                usx[i] = c_cfux(XTmp[i]);
                usy[i] = c_cfuy(YTmp[i]);
        }
        IEodf = 0;
        c_maplot();
        c_gflas2();
        *no = NDum;

        return;
}


void NGCALLF(mapeod,MAPEOD)(nout,nseg,idls,idrs,npts,pnts)
int *nout, *nseg, *idls, *idrs, *npts;
float* pnts;
{
/*
** this version of mapeod omits all parts of the "us" outline dataset
** which are strictly internal, saving only those which are part of
** the external boundary.  (the "border" has area identifier "223";
** we omit anything that is not on the "border".)
*/
      if (IEodf == 1)
      {
           if ((*idls!=223) && (*idrs!=223)) *npts=0;
      }
}



int rtptar (xcra,ycra,ncra,iaai,iagi,nofg)
float *xcra, *ycra;
int *ncra, *iaai, *iagi, *nofg;
{
    int i, iag1;
    extern void mergpo();
/*
**  Calls MERGPO to merge all such polygons into one polygon.
**
**  Find the area identifier of the area relative to group 1.
*/
    iag1 = -1;

    for( i = 0; i < *nofg; i++ ) 
    {
        if (iagi[i] == 1) iag1=iaai[i];
    }
/*
**  If the index of the area relative to group 1 is positive and not 223,
**  fill it.
*/
    if (iag1 > 0 && iag1 != 223) 
    {
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
**  This routine merges polygons into a single polygon.
**
**  Copy the coordinates of the latest polygon into the merge polygon
**  coordinate arrays and, if the polygon is not the first of the group,
**  repeat the first point of the first polygon.  (Actually, the code
**  below does something a little more complicated: if necessary, it
**  interpolates points to ensure that the connecting lines between
**  polygons consist of horizontal and/or vertical steps; this tends
**  to prevent problems caused by deficiencies in the fill algorithms
**  on some devices.)
**
**  The following statement is a quick-and-dirty patch to leave out all
**  the off-shore islands, since they seem to cause some problems.
*/
        if (ncra < 100) return;

        ntmp=NDum;

        if (ntmp+ncra+4 <= 9999) 
        {
            if (NDum != 0) 
            {
                if (XTmp[ntmp-1] != xcra[0] && YTmp[ntmp-1] != ycra[0])
		{
                	if (YTmp[ntmp-1] < ycra[0]) 
			{
                         	ntmp=ntmp+1;
                                XTmp[ntmp-1]=xcra[0];
                                YTmp[ntmp-1]=YTmp[ntmp-2];
                        }
                        else 
			{
                                ntmp=ntmp+1;
                                XTmp[ntmp-1]=XTmp[ntmp-2];
                                YTmp[ntmp-1]=ycra[0];
                        }
                }
                ntmp=ntmp+1;
                XTmp[ntmp-1]=xcra[0];
                YTmp[ntmp-1]=ycra[0];
          }
          for( icra=1; icra <= ncra; icra++ ) 
	  {
                XTmp[ntmp+icra]=xcra[icra];
                YTmp[ntmp+icra]=ycra[icra];
          }
          ntmp=ntmp+ncra;
          if (NDum != 0) 
	  {
                if (XTmp[ntmp-1] != XTmp[0] && YTmp[ntmp-1] != YTmp[0])
		{
                        if (YTmp[ntmp-1] < YTmp[0]) 
			{
                                 ntmp=ntmp+1;
                                 XTmp[ntmp-1]=XTmp[0];
                                 YTmp[ntmp-1]=YTmp[ntmp-2];
                        }
                        else 
			{
                                 ntmp=ntmp+1;
                                 XTmp[ntmp-1]=XTmp[ntmp-2];
                                 YTmp[ntmp-1]=YTmp[0];
                        }
                 }
                 ntmp=ntmp+1;
                 XTmp[ntmp-1]=XTmp[0];
                 YTmp[ntmp-1]=YTmp[0];
          }
    }
    else 
    {
        ntmp=10000;
    }

    NDum=ntmp;
    return;
}


void tstext(xext,yext)
float *xext, *yext;
{
/*
**  calculate the world coordinate extents.
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

