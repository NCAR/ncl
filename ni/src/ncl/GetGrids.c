#include <stdlib.h>
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/NresDB.h>
#include "defs.h"
#include <netcdf.h>
#include "NclDataDefs.h"
#include "NclFileInterfaces.h"
#include "NclMdInc.h"
#include "DataSupport.h"
#include "date.h"
#include "NclGRIB.h"
#include <math.h>


int grid_index[] = { 1, 2, 3, 4, 5, 6, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 33, 34, 37, 38, 39, 40, 41, 42, 43, 44, 45, 50, 55, 56, 61, 62, 63, 64, 75, 76, 77, 85, 86, 87, 90, 91, 92, 93, 98, 100, 101, 103, 104, 105, 106, 107, 126, 201, 202, 203, 204, 205, 206,207, 208, 209, 210, 211, 212, 213, 214 };

int grid_tbl_len = sizeof(grid_index)/sizeof(int);

int grid_gds_index[] = { 0, 1, 2, 3, 4, 5, 13, 50, 90, 201, 202 };

int grid_gds_tbl_len = sizeof(grid_gds_index)/sizeof(int);


#define EAR 6371.2213
#define PI 3.14159265
#define PI4  (PI/4)
#define RADDEG (180./PI)
#define EAST 1
#define WEST -1

static float er;
static float er2;
static float xax;
static float xpp;
static float ypp;
static int southern;

static void grdsetup(float x,float y,float gsp,float d,float ax) 
{
	if(d< 0) {
		southern = -1;
	} else {
		southern = 1;
	}
	er =  EAR * (1.0 + sin(fabs(d)/RADDEG)) / gsp;
	er2 = er * er;
	xax = ax;
	xpp = x;
	ypp = y;
	return;
}


static void grdloc(float xp,float yp, float *xlo, float* xla)
{
	float r2,ss;
	float yy,xx,elong;


	yy = yp - ypp;
	xx = xp - xpp;
	*xlo = 0.0;
	if((yy != 0.0)||(xx!=0) ) {
		elong = RADDEG * atan2(yy,xx);
		*xlo =  elong + xax;
		if(*xlo > 180.0) 
			*xlo = *xlo - 360.0;
		if(*xlo < -180.0)
			*xlo = *xlo + 360;
	} 
	r2 = xx * xx + yy * yy;
	ss = (er2-r2)/(er2+r2);
	*xla = southern * RADDEG * asin(ss);
} 

static printbinary(int val) {
	
	(val & 020000000000) ? fprintf(stdout,"1") : fprintf(stdout,"0");
	(val & 010000000000) ? fprintf(stdout,"1") : fprintf(stdout,"0");
	(val & 004000000000) ? fprintf(stdout,"1") : fprintf(stdout,"0");
	(val & 002000000000) ? fprintf(stdout,"1") : fprintf(stdout,"0");
	(val & 001000000000) ? fprintf(stdout,"1") : fprintf(stdout,"0");
	(val & 000400000000) ? fprintf(stdout,"1") : fprintf(stdout,"0");
	(val & 000200000000) ? fprintf(stdout,"1") : fprintf(stdout,"0");
	(val & 000100000000) ? fprintf(stdout,"1") : fprintf(stdout,"0");
	(val & 000040000000) ? fprintf(stdout,"1") : fprintf(stdout,"0");
	(val & 000020000000) ? fprintf(stdout,"1") : fprintf(stdout,"0");
	(val & 000010000000) ? fprintf(stdout,"1") : fprintf(stdout,"0");
	(val & 000004000000) ? fprintf(stdout,"1") : fprintf(stdout,"0");
	(val & 000002000000) ? fprintf(stdout,"1") : fprintf(stdout,"0");
	(val & 000001000000) ? fprintf(stdout,"1") : fprintf(stdout,"0");
	(val & 000000400000) ? fprintf(stdout,"1") : fprintf(stdout,"0");
	(val & 000000200000) ? fprintf(stdout,"1") : fprintf(stdout,"0");
	(val & 000000100000) ? fprintf(stdout,"1") : fprintf(stdout,"0");
	(val & 000000040000) ? fprintf(stdout,"1") : fprintf(stdout,"0");
	(val & 000000020000) ? fprintf(stdout,"1") : fprintf(stdout,"0");
	(val & 000000010000) ? fprintf(stdout,"1") : fprintf(stdout,"0");
	(val & 000000004000) ? fprintf(stdout,"1") : fprintf(stdout,"0");
	(val & 000000002000) ? fprintf(stdout,"1") : fprintf(stdout,"0");
	(val & 000000001000) ? fprintf(stdout,"1") : fprintf(stdout,"0");
	(val & 000000000400) ? fprintf(stdout,"1") : fprintf(stdout,"0");
	(val & 000000000200) ? fprintf(stdout,"1") : fprintf(stdout,"0");
	(val & 000000000100) ? fprintf(stdout,"1") : fprintf(stdout,"0");
	(val & 000000000040) ? fprintf(stdout,"1") : fprintf(stdout,"0");
	(val & 000000000020) ? fprintf(stdout,"1") : fprintf(stdout,"0");
	(val & 000000000010) ? fprintf(stdout,"1") : fprintf(stdout,"0");
	(val & 000000000004) ? fprintf(stdout,"1") : fprintf(stdout,"0");
	(val & 000000000002) ? fprintf(stdout,"1") : fprintf(stdout,"0");
	(val & 000000000001) ? fprintf(stdout,"1") : fprintf(stdout,"0");
	fprintf(stdout,"\n");
}
/*
* START lat lon grids
*/
void GenLatLon 
#if NhlNeedProto
(GribParamList* thevarrec, float** lat, int* n_dims_lat, int** dimsizes_lat, float** lon, int* n_dims_lon, int** dimsizes_lon,int xsize,int ysize, float lon_start,float lat_start, float lon_dir, float lat_dir)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon,xsize,ysize, lon_start,lat_start, lon_dir, lat_dir)
)
GribParamList* thevarrec;
float** lat;
int* n_dims_lat;
int** dimsizes_lat;
float** lon;
int* n_dims_lon;
int** dimsizes_lon;
int xsize;
int ysize;
float lon_start;
float lat_start;
float lon_dir;
float lat_dir;
#endif
{
	int j;

	*lat = (float*)NclMalloc(sizeof(float)*ysize);
	*lon = (float*)NclMalloc(sizeof(float)*xsize);
        *dimsizes_lat = (int*)NclMalloc(sizeof(int));
        *dimsizes_lon = (int*)NclMalloc(sizeof(int));
        *n_dims_lat = 1;
        *n_dims_lon = 1;
        (*dimsizes_lat)[0] = ysize;
        (*dimsizes_lon)[0] = xsize;
	for(j = 0; j < ysize; j++) {
		(*lat)[j] = lat_start + lat_dir * j;
	}
	for(j = 0; j < xsize; j++) {
		(*lon)[j] = lon_start + lon_dir * j;
	}
	return;
}
void GetGrid_86
#if NhlNeedProto
(GribParamList* thevarrec, float** lat, int* n_dims_lat, int** dimsizes_lat, float** lon, int* n_dims_lon, int** dimsizes_lon)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon)
GribParamList* thevarrec;
float** lat;
int* n_dims_lat;
int** dimsizes_lat;
float** lon;
int* n_dims_lon;
int** dimsizes_lon;
#endif
{
        GenLatLon(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, 360, 90, 0.5, -89.5 , 1.0, 1.0);
        return;
}

void GetGrid_85
#if NhlNeedProto
(GribParamList* thevarrec, float** lat, int* n_dims_lat, int** dimsizes_lat, float** lon, int* n_dims_lon, int** dimsizes_lon)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon)
GribParamList* thevarrec;
float** lat;
int* n_dims_lat;
int** dimsizes_lat;
float** lon;
int* n_dims_lon;
int** dimsizes_lon;
#endif
{
        GenLatLon(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, 360, 90, 0.5, 0.5 , 1.0, 1.0);
        return;
}
void GetGrid_64
#if NhlNeedProto
(GribParamList* thevarrec, float** lat, int* n_dims_lat, int** dimsizes_lat, float** lon, int* n_dims_lon, int** dimsizes_lon)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon)
GribParamList* thevarrec;
float** lat;
int* n_dims_lat;
int** dimsizes_lat;
float** lon;
int* n_dims_lon;
int** dimsizes_lon;
#endif
{
        GenLatLon(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, 91, 46, -180.0, -90.0, 2.0, 2.0);
        return;
}

void GetGrid_63
#if NhlNeedProto
(GribParamList* thevarrec, float** lat, int* n_dims_lat, int** dimsizes_lat, float** lon, int* n_dims_lon, int** dimsizes_lon)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon)
GribParamList* thevarrec;
float** lat;
int* n_dims_lat;
int** dimsizes_lat;
float** lon;
int* n_dims_lon;
int** dimsizes_lon;
#endif
{
        GenLatLon(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, 91, 46, 0.0, -90.0, 2.0, 2.0);
        return;
}

void GetGrid_62
#if NhlNeedProto
(GribParamList* thevarrec, float** lat, int* n_dims_lat, int** dimsizes_lat, float** lon, int* n_dims_lon, int** dimsizes_lon)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon)
GribParamList* thevarrec;
float** lat;
int* n_dims_lat;
int** dimsizes_lat;
float** lon;
int* n_dims_lon;
int** dimsizes_lon;
#endif
{
        GenLatLon(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, 91, 46, -180.0, 0.0, 2.0, 2.0);
        return;
}

void GetGrid_61
#if NhlNeedProto
(GribParamList* thevarrec, float** lat, int* n_dims_lat, int** dimsizes_lat, float** lon, int* n_dims_lon, int** dimsizes_lon)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon)
GribParamList* thevarrec;
float** lat;
int* n_dims_lat;
int** dimsizes_lat;
float** lon;
int* n_dims_lon;
int** dimsizes_lon;
#endif
{
        GenLatLon(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, 91, 46, 0.0, 0.0, 2.0, 2.0);
        return;
}

void GetGrid_50
#if NhlNeedProto
(GribParamList* thevarrec, float** lat, int* n_dims_lat, int** dimsizes_lat, float** lon, int* n_dims_lon, int** dimsizes_lon)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon)
GribParamList* thevarrec;
float** lat;
int* n_dims_lat;
int** dimsizes_lat;
float** lon;
int* n_dims_lon;
int** dimsizes_lon;
#endif
{
        GenLatLon(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, 36, 33, -140.0, 20.0, 2.5, 1.25);
        return;
}

void GetGrid_45
#if NhlNeedProto
(GribParamList* thevarrec, float** lat, int* n_dims_lat, int** dimsizes_lat, float** lon, int* n_dims_lon, int** dimsizes_lon)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon)
GribParamList* thevarrec;
float** lat;
int* n_dims_lat;
int** dimsizes_lat;
float** lon;
int* n_dims_lon;
int** dimsizes_lon;
#endif
{
        GenLatLon(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, 288, 145, 0.0, 90.0, 1.25, -1.25);
        return;
}

void GetGrid_34
#if NhlNeedProto
(GribParamList* thevarrec, float** lat, int* n_dims_lat, int** dimsizes_lat, float** lon, int* n_dims_lon, int** dimsizes_lon)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon)
GribParamList* thevarrec;
float** lat;
int* n_dims_lat;
int** dimsizes_lat;
float** lon;
int* n_dims_lon;
int** dimsizes_lon;
#endif
{
        GenLatLon(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, 181, 46, 0.0, -90.0, 2, 2);
        return;
}

void GetGrid_33
#if NhlNeedProto
(GribParamList* thevarrec, float** lat, int* n_dims_lat, int** dimsizes_lat, float** lon, int* n_dims_lon, int** dimsizes_lon)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon)
GribParamList* thevarrec;
float** lat;
int* n_dims_lat;
int** dimsizes_lat;
float** lon;
int* n_dims_lon;
int** dimsizes_lon;
#endif
{
        GenLatLon(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, 181, 46, 0.0, 0.0, 2, 2);
        return;
}

void GetGrid_30
#if NhlNeedProto
(GribParamList* thevarrec, float** lat, int* n_dims_lat, int** dimsizes_lat, float** lon, int* n_dims_lon, int** dimsizes_lon)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon)
GribParamList* thevarrec;
float** lat;
int* n_dims_lat;
int** dimsizes_lat;
float** lon;
int* n_dims_lon;
int** dimsizes_lon;
#endif
{
        GenLatLon(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, 145, 37, 0.0, -90.0, 2.5, 2.5);
        return;
}
void GetGrid_29
#if NhlNeedProto
(GribParamList* thevarrec, float** lat, int* n_dims_lat, int** dimsizes_lat, float** lon, int* n_dims_lon, int** dimsizes_lon)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon)
GribParamList* thevarrec;
float** lat;
int* n_dims_lat;
int** dimsizes_lat;
float** lon;
int* n_dims_lon;
int** dimsizes_lon;
#endif
{
        GenLatLon(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, 145, 37, 0.0, 0.0, 2.5, 2.5);
        return;
}
void GetGrid_26
#if NhlNeedProto
(GribParamList* thevarrec, float** lat, int* n_dims_lat, int** dimsizes_lat, float** lon, int* n_dims_lon, int** dimsizes_lon)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon)
GribParamList* thevarrec;
float** lat;
int* n_dims_lat;
int** dimsizes_lat;
float** lon;
int* n_dims_lon;
int** dimsizes_lon;
#endif
{
        GenLatLon(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, 72, 19, 0.0, -90.0, 5.0, 5.0);
        return;
}

void GetGrid_25
#if NhlNeedProto
(GribParamList* thevarrec, float** lat, int* n_dims_lat, int** dimsizes_lat, float** lon, int* n_dims_lon, int** dimsizes_lon)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon)
GribParamList* thevarrec;
float** lat;
int* n_dims_lat;
int** dimsizes_lat;
float** lon;
int* n_dims_lon;
int** dimsizes_lon;
#endif
{
        GenLatLon(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, 72, 19, 0.0, 0.0, 5.0, 5.0);
        return;
}

void GetGrid_24
#if NhlNeedProto
(GribParamList* thevarrec, float** lat, int* n_dims_lat, int** dimsizes_lat, float** lon, int* n_dims_lon, int** dimsizes_lon)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon)
GribParamList* thevarrec;
float** lat;
int* n_dims_lat;
int** dimsizes_lat;
float** lon;
int* n_dims_lon;
int** dimsizes_lon;
#endif
{
        GenLatLon(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, 37, 37, -180.0, -90.0, 5.0, 2.5);
        return;
}

void GetGrid_23
#if NhlNeedProto
(GribParamList* thevarrec, float** lat, int* n_dims_lat, int** dimsizes_lat, float** lon, int* n_dims_lon, int** dimsizes_lon)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon)
GribParamList* thevarrec;
float** lat;
int* n_dims_lat;
int** dimsizes_lat;
float** lon;
int* n_dims_lon;
int** dimsizes_lon;
#endif
{
        GenLatLon(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, 37, 37, 0.0, -90.0, 5.0, 2.5);
        return;
}

void GetGrid_22
#if NhlNeedProto
(GribParamList* thevarrec, float** lat, int* n_dims_lat, int** dimsizes_lat, float** lon, int* n_dims_lon, int** dimsizes_lon)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon)
GribParamList* thevarrec;
float** lat;
int* n_dims_lat;
int** dimsizes_lat;
float** lon;
int* n_dims_lon;
int** dimsizes_lon;
#endif
{
        GenLatLon(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, 37, 37, -180.0, 0.0, 5.0, 2.5);
        return;
}


void GetGrid_21
#if NhlNeedProto
(GribParamList* thevarrec, float** lat, int* n_dims_lat, int** dimsizes_lat, float** lon, int* n_dims_lon, int** dimsizes_lon)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon)
GribParamList* thevarrec;
float** lat;
int* n_dims_lat;
int** dimsizes_lat;
float** lon;
int* n_dims_lon;
int** dimsizes_lon;
#endif
{
        GenLatLon(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, 37, 37, 0.0, 0.0, 5.0, 2.5);
        return;
}


void GetGrid_4
#if NhlNeedProto
(GribParamList* thevarrec, float** lat, int* n_dims_lat, int** dimsizes_lat, float** lon, int* n_dims_lon, int** dimsizes_lon)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon)
GribParamList* thevarrec;
float** lat;
int* n_dims_lat;
int** dimsizes_lat;
float** lon;
int* n_dims_lon;
int** dimsizes_lon;
#endif
{
        GenLatLon(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, 720, 361, 0.0, 90.0, .5, -.5);
        return;
}

void GetGrid_3
#if NhlNeedProto
(GribParamList* thevarrec, float** lat, int* n_dims_lat, int** dimsizes_lat, float** lon, int* n_dims_lon, int** dimsizes_lon)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon)
GribParamList* thevarrec;
float** lat;
int* n_dims_lat;
int** dimsizes_lat;
float** lon;
int* n_dims_lon;
int** dimsizes_lon;
#endif
{
        GenLatLon(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, 360, 181, 0.0, 90.0, 1.0, -1.0);
        return;
}

void GetGrid_2
#if NhlNeedProto
(GribParamList* thevarrec, float** lat, int* n_dims_lat, int** dimsizes_lat, float** lon, int* n_dims_lon, int** dimsizes_lon)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon)
GribParamList* thevarrec;
float** lat;
int* n_dims_lat;
int** dimsizes_lat;
float** lon;
int* n_dims_lon;
int** dimsizes_lon;
#endif
{
	GenLatLon(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, 144, 73, 0.0, 90.0, 2.5, -2.5);
	return;
}


/*
* END lat lon grids
*/
/*
* START Polar Stereographic GRIDS
*/
void GetGrid_214
#if NhlNeedProto
(GribParamList* thevarrec, float** lat, int* n_dims_lat, int** dimsizes_lat, float** lon, int* n_dims_lon, int** dimsizes_lon)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon)
GribParamList* thevarrec;
float** lat;
int* n_dims_lat;
int** dimsizes_lat;
float** lon;
int* n_dims_lon;
int** dimsizes_lon;
#endif
{
/*
* Very tricky need to figure out how to exchange dimensions correctly
*/
        int xsize = 97;
        int ysize = 69;
        float polex = 49;
        float poley = 101;
        float dist = 47.625;
        float deg = 60.0;
        float ore = -150.0;
        int x,y;


        *lat = (float*)NclMalloc(sizeof(float) * xsize * ysize);
        *lon= (float*)NclMalloc(sizeof(float) * xsize * ysize);
        *dimsizes_lat = (int*)NclMalloc(sizeof(int) * 2);
        *dimsizes_lon = (int*)NclMalloc(sizeof(int) * 2);
        *n_dims_lat = 2;
        *n_dims_lon = 2;
        (*dimsizes_lat)[0] = ysize;
        (*dimsizes_lat)[1] = xsize;
        (*dimsizes_lon)[0] = ysize;
        (*dimsizes_lon)[1] = xsize;

        grdsetup(polex,poley,dist,deg, ore + 90.0 );
        for (y = 0; y < ysize; y++) {
                for(x = 0; x < xsize; x++) {
                        grdloc(x,y,&((*lon)[y * ysize + x]),&((*lat)[y * ysize + x]));
                }
        }

}

void GetGrid_213
#if NhlNeedProto
(GribParamList* thevarrec, float** lat, int* n_dims_lat, int** dimsizes_lat, float** lon, int* n_dims_lon, int** dimsizes_lon)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon)
GribParamList* thevarrec;
float** lat;
int* n_dims_lat;
int** dimsizes_lat;
float** lon;
int* n_dims_lon;
int** dimsizes_lon;
#endif
{
/*
* Very tricky need to figure out how to exchange dimensions correctly
*/
        int xsize = 129;
        int ysize = 85;
        float polex = 65;
        float poley = 89;
        float dist = 95.250;
        float deg = 60.0;
        float ore = -105.0;
        int x,y;


        *lat = (float*)NclMalloc(sizeof(float) * xsize * ysize);
        *lon= (float*)NclMalloc(sizeof(float) * xsize * ysize);
        *dimsizes_lat = (int*)NclMalloc(sizeof(int) * 2);
        *dimsizes_lon = (int*)NclMalloc(sizeof(int) * 2);
        *n_dims_lat = 2;
        *n_dims_lon = 2;
        (*dimsizes_lat)[0] = ysize;
        (*dimsizes_lat)[1] = xsize;
        (*dimsizes_lon)[0] = ysize;
        (*dimsizes_lon)[1] = xsize;

        grdsetup(polex,poley,dist,deg, ore + 90.0 );
        for (y = 0; y < ysize; y++) {
                for(x = 0; x < xsize; x++) {
                        grdloc(x,y,&((*lon)[y * ysize + x]),&((*lat)[y * ysize + x]));
                }
        }

}

void GetGrid_207
#if NhlNeedProto
(GribParamList* thevarrec, float** lat, int* n_dims_lat, int** dimsizes_lat, float** lon, int* n_dims_lon, int** dimsizes_lon)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon)
GribParamList* thevarrec;
float** lat;
int* n_dims_lat;
int** dimsizes_lat;
float** lon;
int* n_dims_lon;
int** dimsizes_lon;
#endif
{
/*
* Very tricky need to figure out how to exchange dimensions correctly
*/
        int xsize = 49;
        int ysize = 35;
        float polex = 25;
        float poley = 51;
        float dist = 95.250;
        float deg = 60.0;
        float ore = -150.0;
        int x,y;


        *lat = (float*)NclMalloc(sizeof(float) * xsize * ysize);
        *lon= (float*)NclMalloc(sizeof(float) * xsize * ysize);
        *dimsizes_lat = (int*)NclMalloc(sizeof(int) * 2);
        *dimsizes_lon = (int*)NclMalloc(sizeof(int) * 2);
        *n_dims_lat = 2;
        *n_dims_lon = 2;
        (*dimsizes_lat)[0] = ysize;
        (*dimsizes_lat)[1] = xsize;
        (*dimsizes_lon)[0] = ysize;
        (*dimsizes_lon)[1] = xsize;

        grdsetup(polex,poley,dist,deg, ore + 90.0 );
        for (y = 0; y < ysize; y++) {
                for(x = 0; x < xsize; x++) {
                        grdloc(x,y,&((*lon)[y * ysize + x]),&((*lat)[y * ysize + x]));
                }
        }

}

void GetGrid_205
#if NhlNeedProto
(GribParamList* thevarrec, float** lat, int* n_dims_lat, int** dimsizes_lat, float** lon, int* n_dims_lon, int** dimsizes_lon)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon)
GribParamList* thevarrec;
float** lat;
int* n_dims_lat;
int** dimsizes_lat;
float** lon;
int* n_dims_lon;
int** dimsizes_lon;
#endif
{
/*
* Very tricky need to figure out how to exchange dimensions correctly
*/
        int xsize = 45;
        int ysize = 39;
        float polex = 27;
        float poley = 167;
        float dist = 57;
        float deg = 60.0;
        float ore = -60.0;
        int x,y;


        *lat = (float*)NclMalloc(sizeof(float) * xsize * ysize);
        *lon= (float*)NclMalloc(sizeof(float) * xsize * ysize);
        *dimsizes_lat = (int*)NclMalloc(sizeof(int) * 2);
        *dimsizes_lon = (int*)NclMalloc(sizeof(int) * 2);
        *n_dims_lat = 2;
        *n_dims_lon = 2;
        (*dimsizes_lat)[0] = ysize;
        (*dimsizes_lat)[1] = xsize;
        (*dimsizes_lon)[0] = ysize;
        (*dimsizes_lon)[1] = xsize;

        grdsetup(polex,poley,dist,deg, ore + 90.0 );
        for (y = 0; y < ysize; y++) {
                for(x = 0; x < xsize; x++) {
                        grdloc(x,y,&((*lon)[y * ysize + x]),&((*lat)[y * ysize + x]));
                }
        }

}

void GetGrid_203
#if NhlNeedProto
(GribParamList* thevarrec, float** lat, int* n_dims_lat, int** dimsizes_lat, float** lon, int* n_dims_lon, int** dimsizes_lon)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon)
GribParamList* thevarrec;
float** lat;
int* n_dims_lat;
int** dimsizes_lat;
float** lon;
int* n_dims_lon;
int** dimsizes_lon;
#endif
{
/*
* Very tricky need to figure out how to exchange dimensions correctly
*/
        int xsize = 45;
        int ysize = 39;
        float polex = 27;
        float poley = 37;
        float dist = 190.5;
        float deg = 60.0;
        float ore = -150.0;
        int x,y;


        *lat = (float*)NclMalloc(sizeof(float) * xsize * ysize);
        *lon= (float*)NclMalloc(sizeof(float) * xsize * ysize);
        *dimsizes_lat = (int*)NclMalloc(sizeof(int) * 2);
        *dimsizes_lon = (int*)NclMalloc(sizeof(int) * 2);
        *n_dims_lat = 2;
        *n_dims_lon = 2;
        (*dimsizes_lat)[0] = ysize;
        (*dimsizes_lat)[1] = xsize;
        (*dimsizes_lon)[0] = ysize;
        (*dimsizes_lon)[1] = xsize;

        grdsetup(polex,poley,dist,deg, ore + 90.0 );
        for (y = 0; y < ysize; y++) {
                for(x = 0; x < xsize; x++) {
                        grdloc(x,y,&((*lon)[y * ysize + x]),&((*lat)[y * ysize + x]));
                }
        }

}

void GetGrid_202
#if NhlNeedProto
(GribParamList* thevarrec, float** lat, int* n_dims_lat, int** dimsizes_lat, float** lon, int* n_dims_lon, int** dimsizes_lon)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon)
GribParamList* thevarrec;
float** lat;
int* n_dims_lat;
int** dimsizes_lat;
float** lon;
int* n_dims_lon;
int** dimsizes_lon;
#endif
{
/*
* Very tricky need to figure out how to exchange dimensions correctly
*/
        int xsize = 65;
        int ysize = 43;
        float polex = 33;
        float poley = 45;
        float dist = 190.5;
        float deg = 60.0;
        float ore = -105.0;
        int x,y;


        *lat = (float*)NclMalloc(sizeof(float) * xsize * ysize);
        *lon= (float*)NclMalloc(sizeof(float) * xsize * ysize);
        *dimsizes_lat = (int*)NclMalloc(sizeof(int) * 2);
        *dimsizes_lon = (int*)NclMalloc(sizeof(int) * 2);
        *n_dims_lat = 2;
        *n_dims_lon = 2;
        (*dimsizes_lat)[0] = ysize;
        (*dimsizes_lat)[1] = xsize;
        (*dimsizes_lon)[0] = ysize;
        (*dimsizes_lon)[1] = xsize;

        grdsetup(polex,poley,dist,deg, ore + 90.0 );
        for (y = 0; y < ysize; y++) {
                for(x = 0; x < xsize; x++) {
                        grdloc(x,y,&((*lon)[y * ysize + x]),&((*lat)[y * ysize + x]));
                }
        }

}

void GetGrid_201
#if NhlNeedProto
(GribParamList* thevarrec, float** lat, int* n_dims_lat, int** dimsizes_lat, float** lon, int* n_dims_lon, int** dimsizes_lon)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon)
GribParamList* thevarrec;
float** lat;
int* n_dims_lat;
int** dimsizes_lat;
float** lon;
int* n_dims_lon;
int** dimsizes_lon;
#endif
{
/*
* Very tricky need to figure out how to exchange dimensions correctly
*/
        int xsize = 65;
        int ysize = 65;
        float polex = 33;
        float poley = 33;
        float dist =381.0;
        float deg = 60.0;
        float ore = -105.0;
        int x,y;


        *lat = (float*)NclMalloc(sizeof(float) * xsize * ysize);
        *lon= (float*)NclMalloc(sizeof(float) * xsize * ysize);
        *dimsizes_lat = (int*)NclMalloc(sizeof(int) * 2);
        *dimsizes_lon = (int*)NclMalloc(sizeof(int) * 2);
        *n_dims_lat = 2;
        *n_dims_lon = 2;
        (*dimsizes_lat)[0] = ysize;
        (*dimsizes_lat)[1] = xsize;
        (*dimsizes_lon)[0] = ysize;
        (*dimsizes_lon)[1] = xsize;

        grdsetup(polex,poley,dist,deg, ore + 90.0 );
        for (y = 0; y < ysize; y++) {
                for(x = 0; x < xsize; x++) {
                        grdloc(x,y,&((*lon)[y * ysize + x]),&((*lat)[y * ysize + x]));
                }
        }

}

void GetGrid_107
#if NhlNeedProto
(GribParamList* thevarrec, float** lat, int* n_dims_lat, int** dimsizes_lat, float** lon, int* n_dims_lon, int** dimsizes_lon)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon)
GribParamList* thevarrec;
float** lat;
int* n_dims_lat;
int** dimsizes_lat;
float** lon;
int* n_dims_lon;
int** dimsizes_lon;
#endif
{
/*
* Very tricky need to figure out how to exchange dimensions correctly
*/
        int xsize = 120;
        int ysize = 92;
        float polex = 46;
        float poley = 167;
        float dist = 91.452;
        float deg = 60.0;
        float ore = -105.0;
        int x,y;


        *lat = (float*)NclMalloc(sizeof(float) * xsize * ysize);
        *lon= (float*)NclMalloc(sizeof(float) * xsize * ysize);
        *dimsizes_lat = (int*)NclMalloc(sizeof(int) * 2);
        *dimsizes_lon = (int*)NclMalloc(sizeof(int) * 2);
        *n_dims_lat = 2;
        *n_dims_lon = 2;
        (*dimsizes_lat)[0] = ysize;
        (*dimsizes_lat)[1] = xsize;
        (*dimsizes_lon)[0] = ysize;
        (*dimsizes_lon)[1] = xsize;

        grdsetup(polex,poley,dist,deg, ore + 90.0 );
        for (y = 0; y < ysize; y++) {
                for(x = 0; x < xsize; x++) {
                        grdloc(x,y,&((*lon)[y * ysize + x]),&((*lat)[y * ysize + x]));
                }
        }

}

void GetGrid_106
#if NhlNeedProto
(GribParamList* thevarrec, float** lat, int* n_dims_lat, int** dimsizes_lat, float** lon, int* n_dims_lon, int** dimsizes_lon)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon)
GribParamList* thevarrec;
float** lat;
int* n_dims_lat;
int** dimsizes_lat;
float** lon;
int* n_dims_lon;
int** dimsizes_lon;
#endif
{
/*
* Very tricky need to figure out how to exchange dimensions correctly
*/
        int xsize = 165;
        int ysize = 117;
        float polex = 80;
        float poley = 176;
        float dist = 91.452;
        float deg = 60.0;
        float ore = -105.0;
        int x,y;


        *lat = (float*)NclMalloc(sizeof(float) * xsize * ysize);
        *lon= (float*)NclMalloc(sizeof(float) * xsize * ysize);
        *dimsizes_lat = (int*)NclMalloc(sizeof(int) * 2);
        *dimsizes_lon = (int*)NclMalloc(sizeof(int) * 2);
        *n_dims_lat = 2;
        *n_dims_lon = 2;
        (*dimsizes_lat)[0] = ysize;
        (*dimsizes_lat)[1] = xsize;
        (*dimsizes_lon)[0] = ysize;
        (*dimsizes_lon)[1] = xsize;

        grdsetup(polex,poley,dist,deg, ore + 90.0 );
        for (y = 0; y < ysize; y++) {
                for(x = 0; x < xsize; x++) {
                        grdloc(x,y,&((*lon)[y * ysize + x]),&((*lat)[y * ysize + x]));
                }
        }

}

void GetGrid_104
#if NhlNeedProto
(GribParamList* thevarrec, float** lat, int* n_dims_lat, int** dimsizes_lat, float** lon, int* n_dims_lon, int** dimsizes_lon)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon)
GribParamList* thevarrec;
float** lat;
int* n_dims_lat;
int** dimsizes_lat;
float** lon;
int* n_dims_lon;
int** dimsizes_lon;
#endif
{
/*
* Very tricky need to figure out how to exchange dimensions correctly
*/
        int xsize = 147;
        int ysize = 110;
        float polex = 75.5;
        float poley = 109.5;
        float dist = 91.452;
        float deg = 60.0;
        float ore = -105.0;
        int x,y;


        *lat = (float*)NclMalloc(sizeof(float) * xsize * ysize);
        *lon= (float*)NclMalloc(sizeof(float) * xsize * ysize);
        *dimsizes_lat = (int*)NclMalloc(sizeof(int) * 2);
        *dimsizes_lon = (int*)NclMalloc(sizeof(int) * 2);
        *n_dims_lat = 2;
        *n_dims_lon = 2;
        (*dimsizes_lat)[0] = ysize;
        (*dimsizes_lat)[1] = xsize;
        (*dimsizes_lon)[0] = ysize;
        (*dimsizes_lon)[1] = xsize;

        grdsetup(polex,poley,dist,deg, ore + 90.0 );
        for (y = 0; y < ysize; y++) {
                for(x = 0; x < xsize; x++) {
                        grdloc(x,y,&((*lon)[y * ysize + x]),&((*lat)[y * ysize + x]));
                }
        }

}

void GetGrid_103
#if NhlNeedProto
(GribParamList* thevarrec, float** lat, int* n_dims_lat, int** dimsizes_lat, float** lon, int* n_dims_lon, int** dimsizes_lon)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon)
GribParamList* thevarrec;
float** lat;
int* n_dims_lat;
int** dimsizes_lat;
float** lon;
int* n_dims_lon;
int** dimsizes_lon;
#endif
{
/*
* Very tricky need to figure out how to exchange dimensions correctly
*/
        int xsize = 65;
        int ysize = 56;
        float polex = 25.5;
        float poley = 84.5;
        float dist = 91.452;
        float deg = 60.0;
        float ore = -105.0;
        int x,y;


        *lat = (float*)NclMalloc(sizeof(float) * xsize * ysize);
        *lon= (float*)NclMalloc(sizeof(float) * xsize * ysize);
        *dimsizes_lat = (int*)NclMalloc(sizeof(int) * 2);
        *dimsizes_lon = (int*)NclMalloc(sizeof(int) * 2);
        *n_dims_lat = 2;
        *n_dims_lon = 2;
        (*dimsizes_lat)[0] = ysize;
        (*dimsizes_lat)[1] = xsize;
        (*dimsizes_lon)[0] = ysize;
        (*dimsizes_lon)[1] = xsize;

        grdsetup(polex,poley,dist,deg, ore + 90.0 );
        for (y = 0; y < ysize; y++) {
                for(x = 0; x < xsize; x++) {
                        grdloc(x,y,&((*lon)[y * ysize + x]),&((*lat)[y * ysize + x]));
                }
        }

}

void GetGrid_101
#if NhlNeedProto
(GribParamList* thevarrec, float** lat, int* n_dims_lat, int** dimsizes_lat, float** lon, int* n_dims_lon, int** dimsizes_lon)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon)
GribParamList* thevarrec;
float** lat;
int* n_dims_lat;
int** dimsizes_lat;
float** lon;
int* n_dims_lon;
int** dimsizes_lon;
#endif
{
/*
* Very tricky need to figure out how to exchange dimensions correctly
*/
        int xsize = 113;
        int ysize = 91;
        float polex = 58.5;
        float poley = 92.5;
        float dist = 91.452;
        float deg = 60.0;
        float ore = -105.0;
        int x,y;


        *lat = (float*)NclMalloc(sizeof(float) * xsize * ysize);
        *lon= (float*)NclMalloc(sizeof(float) * xsize * ysize);
        *dimsizes_lat = (int*)NclMalloc(sizeof(int) * 2);
        *dimsizes_lon = (int*)NclMalloc(sizeof(int) * 2);
        *n_dims_lat = 2;
        *n_dims_lon = 2;
        (*dimsizes_lat)[0] = ysize;
        (*dimsizes_lat)[1] = xsize;
        (*dimsizes_lon)[0] = ysize;
        (*dimsizes_lon)[1] = xsize;

        grdsetup(polex,poley,dist,deg, ore + 90.0 );
        for (y = 0; y < ysize; y++) {
                for(x = 0; x < xsize; x++) {
                        grdloc(x,y,&((*lon)[y * ysize + x]),&((*lat)[y * ysize + x]));
                }
        }

}

void GetGrid_100
#if NhlNeedProto
(GribParamList* thevarrec, float** lat, int* n_dims_lat, int** dimsizes_lat, float** lon, int* n_dims_lon, int** dimsizes_lon)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon)
GribParamList* thevarrec;
float** lat;
int* n_dims_lat;
int** dimsizes_lat;
float** lon;
int* n_dims_lon;
int** dimsizes_lon;
#endif
{
/*
* Very tricky need to figure out how to exchange dimensions correctly
*/
        int xsize = 83;
        int ysize = 83;
        float polex = 40.5;
        float poley = 88.5;
        float dist = 91.452;
        float deg = 60.0;
        float ore = -105.0;
        int x,y;


        *lat = (float*)NclMalloc(sizeof(float) * xsize * ysize);
        *lon= (float*)NclMalloc(sizeof(float) * xsize * ysize);
        *dimsizes_lat = (int*)NclMalloc(sizeof(int) * 2);
        *dimsizes_lon = (int*)NclMalloc(sizeof(int) * 2);
        *n_dims_lat = 2;
        *n_dims_lon = 2;
        (*dimsizes_lat)[0] = ysize;
        (*dimsizes_lat)[1] = xsize;
        (*dimsizes_lon)[0] = ysize;
        (*dimsizes_lon)[1] = xsize;

        grdsetup(polex,poley,dist,deg, ore + 90.0 );
        for (y = 0; y < ysize; y++) {
                for(x = 0; x < xsize; x++) {
                        grdloc(x,y,&((*lon)[y * ysize + x]),&((*lat)[y * ysize + x]));
                }
        }

}

void GetGrid_87
#if NhlNeedProto
(GribParamList* thevarrec, float** lat, int* n_dims_lat, int** dimsizes_lat, float** lon, int* n_dims_lon, int** dimsizes_lon)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon)
GribParamList* thevarrec;
float** lat;
int* n_dims_lat;
int** dimsizes_lat;
float** lon;
int* n_dims_lon;
int** dimsizes_lon;
#endif
{
/*
* Very tricky need to figure out how to exchange dimensions correctly
*/
        int xsize = 81;
        int ysize = 62;
        float polex = 31.91;
        float poley = 112.53;
        float dist =  68.153;
        float deg = 60.0;
        float ore = -105.0;
        int x,y;


        *lat = (float*)NclMalloc(sizeof(float) * xsize * ysize);
        *lon= (float*)NclMalloc(sizeof(float) * xsize * ysize);
        *dimsizes_lat = (int*)NclMalloc(sizeof(int) * 2);
        *dimsizes_lon = (int*)NclMalloc(sizeof(int) * 2);
        *n_dims_lat = 2;
        *n_dims_lon = 2;
        (*dimsizes_lat)[0] = ysize;
        (*dimsizes_lat)[1] = xsize;
        (*dimsizes_lon)[0] = ysize;
        (*dimsizes_lon)[1] = xsize;

        grdsetup(polex,poley,dist,deg, ore + 90.0 );
        for (y = 0; y < ysize; y++) {
                for(x = 0; x < xsize; x++) {
                        grdloc(x,y,&((*lon)[y * ysize + x]),&((*lat)[y * ysize + x]));
                }
        }

}

void GetGrid_56
#if NhlNeedProto
(GribParamList* thevarrec, float** lat, int* n_dims_lat, int** dimsizes_lat, float** lon, int* n_dims_lon, int** dimsizes_lon)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon)
GribParamList* thevarrec;
float** lat;
int* n_dims_lat;
int** dimsizes_lat;
float** lon;
int* n_dims_lon;
int** dimsizes_lon;
#endif
{
/*
* Very tricky need to figure out how to exchange dimensions correctly
*/
        int xsize = 87;
        int ysize = 71;
        float polex = 40;
        float poley = 73;
        float dist =  127;
        float deg = 60.0;
        float ore = -105.0;
        int x,y;


        *lat = (float*)NclMalloc(sizeof(float) * xsize * ysize);
        *lon= (float*)NclMalloc(sizeof(float) * xsize * ysize);
        *dimsizes_lat = (int*)NclMalloc(sizeof(int) * 2);
        *dimsizes_lon = (int*)NclMalloc(sizeof(int) * 2);
        *n_dims_lat = 2;
        *n_dims_lon = 2;
        (*dimsizes_lat)[0] = ysize;
        (*dimsizes_lat)[1] = xsize;
        (*dimsizes_lon)[0] = ysize;
        (*dimsizes_lon)[1] = xsize;

        grdsetup(polex,poley,dist,deg, ore + 90.0 );
        for (y = 0; y < ysize; y++) {
                for(x = 0; x < xsize; x++) {
                        grdloc(x,y,&((*lon)[y * ysize + x]),&((*lat)[y * ysize + x]));
                }
        }

}
void GetGrid_55
#if NhlNeedProto
(GribParamList* thevarrec, float** lat, int* n_dims_lat, int** dimsizes_lat, float** lon, int* n_dims_lon, int** dimsizes_lon)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon)
GribParamList* thevarrec;
float** lat;
int* n_dims_lat;
int** dimsizes_lat;
float** lon;
int* n_dims_lon;
int** dimsizes_lon;
#endif
{
/*
* Very tricky need to figure out how to exchange dimensions correctly
*/
        int xsize = 87;
        int ysize = 71;
        float polex = 44;
        float poley = 38;
        float dist = 254;
        float deg = 60.0;
        float ore = -105.0;
        int x,y;


        *lat = (float*)NclMalloc(sizeof(float) * xsize * ysize);
        *lon= (float*)NclMalloc(sizeof(float) * xsize * ysize);
        *dimsizes_lat = (int*)NclMalloc(sizeof(int) * 2);
        *dimsizes_lon = (int*)NclMalloc(sizeof(int) * 2);
        *n_dims_lat = 2;
        *n_dims_lon = 2;
        (*dimsizes_lat)[0] = ysize;
        (*dimsizes_lat)[1] = xsize;
        (*dimsizes_lon)[0] = ysize;
        (*dimsizes_lon)[1] = xsize;

        grdsetup(polex,poley,dist,deg, ore + 90.0 );
        for (y = 0; y < ysize; y++) {
                for(x = 0; x < xsize; x++) {
                        grdloc(x,y,&((*lon)[y * ysize + x]),&((*lat)[y * ysize + x]));
                }
        }

}

void GetGrid_28
#if NhlNeedProto
(GribParamList* thevarrec, float** lat, int* n_dims_lat, int** dimsizes_lat, float** lon, int* n_dims_lon, int** dimsizes_lon)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon)
GribParamList* thevarrec;
float** lat;
int* n_dims_lat;
int** dimsizes_lat;
float** lon;
int* n_dims_lon;
int** dimsizes_lon;
#endif
{
/*
* Very tricky need to figure out how to exchange dimensions correctly
*/
        int xsize = 65;
        int ysize = 65;
        float polex = 33;
        float poley = 33;
        float dist = 381; 
        float deg = -60.0;
        float ore = 100.0;
        int x,y;


        *lat = (float*)NclMalloc(sizeof(float) * xsize * ysize);
        *lon= (float*)NclMalloc(sizeof(float) * xsize * ysize);
        *dimsizes_lat = (int*)NclMalloc(sizeof(int) * 2);
        *dimsizes_lon = (int*)NclMalloc(sizeof(int) * 2);
        *n_dims_lat = 2;
        *n_dims_lon = 2;
        (*dimsizes_lat)[0] = ysize;
        (*dimsizes_lat)[1] = xsize;
        (*dimsizes_lon)[0] = ysize;
        (*dimsizes_lon)[1] = xsize;

        grdsetup(polex,poley,dist,deg, ore + 90.0 );
        for (y = 0; y < ysize; y++) {
                for(x = 0; x < xsize; x++) {
                        grdloc(x,y,&((*lon)[y * ysize + x]),&((*lat)[y * ysize + x]));
                }
        }

}

void GetGrid_27
#if NhlNeedProto
(GribParamList* thevarrec, float** lat, int* n_dims_lat, int** dimsizes_lat, float** lon, int* n_dims_lon, int** dimsizes_lon)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon)
GribParamList* thevarrec;
float** lat;
int* n_dims_lat;
int** dimsizes_lat;
float** lon;
int* n_dims_lon;
int** dimsizes_lon;
#endif
{
/*
* Very tricky need to figure out how to exchange dimensions correctly
*/
        int xsize = 65;
        int ysize = 65;
        float polex = 33;
        float poley = 33;
        float dist = 381; 
        float deg = 60.0;
        float ore = 80.0;
        int x,y;


        *lat = (float*)NclMalloc(sizeof(float) * xsize * ysize);
        *lon= (float*)NclMalloc(sizeof(float) * xsize * ysize);
        *dimsizes_lat = (int*)NclMalloc(sizeof(int) * 2);
        *dimsizes_lon = (int*)NclMalloc(sizeof(int) * 2);
        *n_dims_lat = 2;
        *n_dims_lon = 2;
        (*dimsizes_lat)[0] = ysize;
        (*dimsizes_lat)[1] = xsize;
        (*dimsizes_lon)[0] = ysize;
        (*dimsizes_lon)[1] = xsize;

        grdsetup(polex,poley,dist,deg, ore + 90.0 );
        for (y = 0; y < ysize; y++) {
                for(x = 0; x < xsize; x++) {
                        grdloc(x,y,&((*lon)[y * ysize + x]),&((*lat)[y * ysize + x]));
                }
        }

}

void GetGrid_6
#if NhlNeedProto
(GribParamList* thevarrec, float** lat, int* n_dims_lat, int** dimsizes_lat, float** lon, int* n_dims_lon, int** dimsizes_lon)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon)
GribParamList* thevarrec;
float** lat;
int* n_dims_lat;
int** dimsizes_lat;
float** lon;
int* n_dims_lon;
int** dimsizes_lon;
#endif
{
/*
* Very tricky need to figure out how to exchange dimensions correctly
*/
        int xsize = 53;
        int ysize = 45;
        float polex = 27;
        float poley = 49;
        float dist = 190.5 ;
        float deg = 60.0;
        float ore = -105.0;
        int x,y;


        *lat = (float*)NclMalloc(sizeof(float) * xsize * ysize);
        *lon= (float*)NclMalloc(sizeof(float) * xsize * ysize);
        *dimsizes_lat = (int*)NclMalloc(sizeof(int) * 2);
        *dimsizes_lon = (int*)NclMalloc(sizeof(int) * 2);
        *n_dims_lat = 2;
        *n_dims_lon = 2;
        (*dimsizes_lat)[0] = ysize;
        (*dimsizes_lat)[1] = xsize;
        (*dimsizes_lon)[0] = ysize;
        (*dimsizes_lon)[1] = xsize;

        grdsetup(polex,poley,dist,deg, ore + 90.0 );
        for (y = 0; y < ysize; y++) {
                for(x = 0; x < xsize; x++) {
                        grdloc(x,y,&((*lon)[y * ysize + x]),&((*lat)[y * ysize + x]));
                }
        }

}

void GetGrid_5
#if NhlNeedProto
(GribParamList* thevarrec, float** lat, int* n_dims_lat, int** dimsizes_lat, float** lon, int* n_dims_lon, int** dimsizes_lon)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon)
GribParamList* thevarrec;
float** lat;
int* n_dims_lat;
int** dimsizes_lat;
float** lon;
int* n_dims_lon;
int** dimsizes_lon;
#endif
{
/*
* Very tricky need to figure out how to exchange dimensions correctly
*/
	int xsize = 53;
	int ysize = 57;
	float polex = 27;
	float poley = 49;
	float dist = 190.5 ;
	float deg = 60.0;
	float ore = -105.0;
	int x,y;
	
	
	*lat = (float*)NclMalloc(sizeof(float) * xsize * ysize);
	*lon= (float*)NclMalloc(sizeof(float) * xsize * ysize);
	*dimsizes_lat = (int*)NclMalloc(sizeof(int) * 2);
	*dimsizes_lon = (int*)NclMalloc(sizeof(int) * 2);
	*n_dims_lat = 2;
	*n_dims_lon = 2;
        (*dimsizes_lat)[0] = ysize;
        (*dimsizes_lat)[1] = xsize;
        (*dimsizes_lon)[0] = ysize;
        (*dimsizes_lon)[1] = xsize;

	grdsetup(polex,poley,dist,deg, ore + 90.0 );
	for (y = 0; y < ysize; y++) {
		for(x = 0; x < xsize; x++) {
			grdloc(x,y,&((*lon)[y * ysize + x]),&((*lat)[y * ysize + x]));
		}
	}
	
}

void GetGrid_105
#if NhlNeedProto
(GribParamList* thevarrec, float** lat, int* n_dims_lat, int** dimsizes_lat, float** lon, int* n_dims_lon, int** dimsizes_lon)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon)
GribParamList* thevarrec;
float** lat;
int* n_dims_lat;
int** dimsizes_lat;
float** lon;
int* n_dims_lon;
int** dimsizes_lon;
#endif
{
/*
* Very tricky need to figure out how to exchange dimensions correctly
*/
	int xsize = 83;
	int ysize = 83;
	float polex = 40.5;
	float poley = 88.5;
	float dist = 90.75464;
	float deg = 60.0;
	float ore = -105.0;
	int x,y;
	
	
	*lat = (float*)NclMalloc(sizeof(float) * xsize * ysize);
	*lon= (float*)NclMalloc(sizeof(float) * xsize * ysize);
	*dimsizes_lat = (int*)NclMalloc(sizeof(int) * 2);
	*dimsizes_lon = (int*)NclMalloc(sizeof(int) * 2);
	*n_dims_lat = 2;
	*n_dims_lon = 2;
        (*dimsizes_lat)[0] = ysize;
        (*dimsizes_lat)[1] = xsize;
        (*dimsizes_lon)[0] = ysize;
        (*dimsizes_lon)[1] = xsize;

	grdsetup(polex,poley,dist,deg, ore + 90.0 );
	for (y = 0; y < ysize; y++) {
		for(x = 0; x < xsize; x++) {
			grdloc(x,y,&((*lon)[y * ysize + x]),&((*lat)[y * ysize + x]));
		}
	}
	
}
/*
* END Polar Stereographic GRIDS
*/
/*
* Grid dimensions must be set in var_info field of 
*/
static int GenericUnPack
#if NhlNeedProto
(int fd, void** outdat, void** missing_value, GribRecordInqRec *therec, GribParamList* thevarrec)
#else
(fd, outdat, missing_value, therec, thevarrec)
int fd;
void** outdat;
void** missing_value;
GribRecordInqRec *therec;
GribParamList* thevarrec;
#endif
{
	int index =0,i=0;
	int integer = 0;
	int spherical_harm = 0;
	int second_order = 0;
	int additional_flags = 0;
	int sign;
	char tmp[4];
	int number_of_bits;
	int binary_scale_factor;
	int decimal_scale_factor;
	int unused_bits;
	float reference_value;
	float tmpb,tmpa;
	char *bds;
	int total = 0;
	void *data = NULL;
	int isize = sizeof(int)*8;
	unsigned int X;
	int tbits;
	int bboff;
	


	bds = (char*)NclMalloc((unsigned)therec->bds_size);
	lseek(fd,therec->start + therec->bds_off,SEEK_SET);
	read(fd,(void*)bds,therec->bds_size);

	spherical_harm = (int)(bds[3] & (char)0200) ? 1 : 0;
	second_order = (int)(bds[3] & (char)0100) ? 1 : 0;
	integer = (int)(bds[3] & (char)0040) ? 1 : 0;
	additional_flags = (bds[3] & (char)0020) ? 1 : 0;


	tmp[0] = (therec->pds[26] & (char)0177);
	tmp[1] = therec->pds[27];
	decimal_scale_factor  = CnvtToDecimal(2,tmp);
	if(therec->pds[26] & (char)0200) 
		decimal_scale_factor = -decimal_scale_factor;

	number_of_bits = (int)bds[10];
	tmp[0] = (char)(bds[3] & (char)0017);
	unused_bits = CnvtToDecimal(1,tmp);
	tmp[0] = (char)(bds[4] & (char)0177);
	tmp[1] = bds[5];
	binary_scale_factor = CnvtToDecimal(2,tmp);
	if(bds[4] & (char)0200) {
		binary_scale_factor = -binary_scale_factor;
	}
	sign  = (bds[6] & (char) 0200)? 1 : 0;
	tmpa = (float)(bds[6] & (char)0177);
	tmpb = (float)CnvtToDecimal(3,&(bds[7]));

	reference_value = tmpb;
	reference_value *= (float)pow(2.0,-24.0);
	reference_value *= (float)pow(16.0,(double)(tmpa - 64));
	if(sign) {
		reference_value = -reference_value;
	}

	if((!spherical_harm)&&(!second_order)&&(!additional_flags)) {
		if(number_of_bits != 0) {
			i = 11;
			bboff = 0;
			index = 0;
			tbits = 0;
			total = (int)(((therec->bds_size - 11) * 8 - unused_bits)/ number_of_bits);
			if(integer) {
				data = (void*)NclMalloc((unsigned)sizeof(int)*total);
			} else {
				data = (void*)NclMalloc((unsigned)sizeof(float)*total);
			}
			while(index < total) {
				X = UnsignedCnvtToDecimal(4,&(bds[i]));
/*
				fprintf(stdout,"o:");
				printbinary(X);
*/
				X = X << bboff;
/*
				fprintf(stdout,"l:");
				printbinary(X);
*/
				X = X >> (isize - number_of_bits);
/*
				fprintf(stdout,"r:");
				printbinary(X);
				fprintf(stdout,"(%d,%d,%d):\t%d\n",tbits,i,bboff,X);
*/

				if(integer) {
					((int*)data)[index] = (int)(reference_value + (X * pow(2.0,(double)binary_scale_factor)))/pow(10.0,(double)(decimal_scale_factor));

				} else {
					((float*)data)[index] = (float)(reference_value + (X * pow(2.0,(double)binary_scale_factor)))/pow(10.0,(double)(decimal_scale_factor));
				}
/*
				if((index > 1)&&(((float*)data)[index] > ((float*)data)[index-1] + 500)) {
					fprintf(stdout,"index:%d\n",index);
				} else if((index > 1)&&(((float*)data)[index] + 500 < ((float*)data)[index-1] )) {
					fprintf(stdout,"index:%d\n",index);
				}
*/

				tbits += number_of_bits;
				i = (int)(tbits/8.0) + 11;
				bboff = tbits % 8;
				index++;
			}
			*outdat = data;
		} else {
			NhlPError(NhlWARNING,NhlEUNKNOWN,"GribUnPack : constant field\n");
		}
	} else {
		if(spherical_harm)
			NhlPError(NhlWARNING,NhlEUNKNOWN,"GribUnPack : Spherical Harmonics Detected can't un pack\n");
		if(second_order)
			NhlPError(NhlWARNING,NhlEUNKNOWN,"GribUnPack : Second Order Detected can't un pack\n");
		*outdat = NULL;
		*missing_value = NULL;
	}
	return(integer);
}

static int IFOS50UnPack
#if NhlNeedProto
(int fd, void** outdat, void** missing_value, GribRecordInqRec *therec, GribParamList* thevarrec)
#else
(fd, outdat, missing_value, therec, thevarrec)
int fd;
void** outdat;
void** missing_value;
GribRecordInqRec *therec;
GribParamList* thevarrec;
#endif
{
	int count =0,i=0,j = 0;
	int rindex =0;
	int integer = 0;
	int lat= 0;
	int lon= 0;
	int spherical_harm = 0;
	int second_order = 0;
	int additional_flags = 0;
	int sign;
	char tmp[4];
	int number_of_bits;
	int binary_scale_factor;
	int decimal_scale_factor;
	int unused_bits;
	float reference_value;
	float tmpb,tmpa;
	char *bds;
	int total = 0;
	void *data = NULL;
	int isize = sizeof(int)*8;
	unsigned int X;
	int tbits;
	int bboff;
	int fill = 0;
	


	bds = (char*)NclMalloc((unsigned)therec->bds_size);
	lseek(fd,therec->start + therec->bds_off,SEEK_SET);
	read(fd,(void*)bds,therec->bds_size);

	spherical_harm = (int)(bds[3] & (char)0200) ? 1 : 0;
	second_order = (int)(bds[3] & (char)0100) ? 1 : 0;
	integer = (int)(bds[3] & (char)0040) ? 1 : 0;
	additional_flags = (bds[3] & (char)0020) ? 1 : 0;


	tmp[0] = (therec->pds[26] & (char)0177);
	tmp[1] = therec->pds[27];
	decimal_scale_factor  = CnvtToDecimal(2,tmp);
	if(therec->pds[26] & (char)0200) 
		decimal_scale_factor = -decimal_scale_factor;

	number_of_bits = (int)bds[10];
	tmp[0] = (char)(bds[3] & (char)0017);
	unused_bits = CnvtToDecimal(1,tmp);
	tmp[0] = (char)(bds[4] & (char)0177);
	tmp[1] = bds[5];
	binary_scale_factor = CnvtToDecimal(2,tmp);
	if(bds[4] & (char)0200) {
		binary_scale_factor = -binary_scale_factor;
	}
	sign  = (bds[6] & (char) 0200)? 1 : 0;
	tmpa = (float)(bds[6] & (char)0177);
	tmpb = (float)CnvtToDecimal(3,&(bds[7]));

	reference_value = tmpb;
	reference_value *= (float)pow(2.0,-24.0);
	reference_value *= (float)pow(16.0,(double)(tmpa - 64));
	if(sign) {
		reference_value = -reference_value;
	}

	if((!spherical_harm)&&(!second_order)&&(!additional_flags)) {
		if(number_of_bits != 0) {
			if(integer) {
				*missing_value= (void*)NclMalloc((unsigned)sizeof(int));
				*(int*)(*missing_value) = -999;
			} else {
				*missing_value= (void*)NclMalloc((unsigned)sizeof(float));
				*(float*)(*missing_value) = -9999.0;
			}
			i = 11;
			bboff = 0;
			count = 0;
			rindex = 0;
			tbits = 0;
			total = (int)(((therec->bds_size - 11) * 8 - unused_bits)/ number_of_bits);
			if(integer) {
				data = (void*)NclMalloc((unsigned)sizeof(int)*36*33);
			} else {
				data = (void*)NclMalloc((unsigned)sizeof(float)*36*33);
			}
			for(lat = 0; lat < 33; lat++) {
				switch(lat) {
				case 0:
				case 1:
				case 2:
				case 3:
					fill = 7;
					break;
				case 4:
				case 5:
				case 6:
				case 7:
					fill = 6;
					break;
				case 8:
				case 9:
				case 10:
				case 11:
					fill = 5;
					break;
				case 12:
				case 13:
				case 14:
				case 15:
					fill = 4;
					break;
				case 16:
				case 17:
				case 18:
				case 19:
					fill = 3;
					break;
				case 20:
				case 21:
				case 22:
				case 23:
					fill = 2;
					break;
				case 24:
				case 25:	
				case 26:
				case 27:
					fill = 1;
					break;
				default:
					fill = 0;
					break;
				}
				for( j = 0; j< fill; j++) {
					if(integer) {
                                        	((int*)data)[rindex] = -999;

	                                } else {
                                        	((float*)data)[rindex] = -9999.0;
					}
					rindex++;
				}
				for(j = 0; j < 36 - fill * 2;	j++) {
					X = UnsignedCnvtToDecimal(4,&(bds[i]));
					X = X << bboff;
					X = X >> (isize - number_of_bits);
					if(integer) {
						((int*)data)[rindex] = (int)(reference_value + (X * pow(2.0,(double)binary_scale_factor)))/pow(10.0,(double)(decimal_scale_factor));
	
					} else {
						((float*)data)[rindex] = (float)(reference_value + (X * pow(2.0,(double)binary_scale_factor)))/pow(10.0,(double)(decimal_scale_factor));
					}
					tbits += number_of_bits;
					i = (int)(tbits/8.0) + 11;
					bboff = tbits % 8;
					count++;
					rindex++;
				}
				for( j = 0; j< fill; j++) {
					if(integer) {
                                        	((int*)data)[rindex] = -999;

	                                } else {
                                        	((float*)data)[rindex] = -9999.0;
					}
					rindex++;
				}
			}
			*outdat = data;
		} else {
			NhlPError(NhlWARNING,NhlEUNKNOWN,"GribUnPack : constant field\n");
		}
	} else {
		if(spherical_harm)
			NhlPError(NhlWARNING,NhlEUNKNOWN,"GribUnPack : Spherical Harmonics Detected can't un pack\n");
		if(second_order)
			NhlPError(NhlWARNING,NhlEUNKNOWN,"GribUnPack : Second Order Detected can't un pack\n");
		*outdat = NULL;
		*missing_value = NULL;
	}
	return(integer);
}

static int IFOSUnPack
#if NhlNeedProto
(int fd, void** outdat, void** missing_value, GribRecordInqRec *therec, GribParamList* thevarrec)
#else
(fd, outdat, missing_value, therec, thevarrec)
int fd;
void** outdat;
void** missing_value;
GribRecordInqRec *therec;
GribParamList* thevarrec;
#endif
{
	int index =0,i=0;
	int integer = 0;
	int spherical_harm = 0;
	int second_order = 0;
	int additional_flags = 0;
	int sign;
	char tmp[4];
	int number_of_bits;
	int binary_scale_factor;
	int decimal_scale_factor;
	int unused_bits;
	float reference_value;
	float tmpb,tmpa;
	char *bds;
	int total = 0;
	void *data = NULL;
	int isize = sizeof(int)*8;
	unsigned int X;
	int tbits;
	int bboff;
	int npole =0;
	int polefirst = 0;
	


	bds = (char*)NclMalloc((unsigned)therec->bds_size);
	lseek(fd,therec->start + therec->bds_off,SEEK_SET);
	read(fd,(void*)bds,therec->bds_size);

	spherical_harm = (int)(bds[3] & (char)0200) ? 1 : 0;
	second_order = (int)(bds[3] & (char)0100) ? 1 : 0;
	integer = (int)(bds[3] & (char)0040) ? 1 : 0;
	additional_flags = (bds[3] & (char)0020) ? 1 : 0;


	tmp[0] = (therec->pds[26] & (char)0177);
	tmp[1] = therec->pds[27];
	decimal_scale_factor  = CnvtToDecimal(2,tmp);
	if(therec->pds[26] & (char)0200) 
		decimal_scale_factor = -decimal_scale_factor;

	number_of_bits = (int)bds[10];
	tmp[0] = (char)(bds[3] & (char)0017);
	unused_bits = CnvtToDecimal(1,tmp);
	tmp[0] = (char)(bds[4] & (char)0177);
	tmp[1] = bds[5];
	binary_scale_factor = CnvtToDecimal(2,tmp);
	if(bds[4] & (char)0200) {
		binary_scale_factor = -binary_scale_factor;
	}
	sign  = (bds[6] & (char) 0200)? 1 : 0;
	tmpa = (float)(bds[6] & (char)0177);
	tmpb = (float)CnvtToDecimal(3,&(bds[7]));

	reference_value = tmpb;
	reference_value *= (float)pow(2.0,-24.0);
	reference_value *= (float)pow(16.0,(double)(tmpa - 64));
	if(sign) {
		reference_value = -reference_value;
	}

	if((!spherical_harm)&&(!second_order)&&(!additional_flags)) {
		if(number_of_bits != 0) {
			i = 11;
			bboff = 0;
			index = 0;
			tbits = 0;
			total = (int)(((therec->bds_size - 11) * 8 - unused_bits)/ number_of_bits);
			switch(therec->grid_number) {
			case 23:
			case 24:
				polefirst = 1;
				npole = 37;
				break;
			case 26:
				polefirst = 1;
				npole = 72;
				break;
			case 63:
			case 64:
				polefirst = 1;
				npole = 91;
				break;
			case 21:
			case 22:
				polefirst = 0;
				npole = 37;
				break;	
			case 25:
				polefirst = 0;	
				npole = 72;
				break;
			case 61:
			case 62:
				polefirst = 0;
				npole = 91;
				break;
			case 50:
				polefirst = 0;
				npole = 0;
				break;
				
			}
			if(integer) {
				if(npole != 0) {
					data = (void*)NclMalloc((unsigned)sizeof(int)*(total + (npole -1)) );
				} else {
					data = (void*)NclMalloc((unsigned)sizeof(int)*total);
				}
			} else {
				if(npole != 0) {
					data = (void*)NclMalloc((unsigned)sizeof(float)*(total+ (npole -1)) );
				} else {
					data = (void*)NclMalloc((unsigned)sizeof(float)*total);
				}
			}
			*outdat = data;
			if((polefirst)&&(npole >0)) {
				X = UnsignedCnvtToDecimal(4,&(bds[i]));
				X = X << bboff;
				X = X >> (isize - number_of_bits);
				if(integer) {
					((int*)data)[0] = (int)(reference_value + (X * pow(2.0,(double)binary_scale_factor)))/pow(10.0,(double)(decimal_scale_factor));

				} else {
					((float*)data)[0] = (float)(reference_value + (X * pow(2.0,(double)binary_scale_factor)))/pow(10.0,(double)(decimal_scale_factor));
				}
				for(index = 1; index < npole; index++) {
					if(integer) {
						((int*)data)[index] = ((int*)data)[0];
					} else {
						((float*)data)[index] = ((float*)data)[0];
					}
				}
				tbits += number_of_bits;
				i = (int)(tbits/8.0) + 11;
				bboff = tbits % 8;
				if(integer) {
					data = (void*)&(((int*)data)[index]);
				} else {
					data = (void*)&(((float*)data)[index]);
				}
				index = 1;
			}
			while(index < total) {
				X = UnsignedCnvtToDecimal(4,&(bds[i]));
				X = X << bboff;
				X = X >> (isize - number_of_bits);

				if(integer) {
					((int*)data)[index] = (int)(reference_value + (X * pow(2.0,(double)binary_scale_factor)))/pow(10.0,(double)(decimal_scale_factor));

				} else {
					((float*)data)[index] = (float)(reference_value + (X * pow(2.0,(double)binary_scale_factor)))/pow(10.0,(double)(decimal_scale_factor));
				}
				tbits += number_of_bits;
				i = (int)(tbits/8.0) + 11;
				bboff = tbits % 8;
				index++;
			}
			if(!(polefirst)&&(npole > 0)) {
				for( ;index < total + npole -1;index++) {
					if(integer) {
                                                ((int*)data)[index] = ((int*)data)[index -1];
                                        } else {
                                                ((float*)data)[index] = ((float*)data)[index -1];
                                        }
				}
			}
		} else {
			NhlPError(NhlWARNING,NhlEUNKNOWN,"GribUnPack : constant field\n");
		}
	} else {
		if(spherical_harm)
			NhlPError(NhlWARNING,NhlEUNKNOWN,"GribUnPack : Spherical Harmonics Detected can't un pack\n");
		if(second_order)
			NhlPError(NhlWARNING,NhlEUNKNOWN,"GribUnPack : Second Order Detected can't un pack\n");
		*outdat = NULL;
		*missing_value = NULL;
	}
	return(integer);
}

void GdsMEGrid
#if NhlNeedProto
(GribParamList* thevarrec, float** lat, int* n_dims_lat, int** dimsizes_lat, float** lon, int* n_dims_lon, int** dimsizes_lon)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon)
GribParamList* thevarrec;
float** lat;
int* n_dims_lat;
int** dimsizes_lat;
float** lon;
int* n_dims_lon;
int** dimsizes_lon;
#endif
{
			*lat = NULL;
			*n_dims_lat = 0;
			*dimsizes_lat = NULL;
			*lon = NULL;
			*n_dims_lon= 0;
			*dimsizes_lon= NULL;
}
void GdsGNGrid
#if NhlNeedProto
(GribParamList* thevarrec, float** lat, int* n_dims_lat, int** dimsizes_lat, float** lon, int* n_dims_lon, int** dimsizes_lon)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon)
GribParamList* thevarrec;
float** lat;
int* n_dims_lat;
int** dimsizes_lat;
float** lon;
int* n_dims_lon;
int** dimsizes_lon;
#endif
{
			*lat = NULL;
			*n_dims_lat = 0;
			*dimsizes_lat = NULL;
			*lon = NULL;
			*n_dims_lon= 0;
			*dimsizes_lon= NULL;
}
void GdsLEGrid
#if NhlNeedProto
(GribParamList* thevarrec, float** lat, int* n_dims_lat, int** dimsizes_lat, float** lon, int* n_dims_lon, int** dimsizes_lon)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon)
GribParamList* thevarrec;
float** lat;
int* n_dims_lat;
int** dimsizes_lat;
float** lon;
int* n_dims_lon;
int** dimsizes_lon;
#endif
{
			*lat = NULL;
			*n_dims_lat = 0;
			*dimsizes_lat = NULL;
			*lon = NULL;
			*n_dims_lon= 0;
			*dimsizes_lon= NULL;
}
void GdsGAGrid
#if NhlNeedProto
(GribParamList* thevarrec, float** lat, int* n_dims_lat, int** dimsizes_lat, float** lon, int* n_dims_lon, int** dimsizes_lon)
#else
(GribParamList* thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon)
GribParamList* thevarrec;
float** lat;
int* n_dims_lat;
int** dimsizes_lat;
float** lon;
int* n_dims_lon;
int** dimsizes_lon;
#endif
{
			*lat = NULL;
			*n_dims_lat = 0;
			*dimsizes_lat = NULL;
			*lon = NULL;
			*n_dims_lon= 0;
			*dimsizes_lon= NULL;
}
void GdsSTGrid
#if NhlNeedProto
(GribParamList* thevarrec, float** lat, int* n_dims_lat, int** dimsizes_lat, float** lon, int* n_dims_lon, int** dimsizes_lon)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon)
GribParamList* thevarrec;
float** lat;
int* n_dims_lat;
int** dimsizes_lat;
float** lon;
int* n_dims_lon;
int** dimsizes_lon;
#endif
{
			*lat = NULL;
			*n_dims_lat = 0;
			*dimsizes_lat = NULL;
			*lon = NULL;
			*n_dims_lon= 0;
			*dimsizes_lon= NULL;
}
void GdsOLGrid
#if NhlNeedProto
(GribParamList* thevarrec, float** lat, int* n_dims_lat, int** dimsizes_lat, float** lon, int* n_dims_lon, int** dimsizes_lon)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon)
GribParamList* thevarrec;
float** lat;
int* n_dims_lat;
int** dimsizes_lat;
float** lon;
int* n_dims_lon;
int** dimsizes_lon;
#endif
{
			*lat = NULL;
			*n_dims_lat = 0;
			*dimsizes_lat = NULL;
			*lon = NULL;
			*n_dims_lon= 0;
			*dimsizes_lon= NULL;
}

void GdsCEGrid
#if NhlNeedProto
(GribParamList* thevarrec, float** lat, int* n_dims_lat, int** dimsizes_lat, float** lon, int* n_dims_lon, int** dimsizes_lon)
#else
(thevarrec,lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon)
GribParamList* thevarrec;
float** lat;
int* n_dims_lat;
int** dimsizes_lat;
float** lon;
int* n_dims_lon;
int** dimsizes_lon;
#endif
{
	char *gds;
	int la1;
	int lo1;
	int la2;
	int lo2;
	int di;
	int dj;
	int latXlon;
	int idir;
	int jdir;
	int has_dir_inc;
	int vectors;
	int is_thinned_lat;
	int is_thinned_lon;
	char tmp[4];
	int sign;
	int i;
	
	

	if((thevarrec->thelist != NULL)&&(thevarrec->thelist->rec_inq != NULL)) {
		gds = thevarrec->thelist->rec_inq->gds;
		if(gds != NULL) {
			*dimsizes_lat = (int*)NclMalloc(sizeof(int));
			*dimsizes_lon = (int*)NclMalloc(sizeof(int));
/*
* Check for thinned grids
*/
			is_thinned_lat = ((gds[6]==(char)0377)&&(gds[7] ==(char)0377)) ? 1 : 0;
			is_thinned_lon = ((gds[8]==(char)0377)&&(gds[9] ==(char)0377)) ? 1 : 0;
			idir = ((char)0200 & gds[27]) ? -1 : 1;
			jdir = ((char)0100 & gds[27]) ? 1 : -1;
			latXlon = ((char)0040 & gds[27])? 0 : 1; 
			sign = ((char)0200 & gds[10] )? -1 : 1;
			tmp[0] = (char)0177 & gds[10];
			tmp[1] = gds[11];
			tmp[2] = gds[12];
			la1 = sign * CnvtToDecimal(3,tmp);
			sign = ((char)0200 & gds[13] )? -1 : 1;
			tmp[0] = (char)0177 & gds[13];
			tmp[1] = gds[14];
			tmp[2] = gds[15];
			lo1 = sign * CnvtToDecimal(3,tmp);
			sign = ((char)0200 & gds[17] )? -1 : 1;
			tmp[0] = (char)0177 & gds[17];
			tmp[1] = gds[18];
			tmp[2] = gds[19];
			la2 = sign * CnvtToDecimal(3,tmp);
			sign = ((char)0200 & gds[20] )? -1 : 1;
			tmp[0] = (char)0177 & gds[20];
			tmp[1] = gds[21];
			tmp[2] = gds[22];
			lo2 = sign * CnvtToDecimal(3,tmp);
			di = CnvtToDecimal(2,&gds[23]);
			dj = CnvtToDecimal(2,&gds[25]);
			has_dir_inc = ((char)0200 & gds[16]) ? 1 : 0;

			if(is_thinned_lat) {
			
				fprintf(stdout,"thinned lat\n");
			*lat = NULL;
			*n_dims_lat = 0;
			*dimsizes_lat = NULL;
			*lon = NULL;
			*n_dims_lon= 0;
			*dimsizes_lon= NULL;
			} else if (is_thinned_lon) {
				fprintf(stdout,"thinned lon\n");
			*lat = NULL;
			*n_dims_lat = 0;
			*dimsizes_lat = NULL;
			*lon = NULL;
			*n_dims_lon= 0;
			*dimsizes_lon= NULL;

			} else {
				*(*dimsizes_lon) = CnvtToDecimal(2,&(gds[6]));
				*(*dimsizes_lat) = CnvtToDecimal(2,&(gds[8]));
				*n_dims_lat = 1;
				*n_dims_lon = 1;
				*lat = (float*)NclMalloc((unsigned)sizeof(float)* (*(*dimsizes_lat)));
				*lon = (float*)NclMalloc((unsigned)sizeof(float)* (*(*dimsizes_lon)));
				for(i = 0;i < *(*dimsizes_lat) ; i++) {
					(*lat)[i] = (float)(la1 + jdir * i * dj) / 1000.0;
				}
				for(i = 0;i < *(*dimsizes_lon) ; i++) {
					(*lon)[i] = (float)(lo1 + idir * i * di) / 1000.0;
				}
			}
		} else {
			*lat = NULL;
			*n_dims_lat = 0;
			*dimsizes_lat = NULL;
			*lon = NULL;
			*n_dims_lon= 0;
			*dimsizes_lon= NULL;
		}
	}

	return;
}




GridInfoRecord grid_gds[] = {
		GenericUnPack,GdsCEGrid,"Cylindrical Equidistant Projection Grid", /*0*/
/**/		GenericUnPack,GdsMEGrid,"Mercator Projection Grid", /*1*/
/**/		GenericUnPack,GdsGNGrid,"Gnomonic Projection Grid", /*2*/
/**/		GenericUnPack,GdsLEGrid,"Lambert Conformal Secant or Tangent, Conical or bipolar", /*3*/
/**/		GenericUnPack,GdsGAGrid,"Gaussian Latitude/Longitude Grid", /*4*/
/**/		GenericUnPack,GdsSTGrid,"Polar Stereographic Projection Grid", /*5*/
/**/		GenericUnPack,GdsOLGrid,"Oblique Lambert conformal, secant or tangent, conical or bipolar, projection", /*13*/
		NULL,NULL,"Spherical Harmonic Coefficients", /*50*/
		NULL,NULL,"Space View perspecitve or orthographic grid", /*90*/
		NULL,NULL,"Arakawa semi-staggered E-grid on rotated latitude/longitude grid-point array", /*201*/
		NULL,NULL,"Arakawa filled E-grid on rotated latitude/longitude grid-point array" /*202*/
		
};

GridInfoRecord grid[] = {
		NULL,NULL,"1679-point (73x23) Mercator grid with (0,0) at (0W,48.09S), (73,23) at (0W,48.09N); I increasing eastward, Equator at J=12. Grid increment of 5 degs of longitude", /*01*/
		GenericUnPack,GetGrid_2,"10512-point (144x73) global longitude-latitude grid.  (0,0) at 0E, 90N, latitude grid.  (0,0) at 0E, 90N, matrix layout.  N.B.: prime meridian not duplicated.", /*2*/
		GenericUnPack,GetGrid_3,"65160-point (360x181) global longitude-latitude grid.  (0,0) at 0E, 90N, matrix layout.  N.B.: prime meridian not duplicated.", /*3*/
		GenericUnPack,GetGrid_4,"259920-point (720x361) global lon/lat grid. (0,0) at 0E, 90N; matrix layout; prime meridian not duplicated", /*4*/
		GenericUnPack,GetGrid_5,"3021-point (53x57) N. Hemisphere stereographic grid oriented 105W; Pole at (27,49). (LFM analysis)",/*5*/
		GenericUnPack,GetGrid_6,"2385-point (53x45) N. Hemisphere polar stereographic grid oriented 105W; Pole at (27,49). (LFM Forecast)", /*6*/
		IFOSUnPack,GetGrid_21,"1369-point (37x37) longitude-latitude grid. 0-180E, 0-90N", /*21*/
		IFOSUnPack,GetGrid_22,"1369-point (37x37) longitude-latitude grid. 180W-0, 0-90N", /*22*/
		IFOSUnPack,GetGrid_23,"1369-point (37x37) longitude-latitude grid. 0-180E, 90S-0", /*23*/
		IFOSUnPack,GetGrid_24,"1369-point (37x37) longitude-latitude grid. 180W-0, 90S-0", /*24*/
		IFOSUnPack,GetGrid_25,"1368-point (72x19) longitude-latitude grid. 0-355E, 0-90N", /*25*/
		IFOSUnPack,GetGrid_26,"1368-point (72x19) longitude-latitude grid. 0-355E, 90S-0", /*26*/
		GenericUnPack,GetGrid_27,"4225-point (65x65) N. Hemisphere polar stereographic grid oriented 80W; Pole at (33,33).", /*27*/
		GenericUnPack,GetGrid_28,"4225-point (65x65) S. Hemisphere polar stereographic grid oriented 100E; Pole at (33,33).", /*28*/
		GenericUnPack,GetGrid_29,"5365-point (145x37) N. Hemisphere longitude/latitude grid for latitudes 0N to 90N; (0,0) at (0E,0N).", /*29*/
		GenericUnPack,GetGrid_30,"5365-point (145x37) S. Hemisphere longitude/latitude grid for latitudes 90S to 0S; (0,0) at (0E,90S).", /*30*/
		GenericUnPack,GetGrid_33,"8326-point (181x46) N. Hemisphere longitude/latitude grid for latitudes 0N to 90N; (0,0) at (0E,0N).", /*33*/
		GenericUnPack,GetGrid_34,"8326-point (181x46) S. Hemisphere longitude/latitude grid for latitudes 90S to 0S; (0,0) at (0E,90S).", /*34*/
		NULL,NULL,"3447-point (73x73) \"Thinned\" longitude-latitude grid. 60E-330E, 0-90N", /*37*/
		NULL,NULL,"3447-point (73x73) \"Thinned\" longitude-latitude grid. 150E-60E, 0-90N", /*38*/
		NULL,NULL,"3447-point (73x73) \"Thinned\" longitude-latitude grid. 240E-150E, 0-90N", /*39*/
		NULL,NULL,"3447-point (73x73) \"Thinned\" longitude-latitude grid. 330E-240E, 0-90N", /*40*/
		NULL,NULL,"3447-point (73x73) \"Thinned\" longitude-latitude grid. 60E-330E, 90S-0", /*41*/
		NULL,NULL,"3447-point (73x73) \"Thinned\" longitude-latitude grid. 150E-60E, 90S-0", /*42*/
		NULL,NULL,"3447-point (73x73) \"Thinned\" longitude-latitude grid. 240E-150E,90S-0", /*43*/
		NULL,NULL,"3447-point (73x73) \"Thinned\" longitude-latitude grid. 330E-240E, 90S-0", /*44*/
		GenericUnPack,GetGrid_45,"41760-point (288x145) Global Latitude/Longitude 1.25 deg Resoulution. 0E-358.75E, 90N-90S",/*45*/
		IFOS50UnPack,GetGrid_50,"1188-point (36x33) longitude-latitude grid. 140.0W-52.5W, 20N-60N", /*50*/
		GenericUnPack,GetGrid_55,"6177-point (87x71) N. Hemisphere polar tereographic grid oriented 105W; Pole at (44,38). (2/3 bedient NH sfc anl)", /*55*/
		GenericUnPack,GetGrid_56,"6177-point (87x71) N. Hemisphere polar stereographic grid oriented 105W; Pole at (40,73). (1/3 bedient NA sfc anl)", /*56*/
		IFOSUnPack,GetGrid_61,"4186-point (91x46) longitude-latitude grid. 0-180E, 0-90N", /*61*/
		IFOSUnPack,GetGrid_62,"4186-point (91x46) longitude-latitude grid. 180W-0, 0-90N", /*62*/
		IFOSUnPack,GetGrid_63,"4186-point (91x46) longitude-latitude grid. 0-180E, 90S-0", /*63*/
		IFOSUnPack,GetGrid_64,"4186-point (91x46) longitude-latitude grid. 180W-0, 90S-0", /*64*/
		NULL,NULL,"12321-point (111x111) N. Hemisphere Lambert Conformal grid.  No fixed location; used by QLM Hurricane model.", /*75*/
		NULL,NULL,"12321-point (111x111) S. Hemisphere Lambert Conformal grid.  No fixed location; used by QLM Hurricane model.", /*76*/
		NULL,NULL,"12321-point (111x111) N. Hemisphere Mercator grid.  No fixed location; used by QLM Hurricane model.", /*77*/
		GenericUnPack,GetGrid_85,"32400-point (360x90) N. Hemisphere longitude/latitude grid; longitudes: 0.5E to 359.5E (0.5W); latitudes: 0.5N to 89.5N; origin (0,0) at (0.5E,0.5N)", /*85*/
		GenericUnPack,GetGrid_86,"32400-point (360x90) S. Hemisphere longitude/latitude grid; longitudes: 0.5E to 359.5E (0.5W); latitudes: 89.5S to 0.5S; origin (0,0) at (0.5E,89.5S)", /*86*/
		GenericUnPack,GetGrid_87,"5022-point (81x62) N. Hemisphere  polar stereographic grid oriented at 105W. Pole at (31.91, 112.53) Used for RUC.", /*87*/
		NULL,NULL,"12902-point (92x141 semi-staggered) lat. long., rotated such that center located at 52.0N, 111.0W; LL at 37.5W, 35S Unfilled E grid for 80 km ETA model", /*90*/
		NULL,NULL,"25803-point (183x141) lat. long., rotated such that center located at 52.0N, 111.0W; LL at 37.5W,35S Filled E grid for 80 km ETA model", /*91*/
		NULL,NULL,"24162-point (127x191 semi-staggered) lat. long., rotated such that center located at 41.0N, 97.0W; LL at 35W,25S Unfilled E grid for 40 km ETA model", /*92*/
		NULL,NULL,"48323-point (253x191)lat. long., rotated such that center located at 41.0N, 97.0W; LL at 35W ,25S Filled E grid for 40 km ETA model", /*93*/
		NULL,NULL,"18048-point (192x94) Global Gaussian T62 Latitude/Longitude Resolution.", /*98*/
		GenericUnPack,GetGrid_100,"6889-point (83x83) N. Hemisphere polar stereographic grid oriented 105W; Pole at (40.5,88.5). (NGM Original C-Grid)",  /*100*/
		GenericUnPack,GetGrid_101,"10283-point (113x91) N. Hemisphere polar stereographic grid oriented 105W; Pole at (58.5,92.5). (NGM \"Big C-Grid\")", /*101*/
		GenericUnPack,GetGrid_103,"3640-point (65x56) N. Hemisphere polar stereographic grid oriented 105W; Pole at (25.5,84.5) (used by ARL)", /*103*/
		GenericUnPack,GetGrid_104,"16170-point (147x110) N. Hemisphere polar stereographic grid oriented 105W; pole at (75.5,109.5). (NGM Super C grid)", /*104*/
		GenericUnPack,GetGrid_105,"6889-point (83x83) N. Hemisphere polar stereographic grid oriented 105W; pole at  (40.5,88.5).  (U.S. area subset of NGM Super C grid, used by ETA model)", /*105*/
		GenericUnPack,GetGrid_106,"19305-point (165x117) N. Hemisphere stereographic grid oriented 105W; pole at (80,176) Hi res. ETA (2 x resolution of Super C)", /*106*/
		GenericUnPack,GetGrid_107,"11040 point (120x92) N. Hemisphere stereographic grid oriented 105W; pole at (46,167) subset of Hi res. ETA; for ETA & MAPS/RUC", /*107*/
		NULL,NULL,"72960-point (384x190) Global Gaussian Latitude/Longitude T126 Resolution", /*126*/
		GenericUnPack,GetGrid_201,"4225-point (65x65) Hemispheric polar stereographic grid oriented 105W; pole at (33,33)", /*201*/
		GenericUnPack,GetGrid_202,"2795-point (65x43) National - CONUS polar stereographic oriented 105W; pole at (33,45)", /*202*/
		GenericUnPack,GetGrid_203,"1755-point (45x39) National - Alaska polar stereographic oriented 150W; pole at (27,37)", /*203*/
		NULL,NULL,"6324-point (93x68) National - Hawaii Mercator (0,0) is 25S,110E, (93,68) is 60.644S,109.129W", /*204*/
		GenericUnPack,GetGrid_205,"1755-point (45x39) National - Puerto Rico stereographic oriented 60W; pole at (27,57)", /*205*/
		NULL,NULL,"2091-point (51x41) Regional - Central MARD Lambert Conformal oriented 95W; pole at (30.00,169.745)", /*206*/
		GenericUnPack,GetGrid_205,"1715-point (49x35) Regional - Alaska polar stereographic oriented 150W; pole at 25,51", /*207*/
		NULL,NULL,"783-point (29x27) Regional - Hawaii mercator (0,0) is 9.343N,167.315W, (29,27) is 28.092N,145.878W", /*208*/
		NULL,NULL,"8181-point (101x81) Regional - Centeral US MARD - Double Res. Lambert Conformal oriented 95W; pole at (59.000,338.490)", /* 209*/
		NULL,NULL,"625-point (25x25) Regional - Puerto Rico mercator (0,0) is 9.000N,77.00W (25,25) is 26.422,58.625", /*210*/
		NULL,NULL,"6045-point (93x65) Regional - CONUS lambert conformal oriented 95W; pole at (53.000,178.745)", /*211*/
		NULL,NULL,"23865-point (185x129) Regional - CONUS - double resolution lambert conformal oriented 95W; pole at (105.000,256.490)", /* 212 */
		GenericUnPack,GetGrid_213,"10965-point (129x85) National - CONUS - Double Resolution polar stereographic oriented 105W; pole at (65,89)", /*213*/
		GenericUnPack,GetGrid_214,"6693-point (97x69) Regional - Alaska - Double Resolution polar stereographic oriented 150W; pole at (49,101)", /*214*/
};
