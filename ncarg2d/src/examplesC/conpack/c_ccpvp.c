/*
 * $Id: c_ccpvp.c,v 1.2 1994-06-21 14:59:34 haley Exp $
 */

#include <stdio.h>
#include <math.h>

#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define M    40
#define N    40
#define LRWK    1000
#define LIWK    1000
#define RMNLON    -125.
#define RMXLON    -65.
#define RMNLAT    20.
#define RMXLAT    50.

#define min(x,y)    ((x) < (y) ? (x) : (y))
#define max(x,y)    ((x) > (y) ? (x) : (y))

#define WSTYPE SED_WSTYPE
#define WKID   1

main()
{
	float z[N][M], rwrk[LRWK], rlon1[2], rlon2[2], rlat1[2], rlat2[2];
	int iwrk[LIWK];
	extern void mkdat();

	rlon1[0] = RMNLON;
	rlon2[0] = RMXLON;
	rlat1[0] = RMNLAT;
	rlat2[0] = RMXLAT;
	rlon1[1] = rlon2[1] = rlat1[1] = rlat2[1] = 0.0;
/*
 * Open GKS
 */
	gopen_gks ("stdout",0);
	gopen_ws (WKID, NULL, WSTYPE);
	gactivate_ws(WKID);
/*
 * Draw first plot in upper left corner of plot
 */
	mkdat(1,z);
	c_cpsetr("VPL - VIEWPORT LEFT",0.02);
	c_cpsetr("VPR - VIEWPORT RIGHT",0.48);
	c_cpsetr("VPB - VIEWPORT BOTTOM",0.52);
	c_cpsetr("VPT - VIEWPORT TOP",0.98);
	c_cprect((float *)z,M,M,N,rwrk,LRWK,iwrk,LIWK);
	c_cpback((float *)z, rwrk, iwrk);
	c_cpcldr((float *)z,rwrk,iwrk);
/*
 * Draw second plot in upper right corner of plot
 */
	mkdat(2,z);
	c_cpsetr("VPL - VIEWPORT LEFT",0.52);
	c_cpsetr("VPR - VIEWPORT RIGHT",0.98);
	c_cpsetr("VPB - VIEWPORT BOTTOM",0.52);
	c_cpsetr("VPT - VIEWPORT TOP",0.98);
	c_cpsetr("VPS - VIEWPORT SHAPE",-0.33);
	c_cprect((float *)z,M,M,N,rwrk,LRWK,iwrk,LIWK);
	c_cpback((float *)z, rwrk, iwrk);
	c_cpcldr((float *)z,rwrk,iwrk);
/*
 * Draw third plot in lower left corner of plot
 */
	mkdat(3,z);
	c_cpsetr("VPL - VIEWPORT LEFT",0.02);
	c_cpsetr("VPR - VIEWPORT RIGHT",0.48);
	c_cpsetr("VPB - VIEWPORT BOTTOM",0.02);
	c_cpsetr("VPT - VIEWPORT TOP",0.48);
	c_cpsetr("VPS - VIEWPORT SHAPE",-1.5);
	c_cpseti("MAP - MAPPING FLAG",2);
	c_cprect((float *)z,M,M,N,rwrk,LRWK,iwrk,LIWK);
	c_cpback((float *)z, rwrk, iwrk);
	c_cpcldr((float *)z,rwrk,iwrk);
/*
 * Draw fourth plot in lower right corner of plot
 *
 * Draw Lat/Lon lines at 10 degree intervals.
 * Draw political & continental outlines.
 */
	c_mapsti ("GR - GRID",10);
	c_mapstc ("OU - OUTLINE DATASET","PS");
/*
 * Draw a Satellite view over the United States
 */
	c_maproj ("SV - SATELLITE-VIEW",35.,-100.,0.);
	c_mapset ("MA",rlat1,rlon1,rlat2,rlon2);
/*
 * Draw map in the lower right corner of the frame
 */
	c_mappos (0.52,0.98,0.02,0.48);
/*
 * Don't draw a box around the globe
 */
	c_mapsti ("PE",0);
/*
 * Draw map. 
 */
	c_mapdrw();
/*
 * Tell CONPACK that the SET call has been done, force it to generate X
 * coordinates that are longitudes and Y coordinates that are latitudes,
 * turn on mapping to an EZMAP background. Define the out-of-range value
 * (returned by MAPTRN for an unprojectable point).
 */
	mkdat(4,z);
	c_cpseti ("SET - DO SET-CALL FLAG",0);
	c_cpsetr ("XC1 - X COORDINATE AT I = 1",RMNLON);
	c_cpsetr ("XCM - X COORDINATE AT I = M",RMXLON);
	c_cpsetr ("YC1 - Y COORDINATE AT J = 1",RMNLAT);
	c_cpsetr ("YCN - Y COORDINATE AT J = N",RMXLAT);
	c_cpseti ("MAP - MAPPING FLAG",1);
	c_cpsetr ("ORV - OUT-OF-RANGE VALUE",1.e12);
/*
 * Initialize the drawing of the first contour plot.
 */
	c_cprect((float *)z,M,M,N,rwrk,LRWK,iwrk,LIWK);
	c_cpback((float *)z, rwrk, iwrk);
	c_cpcldr((float *)z,rwrk,iwrk);
/*
 * Close frame and close GKS
 */
	c_frame();
	gdeactivate_ws(WKID);
	gclose_ws(WKID);
	gclose_gks();
}

void mkdat(nctfr,zdat)
int nctfr;
float zdat[N][M];
{
/*
 * Create some data to contour
 *
 * nctfr is used to generate a random number, zdat is the data array
 */
	float zmin, zmax;
	float rlon, rlat;
	int i, j;
	extern float c_ggdpnt();

	c_ggdini (0.,1.,nctfr,.9);
	zmin = 1.e36;
	zmax = -1.e36;
	for( i = 0; i < M; i++ ) {
		rlon=.017453292519943*(-180.+360.*(float)i/(float)(M-1));
		for( j = 0; j < N; j++ ) {
			rlat=.017453292519943*(-90.+180.*(float)j/(float)(N-1));
			zdat[j][i] = c_ggdpnt(rlat,rlon)+.5*cos(4.*rlat);
			zmin = min(zmin,zdat[j][i]);
			zmax = max(zmax,zdat[j][i]);
		}
	}
	for( i = 0; i < M; i++ ) {
		for( j = 0; j < N; j++ ) {
			zdat[j][i] = ((zdat[j][i]-zmin)/(zmax-zmin))*130.-50.;
		}
	}
	return;
}


