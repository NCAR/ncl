/*
 * $Id: c_ccpcir.c,v 1.1 1994-05-31 22:28:14 haley Exp $
 */

#include <stdio.h>
#include <math.h>

#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define   M   30
#define   N   30
#define   LRWK   3500
#define   LIWK   3500

#define min(x,y)  ((x) < (y) ? (x) : (y))
#define max(x,y)  ((x) > (y) ? (x) : (y))

float z[N][M];

main()
{
	float rwrk[LRWK];
	float thetmn =  25., thetmx = 125., rhomn = 0.5, rhomx = 5.;
	int iwrk[LIWK];
	extern void mkdat();
/*
 * Open GKS
 */
	c_opngks();
/*
 * Generate some data to contour
 */
	mkdat (2, z);
/*
 * Open GKS
 */
	c_opngks();
/*
 * Turn off clipping.
 */
	gset_clip_ind (GIND_NO_CLIP);
/*
 * Tell CONPACK that the SET call has been done, force it to generate X
 * coordinates that are longitudes and Y coordinates that are latitudes,
 * turn on mapping to an EZMAP background. Define the out-of-range value
 * (returned by MAPTRN for an unprojectable point).
 */
	c_set (0.05,0.95,0.05,0.95,-rhomx,rhomx,-rhomx,rhomx,1);
	c_cpseti ("SET - DO SET-CALL FLAG",0);
	c_cpsetr ("XC1 - X COORDINATE AT INDEX 1",rhomn);
	c_cpsetr ("XCM - X COORDINATE AT INDEX M",rhomx);
	c_cpsetr ("YC1 - Y COORDINATE AT INDEX 1",thetmn);
	c_cpsetr ("YCN - Y COORDINATE AT INDEX N",thetmx);
	c_cpseti ("MAP - MAPPING FLAG",2);
/*
 * Initialize the drawing of the first contour plot.
 */
	c_cprect ((float *)z,M,M,N,rwrk,LRWK,iwrk,LIWK);
/*
 * Draw Contours
 */
	c_cpcldr((float *)z,rwrk,iwrk);
/*
 * Close frame and close GKS
 */
	c_frame();
	c_clsgks();
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
