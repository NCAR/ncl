/*
 * $Id: c_ccphl.c,v 1.1 1994-06-08 14:44:39 haley Exp $
 */

#include <stdio.h>
#include <math.h>

#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define M    30
#define N    20
#define LIWK  500
#define LWRK  500

#define min(x,y) ((x) < (y) ? (x) : (y))
#define max(x,y) ((x) > (y) ? (x) : (y))

main()
{
	float z[N][M], rwrk[LWRK];
	int iwrk[LIWK];
	extern void mkdat();
/*
 * Get some data
 */
	mkdat (3,z);
/*
 * Open GKS
 */
	c_opngks();
/*
 * Turn off clipping.
 */
	gset_clip_ind (GIND_NO_CLIP);
/*
 * Set up High and Low options
 */
	c_cpseti("HLX - HIGH/LOW SEARCH RADIUS IN X",2);
	c_cpseti("HLY - HIGH/LOW SEARCH RADIUS IN Y",2);
	c_cpseti("HLO - HIGH/LOW OVERLAP FLAG",0);
	c_cpsetr("HLL - HIGH/LOW LINE WIDTH",3.0);
/*
 * Initialize the drawing of the first contour plot.
 */
	c_cprect ((float *)z,M,M,N,rwrk,LWRK,iwrk,LIWK);
/*
 * Draw background, contours, and labels
 */
	c_cpback((float *)z,rwrk,iwrk);
	c_cpcldr((float *)z,rwrk,iwrk);
	c_cplbdr((float *)z,rwrk,iwrk);
/*
 * Close frame and close GKS
 */
	c_frame();
	c_clsgks();
}

/*
 * Create some data to contour
 */
void mkdat(nctfr,zdat)
float zdat[N][M];
int nctfr;
{
	int i, j;
	float zmin, zmax, rlat, rlon;
	extern float c_ggdpnt();

/*
 * NCTFR is used to generate a random number, ZDAT is the data array
 */
	c_ggdini (0.,1.,nctfr,.9);
	zmin= 1.e36;
	zmax=-1.e36;
	for( i = 1; i <= M; i++ ) {
		rlon=.017453292519943*(-180.+360.*(float)(i-1)/(float)(M-1));
		for( j = 1; j <= N; j++ ) {
			rlat=.017453292519943*(-90.+180.*(float)(j-1)/(float)(N-1));
			zdat[j-1][i-1] = c_ggdpnt(rlat,rlon)+.5*cos(4.*rlat);
			zmin = min(zmin,zdat[j-1][i-1]);
			zmax = max(zmax,zdat[j-1][i-1]);
		}
	}
	for( i = 0; i < M; i++ ) {
		for( j = 0; j < N; j++ ) {
			zdat[j][i]=((zdat[j][i]-zmin)/(zmax-zmin))*130.-50.;
		}
	}
	return;
}

