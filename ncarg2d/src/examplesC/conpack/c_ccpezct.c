/*
 * $Id: c_ccpezct.c,v 1.2 1994-06-21 14:59:19 haley Exp $
 */

#include <stdio.h>
#include <math.h>

#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define M    50
#define N    50

#define min(x,y) ((x) < (y) ? (x) : (y))
#define max(x,y) ((x) > (y) ? (x) : (y))

#define WSTYPE SED_WSTYPE
#define WKID   1

main()
{
	float zreg[N][M];
	extern void mkdat();
/*
 * Get some data
 */
	mkdat (3,zreg);
/*
 * Open GKS
 */
	gopen_gks("stdout",0);
	gopen_ws(WKID, NULL, WSTYPE);
	gactivate_ws(WKID);
/*
 * Call CPEZCT
 */
	c_cpezct ((float *)zreg, M, N);
/*
 * Close GKS
 */
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
