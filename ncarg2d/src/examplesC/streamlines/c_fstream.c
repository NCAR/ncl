/*
 *  $Id: c_fstream.c,v 1.1 1994-07-28 14:48:13 haley Exp $
 */
#include <stdio.h>
#include <math.h>

#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define WSTYPE SED_WSTYPE
#define WKID   1

#define M  21
#define N  25

float u[N][M], v[N][M];

main()
{
	float xdm, wrk[2*M*N];
	int idm;
	extern void mkdat();
/*
 * IDM is a dummy variable for STINIT and STREAM.
 *
 * Generate some data
 */
	mkdat();
/*
 *  Open GKS, open and activate a workstation.
 */
    gopen_gks("stdout",0);
    gopen_ws(WKID, NULL, WSTYPE);
    gactivate_ws(WKID);
/*
 * Select normalization transformation 0 (user coordinates are the same
 * as NDC coordinates), so that title is drawn at top of plot.
 */
	gsel_norm_tran (0);
/*
 * Call PLCHLQ to write the plot title.
 */
	c_plchlq (.5,.9765,"Example Streamlines Plot",16., 0.,0.);
/*
 * Define normalization transformation 1, and set up linear scaling.
 */
	c_set(0.1, 0.9, 0.1, 0.9,1.0, 21., 1.0, 25.,1);
/*
 * Tell Streamlines that SET has been called, and
 * set spacing of stream lines.
 */
	c_stseti("SET -- Set Call Flag", 0);
	c_stsetr("SSP -- Stream Spacing", 0.015);
/*
 * Initialize Streamlines, and draw streamlines
 */
	c_stinit((float *)u,M,(float *)v,M,&xdm,idm,M,N,wrk,2*M*N);
	c_stream((float *)u,(float *)v,&xdm,&idm,0,wrk);
/*
 * Close Frame
 */
	c_frame();
/*
 * Deactivate and close workstation, close GKS.
 */
    gdeactivate_ws (WKID);
    gclose_ws(WKID);
    gclose_gks();
}

void mkdat()
{
	float tpimx, tpjmx;
	int i, j;
/*
 * Specify horizontal and vertical vector components U and V on
 * the rectangular grid. And set up a special value area near the
 * center.
 */
	tpimx = 2.*3.14/(float)M;
	tpjmx = 2.*3.14/(float)N;
	for( j = 0; j < N; j++ ) {
		for( i = 0; i < M; i++ ) {
            u[j][i] = sin(tpimx*((float)(i+1)-1.));
            v[j][i] = sin(tpjmx*((float)(j+1)-1.));
		}
	}
	return;
}
