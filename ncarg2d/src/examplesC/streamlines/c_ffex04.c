/*
 *  $Id: c_ffex04.c,v 1.1 1994-07-28 20:13:27 haley Exp $
 */
#include <stdio.h>
#include <math.h>

#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define WSTYPE SED_WSTYPE
#define WKID   1

#define M 20
#define N 36
#define NPR  155

float u[N][M],v[N][M];

main()
{
	int i, j, k, idm;
	float wrk[2*M*N], xdm;
/*
 *  Open gks, open and activate a workstation.
 */
    gopen_gks("stdout",0);
    gopen_ws(WKID, NULL, WSTYPE);
    gactivate_ws(WKID);

	c_stsetr("DFM -- Differential Magnitude", 0.01);

	c_stseti("MAP -- Mapping Mode", 2);
	c_stsetr("WDL -- Window Left", -20.0);
	c_stsetr("WDR -- Window Right", 20.0);
	c_stsetr("WDB -- Window Bottom", -20.0);
	c_stsetr("WDT -- Window Top", 20.0);
	c_stsetr("XC1 -- Lower X Bound", 1.0);
	c_stsetr("XCM -- Upper X Bound", 20.0);
	c_stsetr("YC1 -- Lower Y Bound", 0.0);
	c_stsetr("YCN -- Upper Y Bound", 360.0);

	for( k = 1; k >= 0; k-- ) {

		c_stseti("TRT -- Transform Type", k);

		for( i = 0; i < M; i++ ) {
			for( j = 0; j < N; j++ ) {
				u[j][i]=1.0;
				v[j][i]=0.0;
			}
		}

		c_stinit((float *)u,M,(float *)v,M,&xdm,idm,M,N,wrk,2*M*N);
		c_stream((float *)u,(float *)v,&xdm,&idm,0,wrk);

		for( i = 0; i < M; i++ ) {
			for( j = 0; j < N; j++ ) {
				u[j][i]=0.0;
				v[j][i]=1.0;
			}
		}

		c_stinit((float *)u,M,(float *)v,M,&xdm,idm,M,N,wrk,2*M*N);
		c_stream((float *)u,(float *)v,&xdm,&idm,0,wrk);

		c_perim(1,0,1,0);
		c_frame();

	}
/*
 * Deactivate and close workstation, close GKS.
 */
    gdeactivate_ws (WKID);
    gclose_ws(WKID);
    gclose_gks();
}
