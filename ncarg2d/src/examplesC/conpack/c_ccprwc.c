/*
 * $Id: c_ccprwc.c,v 1.1 1994-08-04 16:31:19 haley Exp $
 */

#include <stdio.h>
#include <math.h>

#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define WSTYPE SED_WSTYPE
#define WKID   1
#define K    400
#define N    400
#define LRWK    2000
#define LIWK    2000

float z[N][K];

main()
{
	float rwrk[LRWK];
	int i, j, k, l, m, n, o, p, iwrk[LIWK], ncl;
	extern void getdat();

	getdat ();
/*
 * Open GKS
 */
	gopen_gks ("stdout",0);
	gopen_ws (WKID, NULL, WSTYPE);
	gactivate_ws(WKID);
/*
 * Initialize Conpack
 */
	c_cprect(&z[0][0],K,K,N,rwrk,LRWK,iwrk,LIWK);
/*
 * Draw perimeter
 */
	c_cpback(&z[0][0], rwrk, iwrk);
/*
 * Turn on line labels for every line
 */
	c_cppkcl(&z[0][0], rwrk, iwrk);
    c_cpgeti("NCL",&ncl);
	for( i=1; i <= ncl; i++ ) {
		c_cpseti("PAI",i);
		c_cpseti("CLU",3);
	}
/*
 * Set RWC so that labels come out on some lines
 */
	c_cpseti("RWC",125);
/*
 * Draw Contours
 */
	c_cpcldr(&z[0][0],rwrk,iwrk);
/*
 * Close frame and close GKS
 */
	c_frame();
	gdeactivate_ws(WKID);
	gclose_ws(WKID);
	gclose_gks();
}

void getdat ()
{
    int i, j;

    for( i = 1; i <= K; i++ ) {
        for( j = 1; j <= N; j++ ) {
            z[j-1][i-1] = 10.e-8*(-16.*(float)(i*i*j) + 34.*(float)(i*j*j) - (float)(6*i) + 93.);
        }
    }
    return;
}

