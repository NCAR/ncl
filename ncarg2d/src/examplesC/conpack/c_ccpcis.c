/*
 * $Id: c_ccpcis.c,v 1.2 1994-06-21 14:59:09 haley Exp $
 */

#include <stdio.h>
#include <math.h>

#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define   K   40
#define   N   40
#define   LRWK   1000
#define   LIWK   1000

#define WSTYPE SED_WSTYPE
#define WKID   1

main()
{
	float z[N][K], rwrk[LRWK];
	int m, iwrk[LIWK];
	extern void getdat();

	getdat (z, K, &m, N) ;
/*
 * Open GKS
 */
	gopen_gks("stdout",0);
	gopen_ws(WKID, NULL, WSTYPE);
	gactivate_ws(WKID);
/*
 * Change Contour levels to come at increments of 15
 * Draw labels at every other contour level.
 */
	c_cpsetr ("CIS - CONTOUR INTERVAL SPECIFIER",15.0);
	c_cpseti ("LIS - LABEL INTERVAL SPECIFIER",2);
	c_cpsetr ("CMN - CONTOUR MINIMUM",-5.0);
	c_cpsetr ("CMX - CONTOUR MAXIMUM",75.0);
/*
 * Initialize Conpack
 */
	c_cprect((float *)z,K,m,N,rwrk,LRWK,iwrk,LIWK);
/*
 * Draw perimeter
 */
	c_cpback((float *)z, rwrk, iwrk);
/*
 * Draw Contours
 */
	c_cpcldr((float *)z,rwrk,iwrk);
/*
 * Close frame and close GKS
 */
	c_frame();
	gdeactivate_ws(WKID);
	gclose_ws(WKID);
	gclose_gks();
}

void getdat (z, n, m, k)
float *z;
int k, *m, n;
{
    int i,j,l;

    l = 0;
    *m = k;
    for( j = 1; j <= n; j++ ) {
        for( i = 1; i <= *m; i++ ) {
            z[l++] = 10.e-5*(-16.0*(float)(i*i*j) + 34.0*(float)(i*j*j) - (float)(6.0*i) + 93.0);
        }
    }
    return;
}

