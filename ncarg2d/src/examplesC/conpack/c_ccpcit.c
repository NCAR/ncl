/*
 * $Id: c_ccpcit.c,v 1.3 1994-06-21 14:59:10 haley Exp $
 */

#include <stdio.h>
#include <math.h>

#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define   K   40
#define   N   40
#define   LRWK   1000
#define   LIWK   1000

float cit[10] = {1.,2.,3.,4.,5.,6.,7.,8.,9.,0.};
float lit[10] = {2, 2, 2, 2, 2, 2, 2, 2, 2, 0};

#define WSTYPE SED_WSTYPE
#define WKID   1

main()
{
	float z[N][K], rwrk[LRWK];
	int i, m, iwrk[LIWK];
	extern void getdat();

	getdat (z, K, &m, N) ;
/*
 * Open GKS
 */
	gopen_gks ("stdout",0);
	gopen_ws (WKID, NULL, WSTYPE);
	gactivate_ws(WKID);
/*
 * Change nice values to be steps of 1/3. (1/3, 2/3, 3/3...)
 * Draw labels at every 5th contour level no matter which contour
 * level interval is chosen.
 */
	for( i = 0; i < 10; i++ ) {
		c_cpseti ("PAI - PARAMETER ARRAY INDEX",i+1);
		c_cpsetr ("CIT - CONTOUR INTERVAL TABLE",cit[i]);
		c_cpseti ("LIT - LABEL INTERVAL TABLE",lit[i]);
	}
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


