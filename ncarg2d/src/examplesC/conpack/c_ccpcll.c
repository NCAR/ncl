/*
 * $Id: c_ccpcll.c,v 1.1 1994-06-08 14:44:31 haley Exp $
 */

#include <stdio.h>
#include <math.h>

#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define   K   40
#define   N   40
#define   LRWK   1000
#define   LIWK   1000

main()
{
	float z[N][K], rwrk[LRWK];
	int i, m, ncon, iwrk[LIWK];
	extern void getdat();

	getdat (z, K, &m, N) ;
/*
 * Open GKS
 */
	c_opngks();
/*
 * Call conpack normally
 */
	c_cprect((float *)z,K,m,N,rwrk,LRWK,iwrk,LIWK);

	c_cppkcl((float *)z, rwrk, iwrk);
/*
 * Set a different line width for each contour line
 */
	c_cpgeti("NCL - NUMBER OF CONTOUR LEVELS",&ncon);
	for( i = 1; i <= ncon; i++ ) {
		c_cpseti("PAI - PARAMETER ARRAY INDEX",i);
		c_cpsetr("CLL - CONTOUR LINE LINE WIDTH",(float)i/3.);
	}

	c_cpback((float *)z, rwrk, iwrk);
	c_cpcldr((float *)z,rwrk,iwrk);
/*
 * Close frame and close GKS
 */
	c_frame();
	c_clsgks();
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
