/*
 * $Id: c_ccpclc.c,v 1.1 1994-05-31 22:28:16 haley Exp $
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
	float z[N][K], rwrk[LRWK], cval;
	int i, ncon, m, iwrk[LIWK];
	extern void getdat();
	extern void color();

	getdat (z, K, &m, N);
/*
 * Open GKS
 */
	c_opngks();
/*
 * Set up color table
 */
	color();
/*
 * Call conpack normally
 */
	c_cprect((float *)z,K,m,N,rwrk,LRWK,iwrk,LIWK);

	c_cppkcl((float *)z, rwrk, iwrk);
/*
 * Set line color to red if it's negative, green if it's zero, 
 * and blue if it's positive.
*/
	c_cpgeti("NCL - NUMBER OF CONTOUR LEVELS",&ncon);
	for( i = 1; i <= ncon; i++ ) {
		c_cpseti("PAI - PARAMETER ARRAY INDEX",i);
		c_cpgetr("CLV - CONTOUR LEVEL VALUES",&cval);
		if (cval < 0.0) {
			c_cpseti("CLC - CONTOUR LINE COLOR INDEX",1);
		}
		else if (cval == 0.0) {
			c_cpseti("CLC - CONTOUR LINE COLOR INDEX",2);
		}
		else {
			c_cpseti("CLC - CONTOUR LINE COLOR INDEX",3);
		}
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

void color()
{
	Gcolr_rep rgb;

	rgb.rgb.red = 1.; rgb.rgb.green = 1.; rgb.rgb.blue = 1.;
	gset_colr_rep (1,0,&rgb);
	rgb.rgb.red = 1.; rgb.rgb.green = 0.; rgb.rgb.blue = 0.;
	gset_colr_rep (1,1,&rgb);
	rgb.rgb.red = 0.; rgb.rgb.green = 1.; rgb.rgb.blue = 0.;
	gset_colr_rep (1,2,&rgb);
	rgb.rgb.red = 0.; rgb.rgb.green = 0.; rgb.rgb.blue = 1.;
	gset_colr_rep (1,3,&rgb);
	return;
}
