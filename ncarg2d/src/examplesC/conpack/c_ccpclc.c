/*
 * $Id: c_ccpclc.c,v 1.2 1994-06-21 14:59:11 haley Exp $
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
	float z[N][K], rwrk[LRWK], cval;
	int i, ncon, m, iwrk[LIWK];
	extern void getdat();
	extern void color();

	getdat (z, K, &m, N);
/*
 * Open GKS
 */
	gopen_gks ("stdout",0);
	gopen_ws (WKID, NULL, WSTYPE);
	gactivate_ws(WKID);
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

void color()
{
	Gcolr_rep rgb[4];
	int i;

	rgb[0].rgb.red = 1.; rgb[0].rgb.green = 1.; rgb[0].rgb.blue = 1.;
	rgb[1].rgb.red = 1.; rgb[1].rgb.green = 0.; rgb[1].rgb.blue = 0.;
	rgb[2].rgb.red = 0.; rgb[2].rgb.green = 1.; rgb[2].rgb.blue = 0.;
	rgb[3].rgb.red = 0.; rgb[3].rgb.green = 0.; rgb[3].rgb.blue = 1.;

	for( i = 0; i <= 3; i++ ) {
		gset_colr_rep (WKID,i,&rgb[i]);
	}
	return;
}
