/*
 * $Id: c_ccpga.c,v 1.2 1994-06-21 14:59:21 haley Exp $
 */

#include <stdio.h>
#include <math.h>

#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define   K   30
#define   N   30
#define   LRWK   1000
#define   LIWK   1000

#define WSTYPE SED_WSTYPE
#define WKID   1

main()
{
	float z[N][K], rwrk[LRWK];
	int i, m, iwrk[LIWK];
	extern void getdat();
	extern float mod();

	getdat (z, K, &m, N) ;
/*
 * Open GKS
 */
	gopen_gks("stdout",0);
	gopen_ws(WKID, NULL, WSTYPE);
	gactivate_ws(WKID);
/*
 * Turn clipping off
 */
	gset_clip_ind (GIND_NO_CLIP);
/*
 * Set X and Y min and max values
 */
	c_cpsetr ("XC1 - X COORDINATE AT INDEX 1",2.0);
	c_cpsetr ("XCM - X COORDINATE AT INDEX M",20.0);
	c_cpsetr ("YC1 - Y COORDINATE AT INDEX 1",0.0);
	c_cpsetr ("YCN - Y COORDINATE AT INDEX N",.01);
/*
 * Make viewport slightly smaller so that labels will fit
 */
	c_cpsetr ("VPL - VIEWPORT LEFT",0.10);
	c_cpsetr ("VPB - VIEWPORT BOTTOM",0.10);
/*
 * Initialize Conpack
 */
	c_cprect((float *)z,K,m,N,rwrk,LRWK,iwrk,LIWK);
/*
 * Draw and label perimeter
 */
	c_labmod("(E7.2)","(E7.2)",0,0,10,10,0,0,1);
	c_gridal(K-1,0,N-1,0,1,1,5,0.,0.);
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
