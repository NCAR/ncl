/*
 * $Id: c_ccpsps1.c,v 1.1 1994-05-13 14:25:43 haley Exp $
 */

#include <stdio.h>

#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define K    9
#define N    7
#define LRWK    1000
#define LIWK    1000
#define LZDT    2000

main()
{
    float z[N][K], zdat[LZDT], rwrk[LRWK];
    int j;
    int m, iwrk[LIWK];
    extern void getdat(), mark();

    getdat (z, N, &m, K);
/*
 * Open GKS
 */
    c_opngks();
/*
 * Initialize Conpack
 */
    c_cpsps1((float *)z,K,K,N,rwrk,LRWK,iwrk,LIWK,zdat,LZDT);
/*
 * Draw perimeter
 */
    c_cpback(zdat, rwrk, iwrk);
/*
 * Use a different line attribute on every line drawn
 * Draw Contours
 */
    c_cpcldr(zdat,rwrk,iwrk);
/*
 * Mark data points
 */
    mark (m, N);
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
    int i, j, l;

    l = 0;
    *m = k;
    for( j = 0; j < n; j++ ) {
        for( i = 0; i < *m; i++ ) {
            z[l++] = 10.e-8*(-16.*(float)(i*i*j) + 34.*(float)(i*j*j) - (float)(6*i) + 93.);
        }
    }
    return;
}

void mark (m, n)
int m, n;
{
    int i, j, idum5;
    float x[1], y[1];
    float dum1, dum2, dum3, dum4, xmin, xmax, ymin, ymax;
    int idum;

    c_getset(&dum1,&dum2,&dum3,&dum4,&xmin,&xmax,&ymin,&ymax,&idum5);
    gset_marker_size(.5);

    for( i = 1; i <= m; i++ ) {
        for( j = 1; j <= n; j++ ) {
            x[0] = (float)(i-1)*(xmax-xmin)/(float)(m-1)+xmin;
            y[0] = (float)(j-1)*(ymax-ymin)/(float)(n-1)+ymin;
            c_points (x, y, 1, -4, 0);
        }
    }
    return;
}

