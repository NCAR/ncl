/*
 * $Id: c_ccpklb.c,v 1.2 1994-05-31 22:28:20 haley Exp $
 */

#include <stdio.h>

#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define K    40
#define N    40
#define LRWK    2000
#define LIWK    2000

main()
{
    float  z[N][K], rwrk[LRWK], clvl;
    int i, m, iwrk[LIWK], nocl;
    char string[17];
    extern void getdat();

    getdat (z, N, &m, K);
/*
 *  Open GKS
 */
    c_opngks();
/*
 * Initialize Conpack
 */
    c_cprect((float *)z,K,K,N,rwrk,LRWK,iwrk,LIWK);

    c_cppkcl((float *)z, rwrk, iwrk);
    c_cppklb((float *)z, rwrk, iwrk);
    c_cpgeti("NCL - number of CONTOUR LEVELS",&nocl);
    for( i = 1; i <= nocl; i++ ) {
        c_cpseti("PAI - PARAMETER ARRAY INDEX",i);
        c_cpgetr("CLV - CONTOUR LEVEL VALUE",&clvl);
        sprintf( string, " Contour at %5.3f", clvl );
        c_cpsetc("LLT - LINE LABEL TEXT",string);
    }
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
            z[l++] = 10.e-5*(-16.*(float)(i*i*j) + 34.*(float)(i*j*j) - (float)(6*i) + 93.);
        }
    }
    return;
}

