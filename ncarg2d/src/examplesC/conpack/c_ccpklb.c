/*
 * $Id: c_ccpklb.c,v 1.4 1994-08-23 22:50:10 haley Exp $
 */

#include <stdio.h>

#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define K    40
#define N    40
#define LRWK    2000
#define LIWK    2000

#define WSTYPE SED_WSTYPE
#define WKID   1

float  z[N][K], rwrk[LRWK];
int iwrk[LIWK];

main()
{
    float  clvl;
    int i, m, nocl;
    char string[17];
    extern void getdat();

    getdat ();
/*
 *  Open GKS
 */
    gopen_gks ("stdout",0);
    gopen_ws (WKID, NULL, WSTYPE);
    gactivate_ws(WKID);
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
    gdeactivate_ws(WKID);
    gclose_ws(WKID);
    gclose_gks();
}

void getdat ()
{
    int i, j;

    for( i = 1; i <= K; i++ ) {
        for( j = 1; j <= N; j++ ) {
            z[j-1][i-1] = 10.e-8*(-16.*(float)(i*i*j)+34.*(float)(i*j*j)-(float)(6*i)+93.);
        }
    }
    return;
}

