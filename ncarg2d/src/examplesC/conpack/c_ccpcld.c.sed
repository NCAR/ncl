/*
 * $Id: c_ccpcld.c.sed,v 1.2 1994-08-23 22:50:00 haley Exp $
 */

#include <stdio.h>
#include <math.h>

#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define   K   40
#define   N   40
#define   LRWK   1000
#define   LIWK   1000


int ibts[7][16] = { 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 
               1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0,
               1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0,
               1, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 0,
               1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0,
               1, 1, 1, 1, 0, 1, 1, 1, 0, 0, 1, 1, 0, 0, 0, 1,
               1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1};

#define WSTYPE SED_WSTYPE
#define WKID   1

main()
{
	float z[N][K], rwrk[LRWK];
	int i, nocl, iwrk[LIWK];
	extern void getdat();
	extern int ipat();

	getdat (z) ;
/*
 * Open GKS
 */
	gopen_gks ("stdout",0);
	gopen_ws (WKID, NULL, WSTYPE);
	gactivate_ws(WKID);
/*
 * Choose the number of contour levels
 */
	c_cpseti("CLS - CONTOUR LEVEL SELECTION FLAG",-7);
/*
 * Call conpack normally
 */
	c_cprect((float *)z,K,N,N,rwrk,LRWK,iwrk,LIWK);
	c_cppkcl((float *)z, rwrk, iwrk);
/*
 * Set a different dash pattern for each contour line
 */
	c_cpgeti("NCL - NUMBER OF CONTOUR LEVELS",&nocl);
	for( i = 0; i < nocl; i++ ) {
		c_cpseti("PAI - PARAMETER ARRAY INDEX",i+1);
		c_cpseti("CLD - CONTOUR LINE DASH PATTERN",ipat(&ibts[i][0]));
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

void getdat (z)
float *z;
{
    int i,j,l;

    l = 0;
    for( j = 1; j <= K; j++ ) {
        for( i = 1; i <= N; i++ ) {
            z[l++] = 10.e-5*(-16.0*(float)(i*i*j) + 34.0*(float)(i*j*j) - (float)(6.0*i) + 93.0);
        }
    }
    return;
}

int ipat (ibt)
int *ibt;
{
	int i, j, k, one = 1;
	extern int ishift_(int*,int*);
	extern int ior_(int*,int*);
	
	j=0;
	for( i = 0; i < 16; i++ )  { 
		k = ishift_(&j,&one);
		j = ior_(&k,&ibt[i]);
	}
	return(j);
}
