/*
 * $Id: c_ccphcf.c,v 1.1 1994-06-08 14:44:39 haley Exp $
 */

/*
 * This program demonstrates the use of hachures
 */

#include <stdio.h>
#include <math.h>

#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define M  20
#define N  30
#define LRWK  3500
#define LIWK  4000

#define min(x,y)    ((x) < (y) ? (x) : (y))
#define max(x,y)    ((x) > (y) ? (x) : (y))
#define pow2(x)     ((x)*(x))

main()
{
	float zreg[N][M], rwrk[LRWK];
	int iwrk[LIWK];
	extern void gendat();
/*
 * Create some data
 */
	gendat(zreg,M,M,N,5,15,5.,15.);
/*
 * Open GKS
 */
	c_opngks();
/*
 * Turn on hachures and demonstrate changing hachure spacing and length
 */
	c_cpseti("HCF - HACHURING FLAG",2);
	c_cpsetr("HCS - HACHURE SPACING",.05);
	c_cpsetr("HCL - HACHURE LENGTH",.01);
/*
 * Initialize Conpack
 */
	c_cprect((float *)zreg,M,M,N,rwrk,LRWK,iwrk,LIWK);
/*
 * Draw perimiter
 */
	c_cpback((float *)zreg, rwrk, iwrk);
/*
 * Draw Contours
 */
	c_cpcldr((float *)zreg,rwrk,iwrk);
/*
 * Close frame and close GKS
 */
	c_frame();
	c_clsgks();
}

void gendat (data,idim,m,n,mlow,mhgh,dlow,dhgh)
float *data, dlow, dhgh;
int idim, m, n, mlow, mhgh;
{
/*
 * This is a routine to generate test data for two-dimensional graphics
 * routines.  Given an array "DATA", dimensioned "IDIM x 1", it fills
 * the sub-array ((DATA(I,J),I=1,M),J=1,N) with a two-dimensional field
 * of data having approximately "MLOW" lows and "MHGH" highs, a minimum
 * value of exactly "DLOW" and a maximum value of exactly "DHGH".
 *
 * "MLOW" and "MHGH" are each forced to be greater than or equal to 1
 * and less than or equal to 25.
 *
 * The function used is a sum of exponentials.
 */
    float ccnt[3][50], fovm, fovn, dmin, dmax, temp;
    extern float fran();
    int nlow, nhgh, ncnt, i, j, k, ii;

    fovm=9./(float)m;
    fovn=9./(float)n;

    nlow=max(1,min(25,mlow));
    nhgh=max(1,min(25,mhgh));
    ncnt=nlow+nhgh;

    for( k=1; k <= ncnt; k++ ) {
        ccnt[0][k-1]=1.+((float)m-1.)*fran();
        ccnt[1][k-1]=1.+((float)n-1.)*fran();
        if (k <= nlow) {
            ccnt[2][k-1]= -1.;
        }
        else {
            ccnt[2][k-1] = 1.;
        }
    }

    dmin =  1.e36;
    dmax = -1.e36;
    ii = 0;
    for( j = 1; j <= n; j++ ) {
        for( i = 1; i <= m; i++ ) {
            data[ii]=.5*(dlow+dhgh);
            for( k = 1; k <= ncnt; k++ ) {
                temp = -(pow2((fovm*((float)(i)-ccnt[0][k-1])))+
                         pow2(fovn*((float)(j)-ccnt[1][k-1])));
                if (temp >= -20.) data[ii]=data[ii]+.5*(dhgh-dlow)
                                           *ccnt[2][k-1]*exp(temp);
            }
            dmin=min(dmin,data[ii]);
            dmax=max(dmax,data[ii]);
            ii++;
        }
    }

    for( j = 0; j < m*n; j++ ) {
        data[j]=(data[j]-dmin)/(dmax-dmin)*(dhgh-dlow)+dlow;
    }
}

float rseq[] = { .749, .973, .666, .804, .081, .483, .919, .903, .951, .960,
   .039, .269, .270, .756, .222, .478, .621, .063, .550, .798, .027, .569,
   .149, .697, .451, .738, .508, .041, .266, .249, .019, .191, .266, .625,
   .492, .940, .508, .406, .972, .311, .757, .378, .299, .536, .619, .844,
   .342, .295, .447, .499, .688, .193, .225, .520, .954, .749, .997, .693,
   .217, .273, .961, .948, .902, .104, .495, .257, .524, .100, .492, .347,
   .981, .019, .225, .806, .678, .710, .235, .600, .994, .758, .682, .373,
   .009, .469, .203, .730, .588, .603, .213, .495, .884, .032, .185, .127,
   .010, .180, .689, .354, .372, .429 };

float fran()
{
    static int iseq = 0;
    iseq = (iseq % 100) + 1;
    return(rseq[iseq-1]);
}
