/*
 * $Id: c_caredg.c,v 1.1 1994-07-15 21:36:11 haley Exp $
 */

#include <stdio.h>
#include <math.h>

#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define WSTYPE SED_WSTYPE
#define WKID   1

#define M   40
#define N   40
#define max(x,y)    ((x) > (y) ? (x) : (y) )
#define min(x,y)    ((x) < (y) ? (x) : (y) )
#define pow2(x)    ((x)*(x))

main()
{
/*
 * Produce a Mercator projection of the Americas, using simplified 
 * continental outlines.  See the routine MAPEOD, below.
 * And draw a basic contour plot beside it.
 */

	float z[N][M], rwrk[2000], plim1[2],plim2[2],plim3[2],plim4[2];
	float vpl,vpr,vpb,vpt,wl,wr,wb,wt;
	int log;
	int iwrk[2000], ierr;
	extern void gendat();

	plim1[0] = -60.; plim1[1] = 0.;
	plim2[0] = -170.; plim2[1] = 0.;
	plim3[0] = 75.; plim3[1] = 0.;
	plim4[0] = -30.; plim4[1] = 0.;
/*
 * Get some data
 */
	gendat(z,M,M,N,1,25,1.0,25.);
/*
 *  Open GKS, open and activate a workstation.
 */
    gopen_gks ("stdout",0);
    gopen_ws (WKID, NULL, WSTYPE);
    gactivate_ws(WKID);
/*
 * Turn off the clipping indicator.
 */
	gset_clip_ind(GIND_NO_CLIP);
/*
 * Draw the map.
 */
	c_mappos (0.0,.53,0.53,1.);
	c_supmap (9,0.,0.,0.,plim1,plim2,plim3,plim4,2,0,2,0,&ierr);
	c_getset (&vpl,&vpr,&vpb,&vpt,&wl,&wr,&wb,&wt,&log);
	gsel_norm_tran(0);
	c_plchhq (.25,.50,"Geographic Map",.013,0.,0.);
/*
 * Draw contour plot
 */
	c_set (.53, 1.,vpb,vpt,1.,(float)M,1.,(float)N,log);
	c_cpsetr ("SET",0.);
	c_cpsetr ("LLP",0.);
	c_cprect (&z[0][0], M, M, N, rwrk, 2000, iwrk, 2000);
	c_perim (0,0,0,0);
	c_cpcldr (&z[0][0], rwrk, iwrk);
	gsel_norm_tran (0);
	c_plchhq (.75,.50,"Contour Map",.013,0.,0.);
/*
 * Draw Vertical Strips
 */
	c_set(0.,1.,0.,1.,0.,1.,0.,1.,1);
	c_line (.2,.05,.8,.05);
	c_line (.2,.45,.8,.45);
	c_line (.2,.05,.2,.45);
	c_line (.3,.05,.3,.45);
	c_line (.4,.05,.4,.45);
	c_line (.5,.05,.5,.45);
	c_line (.6,.05,.6,.45);
	c_line (.7,.05,.7,.45);
	c_line (.8,.05,.8,.45);
	gsel_norm_tran (0);
	c_plchhq (.50,.01,"Vertical Strips",.013,0.,0.);
/*
 * Deactivate and close workstation, close GKS.
 */
    gdeactivate_ws(WKID);
    gclose_ws(WKID);
    gclose_gks();
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
