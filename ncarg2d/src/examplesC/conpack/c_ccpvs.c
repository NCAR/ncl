/*
 * $Id: c_ccpvs.c,v 1.1 1994-05-31 22:28:25 haley Exp $
 */

#include <stdio.h>
#include <math.h>

#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define min(x,y)    ((x) < (y) ? (x) : (y))
#define max(x,y)    ((x) > (y) ? (x) : (y))
#define pow2(x)     ((x)*(x))

#define LMAP  100000
#define LWRK    1000

main()
{
/*
 * Declare required data arrays and workspace arrays.
 */
	float zdat[14][23],rwrk[LWRK],xcra[LWRK],ycra[LWRK];
	int iwrk[LWRK],iama[LMAP], iarea[2],igrp[2];
/*
 * Declare the routine which will color the areas.
 */
	extern void color();
	extern int fill();
	extern void gendat();
/*
 * Open GKS.
 */
	c_opngks();
/*
 * Turn off the clipping indicator.
 */
	gset_clip_ind (GIND_NO_CLIP);
/*
 * Force solid fill.
 */
	gset_fill_int_style (GSTYLE_SOLID);
/*
 * Define color indices.
 */
	color();
/*
 * Generate an array of test data.
 */
	gendat (zdat,23,23,14,20,20,-136.148,451.834);
/*
C Tell CONPACK to use 12 contour lines, splitting the range into 13
 * equal bands, one for each of the 13 colors available.
 */
	c_cpseti ("CLS - CONTOUR LEVEL SELECTOR",-12);
/*
 * Tell Conpack that we want 3 vertical strips
 */
	c_cpseti ("NVS - NUMBER OF VERTICAL STRIPS",3);
/*
 * Initialize the drawing of the contour plot.
 */
	c_cprect ((float *)zdat,23,23,14,rwrk,LWRK,iwrk,LWRK);
/*
 * Initialize the area map and put the contour lines into it.
 */
	c_arinam (iama,LMAP);
	c_cpclam ((float *)zdat,rwrk,iwrk,iama);
/*
 * Color the map.
 */
	c_arscam (iama,xcra,ycra,LWRK,iarea,igrp,2,fill);
/*
 * Put black contour lines over the colored map.
 */
	gset_line_colr_ind (0);
	c_cpcldr ((float *)zdat,rwrk,iwrk);
	gset_line_colr_ind (1);
/*
 * Advance the frame.
 */
	c_frame();
/*
 * Close GKS.
 */
	c_clsgks();
/*
 * Done.
 */
}


int fill(
    float *xcra,
    float *ycra,
    int *ncra,
    int *iarea,
    int *igrp,
    int *ngrps
)
{
	int i, ifill;
	float xavg, xsum;
	Gpoint_list fill_area;
/*
 * Get area identifiers for contour levels and vertical strips.
 */
	ifill=0;
	for( i = 0; i < *ngrps; i++ ) {
		if (igrp[i] == 3) ifill=iarea[i];
	}
/*
 * Find the average X value of the X coordinates defining the area
 */
	xsum=0.;
	for( i = 0; i < *ncra; i++ ) {
		xsum=xsum+xcra[i];
	}
	xavg=xsum/(float)*ncra;
/*
 * Fill vertical strip 1.
 *
 *
 * Create structure to pass to gfill_area
 */
	fill_area.num_points = *ncra-1;
	fill_area.points = (Gpoint *) malloc(fill_area.num_points*sizeof(Gpoint));
	if( !fill_area.points ) {
		fprintf( stderr, "fill: Not enough memory to create fill area structure\n" );
		gemergency_close_gks();
		exit(1);
	}
	for( i = 0; i < *ncra-1; i++ ) {
		fill_area.points[i].x = xcra[i];
		fill_area.points[i].y = ycra[i];
	}
	if (ifill > 0 && xavg < 0.35) {
		gset_fill_colr_ind(ifill+2);
		gfill_area(&fill_area);
/*
 * Fill vertical strip 2
 */
	}
	else if (ifill > 0 & xavg >= 0.35 & xavg <= 0.65) {
		gset_fill_colr_ind(ifill+15);
		gfill_area(&fill_area);
	}
/*
 * Fill vertical strip 3
 */
	else if (ifill > 0 & xavg > 0.65) {
		gset_fill_colr_ind(ifill+28);
		gfill_area(&fill_area);
	}
	free((Gpoint *)fill_area.points);
/*
 * Done.
 */
	return(0);
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

void color()
{
	Gcolr_rep rgb;
/*
 *     BACKGROUND COLOR
 *     BLACK
 */
	rgb.rgb.red = 0.; rgb.rgb.green = 0.; rgb.rgb.blue = 0.;
	gset_colr_rep(1,0,&rgb);
/*
 *     FORGROUND COLORS
 * White
 */
	rgb.rgb.red = 1.0; rgb.rgb.green =  1.0; rgb.rgb.blue =  1.0;
	gset_colr_rep(1, 1,&rgb);
/*
 * Aqua
 */
	rgb.rgb.red = 0.0; rgb.rgb.green =  0.9; rgb.rgb.blue =  1.0;
	gset_colr_rep(1, 2,&rgb);
/*
 * Red1
 */
	rgb.rgb.red = 0.9; rgb.rgb.green =  0.25; rgb.rgb.blue =  0.0;
	gset_colr_rep(1, 3,&rgb);
/*
 * OrangeRed1
 */
	rgb.rgb.red = 1.0; rgb.rgb.green =  0.0; rgb.rgb.blue =  0.2;
	gset_colr_rep(1, 4,&rgb);
/*
 * Orange1
 */
	rgb.rgb.red = 1.0; rgb.rgb.green =  0.65; rgb.rgb.blue =  0.0;
	gset_colr_rep(1, 5,&rgb);
/*
 * Yellow1
 */
	rgb.rgb.red = 1.0; rgb.rgb.green =  1.0; rgb.rgb.blue =  0.0;
	gset_colr_rep(1, 6,&rgb);
/*
 * GreenYellow1
 */
	rgb.rgb.red = 0.7; rgb.rgb.green =  1.0; rgb.rgb.blue =  0.2;
	gset_colr_rep(1, 7,&rgb);
/*
 * Chartreuse1
 */
	rgb.rgb.red = 0.5; rgb.rgb.green =  1.0; rgb.rgb.blue =  0.0;
	gset_colr_rep(1, 8,&rgb);
/*
 * Celeste1
 */
	rgb.rgb.red = 0.2; rgb.rgb.green =  1.0; rgb.rgb.blue =  0.5;
	gset_colr_rep(1, 9,&rgb);
/*
 * Green1
 */
	rgb.rgb.red = 0.2; rgb.rgb.green =  0.8; rgb.rgb.blue =  0.2;
	gset_colr_rep(1, 10,&rgb);
/*
 * DeepSkyBlue1
 */
	rgb.rgb.red = 0.0; rgb.rgb.green =  0.75; rgb.rgb.blue =  1.0;
	gset_colr_rep(1, 11,&rgb);
/*
 * RoyalBlue1
 */
	rgb.rgb.red = 0.25; rgb.rgb.green =  0.45; rgb.rgb.blue =  0.95;
	gset_colr_rep(1, 12,&rgb);
/*
 * SlateBlue1
 */
	rgb.rgb.red = 0.4; rgb.rgb.green =  0.35; rgb.rgb.blue =  0.8;
	gset_colr_rep(1, 13,&rgb);
/*
 * DarkViolet1
 */
	rgb.rgb.red = 0.6; rgb.rgb.green =  0.0; rgb.rgb.blue =  0.8;
	gset_colr_rep(1, 14,&rgb);
/*
 * Orchid1
 */
	rgb.rgb.red = 0.85; rgb.rgb.green =  0.45; rgb.rgb.blue =  0.8;
	gset_colr_rep(1, 15,&rgb);
/*
 * Red2
 */
	rgb.rgb.red = 0.77; rgb.rgb.green =  0.21; rgb.rgb.blue =  0.0;
	gset_colr_rep(1, 16,&rgb);
/*
 * OrangeRed2
 */
	rgb.rgb.red = 0.85; rgb.rgb.green =  0.0; rgb.rgb.blue =  0.17;
	gset_colr_rep(1, 17,&rgb);
/*
 * Orange2
 */
	rgb.rgb.red = 0.85; rgb.rgb.green =  0.55; rgb.rgb.blue =  0.0;
	gset_colr_rep(1, 18,&rgb);
/*
 * Yellow2
 */
	rgb.rgb.red = 0.85; rgb.rgb.green =  0.85; rgb.rgb.blue =  0.0;
	gset_colr_rep(1, 19,&rgb);
/*
 * GreenYellow2
 */
	rgb.rgb.red = 0.6; rgb.rgb.green =  0.85; rgb.rgb.blue =  0.17;
	gset_colr_rep(1, 20,&rgb);
/*
 * Chartreuse2
 */
	rgb.rgb.red = 0.43; rgb.rgb.green =  0.85; rgb.rgb.blue =  0.0;
	gset_colr_rep(1, 21,&rgb);
/*
 * Celeste2
 */
	rgb.rgb.red = 0.17; rgb.rgb.green =  0.85; rgb.rgb.blue =  0.43;
	gset_colr_rep(1, 22,&rgb);
/*
 * Green2
 */
	rgb.rgb.red = 0.17; rgb.rgb.green =  0.68; rgb.rgb.blue =  0.17;
	gset_colr_rep(1, 23,&rgb);
/*
 * DeepSkyBlue2
 */
	rgb.rgb.red = 0.0; rgb.rgb.green =  0.64; rgb.rgb.blue =  0.85;
	gset_colr_rep(1, 24,&rgb);
/*
 * RoyalBlue2
 */
	rgb.rgb.red = 0.21; rgb.rgb.green =  0.38; rgb.rgb.blue =  0.81;
	gset_colr_rep(1, 25,&rgb);
/*
 * SlateBlue2
 */
	rgb.rgb.red = 0.32; rgb.rgb.green =  0.3; rgb.rgb.blue =  0.68;
	gset_colr_rep(1, 26,&rgb);
/*
 * DarkViolet2
 */
	rgb.rgb.red = 0.51; rgb.rgb.green =  0.0; rgb.rgb.blue =  0.68;
	gset_colr_rep(1, 27,&rgb);
/*
 * Orchid2
 */
	rgb.rgb.red = 0.72; rgb.rgb.green =  0.38; rgb.rgb.blue =  0.68;
	gset_colr_rep(1, 28,&rgb);
/*
 * Red3
 */
	rgb.rgb.red = 0.63; rgb.rgb.green =  0.18; rgb.rgb.blue =  0.0;
	gset_colr_rep(1, 29,&rgb);
/*
 * OrangeRed3
 */
	rgb.rgb.red = 0.7; rgb.rgb.green =  0.0; rgb.rgb.blue =  0.14;
	gset_colr_rep(1, 30,&rgb);
/*
 * Orange3
 */
	rgb.rgb.red = 0.6; rgb.rgb.green =  0.39; rgb.rgb.blue =  0.0;
	gset_colr_rep(1, 31,&rgb);
/*
 * Yellow3
 */
	rgb.rgb.red = 0.7; rgb.rgb.green =  0.7; rgb.rgb.blue =  0.0;
	gset_colr_rep(1, 32,&rgb);
/*
 * GreenYellow3
 */
	rgb.rgb.red = 0.49; rgb.rgb.green =  0.7; rgb.rgb.blue =  0.14;
	gset_colr_rep(1, 33,&rgb);
/*
 * Chartreuse3
 */
	rgb.rgb.red = 0.35; rgb.rgb.green =  0.7; rgb.rgb.blue =  0.0;
	gset_colr_rep(1, 34,&rgb);
/*
 * Celeste3
 */
	rgb.rgb.red = 0.14; rgb.rgb.green =  0.7; rgb.rgb.blue =  0.35;
	gset_colr_rep(1, 35,&rgb);
/*
 * Green3
 */
	rgb.rgb.red = 0.14; rgb.rgb.green =  0.56; rgb.rgb.blue =  0.14;
	gset_colr_rep(1, 36,&rgb);
/*
 * DeepSkyBlue3
 */
	rgb.rgb.red = 0.0; rgb.rgb.green =  0.53; rgb.rgb.blue =  0.7;
	gset_colr_rep(1, 37,&rgb);
/*
 * RoyalBlue3
 */
	rgb.rgb.red = 0.18; rgb.rgb.green =  0.32; rgb.rgb.blue =  0.67;
	gset_colr_rep(1, 38,&rgb);
/*
 * SlateBlue3
 */
	rgb.rgb.red = 0.28; rgb.rgb.green =  0.25; rgb.rgb.blue =  0.56;
	gset_colr_rep(1, 39,&rgb);
/*
 * DarkViolet3
 */
	rgb.rgb.red = 0.42; rgb.rgb.green =  0.0; rgb.rgb.blue =  0.56;
	gset_colr_rep(1, 40,&rgb);
/*
 * Orchid3
 */
	rgb.rgb.red = 0.6; rgb.rgb.green =  0.32; rgb.rgb.blue =  0.56;
	gset_colr_rep(1, 41,&rgb);
/*
 * Lavender
 */
	rgb.rgb.red = 0.8; rgb.rgb.green =  0.8; rgb.rgb.blue =  1.0;
	gset_colr_rep(1, 42,&rgb);
/*
 * Gray
 */
	rgb.rgb.red = 0.7; rgb.rgb.green =  0.7; rgb.rgb.blue =  0.7;
	gset_colr_rep(1, 43,&rgb);
/*
 * Done.
 */
    return;
}
