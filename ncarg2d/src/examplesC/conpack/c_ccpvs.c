/*
 * $Id: c_ccpvs.c,v 1.2 1994-06-21 14:59:35 haley Exp $
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

#define WSTYPE SED_WSTYPE
#define WKID   1

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
	gopen_gks ("stdout",0);
	gopen_ws (WKID, NULL, WSTYPE);
	gactivate_ws(WKID);
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
	gdeactivate_ws(WKID);
	gclose_ws(WKID);
	gclose_gks();
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
	Gcolr_rep rgb[44];
	int i;
/*
 *     BACKGROUND COLOR
 *     BLACK
 */
	rgb[0].rgb.red = 0.; rgb[0].rgb.green = 0.; rgb[0].rgb.blue = 0.;
/*
 *     FORGROUND COLORS
 * White
 */
	rgb[1].rgb.red = 1.0; rgb[1].rgb.green = 1.0; rgb[1].rgb.blue = 1.0;
/*
 * Aqua
 */
	rgb[2].rgb.red = 0.0; rgb[2].rgb.green = 0.9; rgb[2].rgb.blue = 1.0;
/*
 * Red1
 */
	rgb[3].rgb.red = 0.9; rgb[3].rgb.green = 0.25; rgb[3].rgb.blue = 0.0;
/*
 * OrangeRed1
 */
	rgb[4].rgb.red = 1.0; rgb[4].rgb.green = 0.0; rgb[4].rgb.blue = 0.2;
/*
 * Orange1
 */
	rgb[5].rgb.red = 1.0; rgb[5].rgb.green = 0.65; rgb[5].rgb.blue = 0.0;
/*
 * Yellow1
 */
	rgb[6].rgb.red = 1.0; rgb[6].rgb.green = 1.0; rgb[6].rgb.blue = 0.0;
/*
 * GreenYellow1
 */
	rgb[7].rgb.red = 0.7; rgb[7].rgb.green = 1.0; rgb[7].rgb.blue = 0.2;
/*
 * Chartreuse1
 */
	rgb[8].rgb.red = 0.5; rgb[8].rgb.green = 1.0; rgb[8].rgb.blue = 0.0;
/*
 * Celeste1
 */
	rgb[9].rgb.red = 0.2; rgb[9].rgb.green = 1.0; rgb[9].rgb.blue = 0.5;
/*
 * Green1
 */
	rgb[10].rgb.red = 0.2; rgb[10].rgb.green = 0.8; rgb[10].rgb.blue = 0.2;
/*
 * DeepSkyBlue1
 */
	rgb[11].rgb.red = 0.0; rgb[11].rgb.green = 0.75; rgb[11].rgb.blue = 1.0;
/*
 * RoyalBlue1
 */
	rgb[12].rgb.red = 0.25; rgb[12].rgb.green = 0.45; rgb[12].rgb.blue = 0.95;
/*
 * SlateBlue1
 */
	rgb[13].rgb.red = 0.4; rgb[13].rgb.green = 0.35; rgb[13].rgb.blue = 0.8;
/*
 * DarkViolet1
 */
	rgb[14].rgb.red = 0.6; rgb[14].rgb.green = 0.0; rgb[14].rgb.blue = 0.8;
/*
 * Orchid1
 */
	rgb[15].rgb.red = 0.85; rgb[15].rgb.green = 0.45; rgb[15].rgb.blue = 0.8;
/*
 * Red2
 */
	rgb[16].rgb.red = 0.77; rgb[16].rgb.green = 0.21; rgb[16].rgb.blue = 0.0;
/*
 * OrangeRed2
 */
	rgb[17].rgb.red = 0.85; rgb[17].rgb.green = 0.0; rgb[17].rgb.blue = 0.17;
/*
 * Orange2
 */
	rgb[18].rgb.red = 0.85; rgb[18].rgb.green = 0.55; rgb[18].rgb.blue = 0.0;
/*
 * Yellow2
 */
	rgb[19].rgb.red = 0.85; rgb[19].rgb.green = 0.85; rgb[19].rgb.blue = 0.0;
/*
 * GreenYellow2
 */
	rgb[20].rgb.red = 0.6; rgb[20].rgb.green = 0.85; rgb[20].rgb.blue = 0.17;
/*
 * Chartreuse2
 */
	rgb[21].rgb.red = 0.43; rgb[21].rgb.green = 0.85; rgb[21].rgb.blue = 0.0;
/*
 * Celeste2
 */
	rgb[22].rgb.red = 0.17; rgb[22].rgb.green = 0.85; rgb[22].rgb.blue = 0.43;
/*
 * Green2
 */
	rgb[23].rgb.red = 0.17; rgb[23].rgb.green = 0.68; rgb[23].rgb.blue = 0.17;
/*
 * DeepSkyBlue2
 */
	rgb[24].rgb.red = 0.0; rgb[24].rgb.green = 0.64; rgb[24].rgb.blue = 0.85;
/*
 * RoyalBlue2
 */
	rgb[25].rgb.red = 0.21; rgb[25].rgb.green = 0.38; rgb[25].rgb.blue = 0.81;
/*
 * SlateBlue2
 */
	rgb[26].rgb.red = 0.32; rgb[26].rgb.green = 0.3; rgb[26].rgb.blue = 0.68;
/*
 * DarkViolet2
 */
	rgb[27].rgb.red = 0.51; rgb[27].rgb.green = 0.0; rgb[27].rgb.blue = 0.68;
/*
 * Orchid2
 */
	rgb[28].rgb.red = 0.72; rgb[28].rgb.green = 0.38; rgb[28].rgb.blue = 0.68;
/*
 * Red3
 */
	rgb[29].rgb.red = 0.63; rgb[29].rgb.green = 0.18; rgb[29].rgb.blue = 0.0;
/*
 * OrangeRed3
 */
	rgb[30].rgb.red = 0.7; rgb[30].rgb.green = 0.0; rgb[30].rgb.blue = 0.14;
/*
 * Orange3
 */
	rgb[31].rgb.red = 0.6; rgb[31].rgb.green = 0.39; rgb[31].rgb.blue = 0.0;
/*
 * Yellow3
 */
	rgb[32].rgb.red = 0.7; rgb[32].rgb.green = 0.7; rgb[32].rgb.blue = 0.0;
/*
 * GreenYellow3
 */
	rgb[33].rgb.red = 0.49; rgb[33].rgb.green = 0.7; rgb[33].rgb.blue = 0.14;
/*
 * Chartreuse3
 */
	rgb[34].rgb.red = 0.35; rgb[34].rgb.green = 0.7; rgb[34].rgb.blue = 0.0;
/*
 * Celeste3
 */
	rgb[35].rgb.red = 0.14; rgb[35].rgb.green = 0.7; rgb[35].rgb.blue = 0.35;
/*
 * Green3
 */
	rgb[36].rgb.red = 0.14; rgb[36].rgb.green = 0.56; rgb[36].rgb.blue = 0.14;
/*
 * DeepSkyBlue3
 */
	rgb[37].rgb.red = 0.0; rgb[37].rgb.green = 0.53; rgb[37].rgb.blue = 0.7;
/*
 * RoyalBlue3
 */
	rgb[38].rgb.red = 0.18; rgb[38].rgb.green = 0.32; rgb[38].rgb.blue = 0.67;
/*
 * SlateBlue3
 */
	rgb[39].rgb.red = 0.28; rgb[39].rgb.green = 0.25; rgb[39].rgb.blue = 0.56;
/*
 * DarkViolet3
 */
	rgb[40].rgb.red = 0.42; rgb[40].rgb.green = 0.0; rgb[40].rgb.blue = 0.56;
/*
 * Orchid3
 */
	rgb[41].rgb.red = 0.6; rgb[41].rgb.green = 0.32; rgb[41].rgb.blue = 0.56;
/*
 * Lavender
 */
	rgb[42].rgb.red = 0.8; rgb[42].rgb.green = 0.8; rgb[42].rgb.blue = 1.0;
/*
 * Gray
 */
	rgb[43].rgb.red = 0.7; rgb[43].rgb.green = 0.7; rgb[43].rgb.blue = 0.7;
	for( i = 0; i <= 43; i++ ) {
		gset_colr_rep(WKID,i,&rgb[i]);
	}

    return;
}
