/*
 * $Id: c_cmpita.c,v 1.1 1994-05-24 22:42:17 haley Exp $
 */

#include <stdio.h>
#include <math.h>

#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define IGRD     15
#define IMAP     220000
#define NWRK     15000
#define M     180/IGRD
#define N     360/IGRD

#define min(x,y)   ((x) < (y) ? (x) : (y))
#define max(x,y)   ((x) > (y) ? (x) : (y))
#define pow2(x)    ((x)*(x))

float zdat[N][M];

main()
{
	int i, j, log;
	float vpl,vpr,vpb,vpt,wl,wr,wb,wt;
	float left;
/*
 * Declare fill routine external, or crash program
 */
	extern int fill();
	extern void gendat(), color();

	int map[IMAP], igrp[5], iarea[5], ispace;
	float xwrk[NWRK],ywrk[NWRK];
	float plim1[2], plim2[2], plim3[2], plim4[2];

	plim1[0] = plim2[0] = plim3[0] = plim4[0] = 0.;
	plim1[1] = plim2[1] = plim3[1] = plim4[1] = 0.;
/*
 * Print out a warning about how time consuming this example is
 */
	printf( "WARNING: This example may take 20 minutes\n" );
	printf( "          to execute on some machines.\n" );
/*
 * Generate some data to base color fill on
 */
	gendat(25,25,1.,15.);
/*
 * Make sure that data at -180 is the same as data at +180
 */
	for( i = 0; i < M; i++ ) {
		zdat[N-1][i] = zdat [0][i];
	}
/*
 * Open GKS.
 */
	c_opngks();
/*
 * Set up color table
 */
	color();
/*
 * Set the outline-dataset parameter.
 */
	c_mapstc ("OU - OUTLINE DATASET SELECTOR","PO");
/*
 * Set the projection-type parameters.
 */
	c_maproj ("CE",0.,0.,0.);
/*
 * Set the limits parameters.
 */
	c_mapset ("MA",plim1,plim2,plim3,plim4);
/*
 * Initalize areas, initialize ezmap
 */
	c_arinam (map, IMAP);
	c_mapint();
/*
 * Add geographic outlines to area map
 */
	c_mapbla (map);
/*
 * Add longitude lines at 2 degree intervals over the states to area map
 */
	for( i = -90; i <= 90; i+=IGRD ) {
		for( j = -180; j <= 180-IGRD; j+=IGRD ) {
            left  = (j+181)*1000+(i+91);
            c_mapita((float)i,(float)j,0,map,5,left,0);
            c_mapita((float)i,(float)(j+IGRD),1,map,5,left,0);
            c_mapiqa(map,5,left,0);
		}
	}
/*
 * Add latitude lines at 2 degree intervals over the states to area map
 */
	for( i = -180; i <= 180; i+=IGRD ) {
		for( j = -90; j <= 90-IGRD; j+=IGRD ) {
            c_mapita((float)j,(float)i,0,map,5,0,0);
            c_mapita((float)(j+IGRD),(float)i,1,map,5,0,0);
            c_mapiqa(map,5,0,0);
		}
	}
/*
 * Fill in areas over land with colors
 */
	gset_fill_int_style(GSTYLE_SOLID);
	c_arscam(map,xwrk,ywrk,NWRK,iarea,igrp,5,fill);
/*
 * Draw perimeter
 */
	c_mapsti("LA - LABEL FLAG",0);
	c_maplbl();
/*
 * Draw map over area plot
 */
	c_maplot();
/*
 * Report how much space was used in the area map
 */
	ispace = map[0] - map[5] + map[4];
	printf( "Area Map Workspace Used: %d\n",ispace);
/*
 * Put the label at the top of the plot.
 */
	c_getset (&vpl,&vpr,&vpb,&vpt,&wl,&wr,&wb,&wt,&log);
	c_set (0.,1.,0.,1.,0.,1.,0.,1.,1);
	gset_linewidth(2.);
	c_plchhq (.5,vpt+.02,"Filling Gridded Data over Landmasses",.017,0.,0.);
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

int fill (xwrk,ywrk,nwrk,iarea,igrp,nsize)
float *xwrk, *ywrk;
int *iarea, *igrp;
int *nwrk, *nsize;
{
    int i, j, iclr;
    int iarea1, iarea5;
    Gpoint_list fill_area;
	float lat, lon;
/*
 * Group 5 is the group of 2 degree grid lines, group 1 are political and
 * continental outlines.
 */
	iarea1 = -1;
	iarea5 = -1;
/*
 * If there are less than 3 points defining the area, return to arscam
 */
    if(*nwrk <= 3) return(0);
/*
 * Check each of the group and area identifiers for the current area
 */
    for( i = 0; i < *nsize; i++ ) {
        if (igrp[i] == 1) iarea1 = iarea[i];
        if (igrp[i] == 5) iarea5 = iarea[i];
    }
/*
 * If the area identifier is over the globe
 */
    if( iarea1 > 0 ) {
/*
 * If the color id for the area is 1, then the area is over ocean and
 * don't fill area
 */
        if (c_mapaci(iarea1) != 1 && iarea5 > 0) {
/*
 * At this point you need to invert your area identifier function to 
 * retrieve your latitude and longitude values (or your data array
 * indicies) so that you can color fill based on them.
 */
            lat = iarea5 % 1000;
            i = lat/IGRD;
            lon = iarea5/1000;
            j = lon/IGRD;
/*
 * Our data is predefined to have values between 1. and 15 (chosen
 * because we have 15 colors defined in subroutine COLOR.
 * color index 1 is white, so we set the color index to start at 2.
 */
            iclr = (int)zdat[j][i]+1;
            gset_fill_colr_ind(iclr);
/*
 * set up struct for fill area
 */
			fill_area.num_points = *nwrk-1;
			fill_area.points = (Gpoint *)malloc(fill_area.num_points*sizeof(Gpoint));
			if( !fill_area.points ) {
				fprintf( stderr, "fill:  Not enough memory to create fill area array\n" );
				gemergency_close_gks();
				exit(1);
			}
			for( i = 0; i < *nwrk-1; i++ ) {
				fill_area.points[i].x = xwrk[i];
				fill_area.points[i].y = ywrk[i];
			}
			gfill_area(&fill_area);
			free(fill_area.points);
		}
	}
	return(0);
}

void gendat (mlow,mhgh,dlow,dhgh)
int mlow, mhgh;
float dlow, dhgh;
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
	int i, j, k, nlow, nhgh, ncnt, m, n;
	float fovm, fovn, dmin, dmax, temp;
	float ccnt[3][50];
	extern float fran();
	
	m = M;
	n = N;
	fovm = 9./(float)m;
	fovn = 9./(float)n;

	nlow = max(1,min(25,mlow));
	nhgh =  max(1,min(25,mhgh));
	ncnt = nlow+nhgh;

	for( k = 0; k < ncnt; k++ ) {
		ccnt[0][k] = 1.+((float)m-1.)*fran();
		ccnt[1][k] = 1.+((float)n-1.)*fran();
		if (k <= nlow) {
            ccnt[2][k] = -1.;
		}
		else {
            ccnt[2][k] = 1.;
		}
	}

	dmin = 1.36;
	dmax = -1.e36;
	for( j = 0; j < N; j++ ) {
		for( i = 0; i < M; i++ ) {
            zdat[j][i]=.5*(dlow+dhgh);
            for( k = 0; k < ncnt; k++ ) {
				temp = -(pow2((fovm*((float)(i+1)-ccnt[0][k])))+
                         pow2(fovn*((float)(j+1)-ccnt[1][k])));
				if (temp >= -20.) zdat[j][i] = zdat[j][i]+.5*(dhgh-dlow)
                                               *ccnt[2][k]*exp(temp);
			}
            dmin = min(dmin,zdat[j][i]);
            dmax = max(dmax,zdat[j][i]);
		}
	}

	for( j = 0; j < N; j++ ) {
		for( i = 0; i < M; i++ ) {
            zdat[j][i] = (zdat[j][i]-dmin)/(dmax-dmin)*(dhgh-dlow)+dlow;
		}
	}

	return;
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
/*
 *     BACKGROUND COLOR
 *     BLACK
 *     CALL GSCR(1,0,0.,0.,0.)
 *
 *     FORGROUND COLORS
 * White
 */
	Gcolr_rep rgb;

	rgb.rgb.red =  1.0; rgb.rgb.green =  1.0; rgb.rgb.blue =  1.0;
	gset_colr_rep(1,  1, &rgb);
/*
 * Red
 */
	rgb.rgb.red =  0.9; rgb.rgb.green =  0.25; rgb.rgb.blue =  0.0;
	gset_colr_rep(1,  2, &rgb);
/*
 * OrangeRed
 */
	rgb.rgb.red =  1.0; rgb.rgb.green =  0.0; rgb.rgb.blue =  0.2;
	gset_colr_rep(1,  3, &rgb);
/*
 * Orange
 */
	rgb.rgb.red =  1.0; rgb.rgb.green =  0.65; rgb.rgb.blue =  0.0;
	gset_colr_rep(1,  4, &rgb);
/*
 * Yellow
 */
	rgb.rgb.red =  1.0; rgb.rgb.green =  1.0; rgb.rgb.blue =  0.0;
	gset_colr_rep(1,  5, &rgb);
/*
 * GreenYellow
 */
	rgb.rgb.red =  0.7; rgb.rgb.green =  1.0; rgb.rgb.blue =  0.2;
	gset_colr_rep(1,  6, &rgb);
/*
 * Chartreuse
 */
	rgb.rgb.red =  0.5; rgb.rgb.green =  1.0; rgb.rgb.blue =  0.0;
	gset_colr_rep(1,  7, &rgb);
/*
 * Celeste
 */
	rgb.rgb.red =  0.2; rgb.rgb.green =  1.0; rgb.rgb.blue =  0.5;
	gset_colr_rep(1,  8, &rgb);
/*
 * Green
 */
	rgb.rgb.red =  0.2; rgb.rgb.green =  0.8; rgb.rgb.blue =  0.2;
	gset_colr_rep(1,  9, &rgb);
/*
 * Aqua
 */
	rgb.rgb.red =  0.0; rgb.rgb.green =  0.9; rgb.rgb.blue =  1.0;
	gset_colr_rep(1, 10, &rgb);
/*
 * DeepSkyBlue
 */
	rgb.rgb.red =  0.0; rgb.rgb.green =  0.75; rgb.rgb.blue =  1.0;
	gset_colr_rep(1, 11, &rgb);
/*
 * RoyalBlue
 */
	rgb.rgb.red =  0.25; rgb.rgb.green =  0.45; rgb.rgb.blue =  0.95;
	gset_colr_rep(1, 12, &rgb);
/*
 * SlateBlue
 */
	rgb.rgb.red =  0.4; rgb.rgb.green =  0.35; rgb.rgb.blue =  0.8;
	gset_colr_rep(1, 13, &rgb);
/*
 * DarkViolet
 */
	rgb.rgb.red =  0.6; rgb.rgb.green =  0.0; rgb.rgb.blue =  0.8;
	gset_colr_rep(1, 14, &rgb);
/*
 * Orchid
 */
	rgb.rgb.red =  0.85; rgb.rgb.green =  0.45; rgb.rgb.blue =  0.8;
	gset_colr_rep(1, 15, &rgb);
/*
 * Lavender
 */
	rgb.rgb.red =  0.8; rgb.rgb.green =  0.8; rgb.rgb.blue =  1.0;
	gset_colr_rep(1, 16, &rgb);
/*
 * Gray
 */
	rgb.rgb.red =  0.7; rgb.rgb.green =  0.7; rgb.rgb.blue =  0.7;
	gset_colr_rep(1, 17, &rgb);
/*
 * Done.
 */
	return;
}
