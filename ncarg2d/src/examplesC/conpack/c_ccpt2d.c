#include <stdio.h>
#include <math.h>
#include <ncarg/gks.h>
#include <ncarg/ncargC.h>

/*
**  Define error file, Fortran unit number, and workstation type,
**  and workstation ID.
*/
#define IERRF  "stdout"
#define LUNIT  "gmeta"
#define IWTYPE SED_WSTYPE
#define IWKID  1
#define ISZDM  0

#define LMAP 100000	/* contour map size */
#define LWRK 1000	/* size of real work array */
#define M 40		/* rows of data */
#define N 40 		/* columns of data */


int main()
{
	int i,j;	/* counters */

	float VPL,VPR,VPB,VPT,WL,WR,WB,WT;	/* c_arscam variables */
	int LOG;

	FILE* fp;	/* data file pointer */

			/*  Area processing routine called by c_arscam */
	extern int fill(float*, float*, int*, int*, int*, int*);

			/*  Define a color table */
	extern void color();

/*
**  Declare required data arrays and workspace arrays.
*/
        float ZDAT[M][N];
	float RWRK[LWRK];
	float XCRA[LWRK];
	float YCRA[LWRK];

        int IWRK[LWRK];
	int IAMA[LMAP];
	int IAREA[2];
	int IGRP[2];


/*
**  Open GKS.
*/
        gopen_gks(IERRF, ISZDM);
        gopen_ws(IWKID, LUNIT, IWTYPE);
        gactivate_ws(IWKID);

/*
**  Turn off the clipping indicator.
*/
        gset_clip_ind (0);
/*
**  Force solid fill.
*/
        gset_fill_int_style (1);
/*
**  Define color indices.
*/
        color();
/*
**  Retrieve an array of test data.
*/
	fp = fopen("ccpila.dat","r");
	for(i=0; i<M; ++i)
	{
		for(j=0; j<N; ++j)
		{
			fscanf(fp,"%f",&ZDAT[i][j]);
		}
	}
/*
** Tell CONPACK to use 12 contour levels, splitting the range into 13
** equal bands, one for each of the 13 colors available.
*/
        c_cpseti ("CLS - CONTOUR LEVEL SELECTION FLAG",-12);
/*
** Draw smoothed plot to the right
*/
	c_cpsetr ("VPL - VIEWPORT LEFT",.51);
	c_cpsetr ("VPR - VIEWPORT RIGHT",1.0);
/*
** Set smoothing so that lines are very smooth
*/
        c_cpsetr ("T2D - TENSION ON 2D SPLINES",.0001);
/*
** Initialize the drawing of the contour plot.
*/
        c_cprect (&ZDAT[0][0],M,M,N,RWRK,LWRK,IWRK,LWRK);
/*
** Initialize the area map and put the contour lines into it.
*/
        c_arinam (IAMA,LMAP);
        c_cpclam (&ZDAT[0][0],RWRK,IWRK,IAMA);
/*
** Color the map.
*/
        c_arscam (IAMA,XCRA,YCRA,LWRK,IAREA,IGRP,2,fill);
/*
** Put black contour lines over the colored map.
*/
        gset_line_colr_ind (0);
        c_cpcldr (&ZDAT[0][0],RWRK,IWRK);
/*
** Draw unsmoothed plot to the left
*/
	c_cpsetr ("VPL - VIEWPORT LEFT",0.0);
	c_cpsetr ("VPR - VIEWPORT RIGHT",0.49);
/* 
** Tell Conpack that we want no smoothing
*/
        c_cpsetr ("T2D - TENSION ON 2D SPLINES",0.);
/*
** Initialize the drawing of the contour plot.
*/
        c_cprect (&ZDAT[0][0],M,M,N,RWRK,LWRK,IWRK,LWRK);
/*
** Initialize the area map and put the contour lines into it.
*/
        c_arinam (IAMA,LMAP);
        c_cpclam (&ZDAT[0][0],RWRK,IWRK,IAMA);
/*
** Color the map.
*/
        c_arscam (IAMA,XCRA,YCRA,LWRK,IAREA,IGRP,2,fill);
/*
** Put black contour lines over the colored map.
*/
        gset_line_colr_ind (0);
        c_cpcldr (&ZDAT[0][0],RWRK,IWRK);
/*
** Draw titles in white
*/
	c_getset(&VPL,&VPR,&VPB,&VPT,&WL,&WR,&WB,&WT,&LOG);
	c_set(0.,1.,0.,1.,0.,1.,0.,1.,1);
        gset_line_colr_ind (0);
	c_pchiqu (.3,VPT+.015,"Unsmoothed Contours",.015,0.,0.);
	c_pchiqu (.75,VPT+.015,"Over Smoothed Contours",.015,0.,0.);
/*
** Advance the frame.
*/
        c_frame();
/*
** Close GKS.
*/
        gdeactivate_ws(IWKID);
        gclose_ws(IWKID);
        gclose_gks();
        return (0);
/*
** Done.
*/
}


int fill (float*xwrk, float*ywrk, int*nwrk, int*iarea, int*igrp, int*ngrps)
{
/*
**  Retrieve area id for geographic area
** 
**  xwrk,ywrk,nwrk  - working data arrays
**  iarea           - area identifier
**  igrp            - group identifier
**  ngrps           - total number of group identifiers
*/
        int i;          /* counter */
        int iarea3;     /* area id of group 3 */

        Gpoint_list point_list;         /* points of area to be filled */

        for(i=0; i<*ngrps; ++i)
        {
                if (igrp[i] == 3) iarea3=iarea[i];
        }

        if (iarea3 > 0)
        {
/* If the area is defined by 3 or more points, fill it */
                gset_fill_colr_ind(iarea3+2);
                point_list.num_points = *nwrk;
                point_list.points = (Gpoint*)malloc(*nwrk * sizeof(Gpoint));
        	if(!point_list.points) 
		{
            		fprintf(stderr, "fill:  Not enough memory to create");
			fprintf(stderr, " fill area array\n" );
            		gemergency_close_gks();
            		exit(1);
        	}
                for(i=0; i<*nwrk; ++i)
                {
                        point_list.points[i].x = xwrk[i];
                        point_list.points[i].y = ywrk[i];
                }
                gfill_area(&point_list);
		free(point_list.points);
        }

/* Otherwise, do nothing */
        return 1;
}


void color()
{
        Gcolr_rep colr_rep;     /* red, green, blue color values */

/*
**     BACKGROUND COLOR
**  Black
*/
        colr_rep.rgb.red = 0.0;
        colr_rep.rgb.green = 0.0;
        colr_rep.rgb.blue = 0.0;
        gset_colr_rep(1,0,&colr_rep);
/*
**      FORGROUND COLORS
**  White
*/
        colr_rep.rgb.red = 1.0;
        colr_rep.rgb.green = 1.0;
        colr_rep.rgb.blue = 1.0;
        gset_colr_rep(1,1,&colr_rep);
/*
**  Aqua
*/
        colr_rep.rgb.red = 0.0;
        colr_rep.rgb.green = 0.9;
        colr_rep.rgb.blue = 1.0;
        gset_colr_rep(1,2,&colr_rep);
/*
**  Red
*/
        colr_rep.rgb.red = 0.9;
        colr_rep.rgb.green = 0.25;
        colr_rep.rgb.blue = 0.0;
        gset_colr_rep(1,3,&colr_rep);
/*
**  OrangeRed
*/
        colr_rep.rgb.red = 1.0;
        colr_rep.rgb.green = 0.0;
        colr_rep.rgb.blue = 0.2;
        gset_colr_rep(1,4,&colr_rep);
/*
**  Orange
*/
        colr_rep.rgb.red = 1.0;
        colr_rep.rgb.green = 0.65;
        colr_rep.rgb.blue = 0.0;
        gset_colr_rep(1,5,&colr_rep);
/*
**  Yellow
*/
        colr_rep.rgb.red = 1.0;
        colr_rep.rgb.green = 1.0;
        colr_rep.rgb.blue = 0.0;
        gset_colr_rep(1,6,&colr_rep);
/*
**  GreenYellow
*/
        colr_rep.rgb.red = 0.7;
        colr_rep.rgb.green = 1.0;
        colr_rep.rgb.blue = 0.2;
        gset_colr_rep(1,7,&colr_rep);
/*
**  Chartreuse
*/
        colr_rep.rgb.red = 0.5;
        colr_rep.rgb.green = 1.0;
        colr_rep.rgb.blue = 0.0;
        gset_colr_rep(1,8,&colr_rep);
/*
**  Celeste
*/
        colr_rep.rgb.red = 0.2;
        colr_rep.rgb.green = 1.0;
        colr_rep.rgb.blue = 0.5;
        gset_colr_rep(1,9,&colr_rep);
/*
**  Green
*/
        colr_rep.rgb.red = 0.2;
        colr_rep.rgb.green = 0.8;
        colr_rep.rgb.blue = 0.2;
        gset_colr_rep(1,10,&colr_rep);
/*
**  DeepSkyBlue
*/
        colr_rep.rgb.red = 0.0;
        colr_rep.rgb.green = 0.75;
        colr_rep.rgb.blue = 1.0;
        gset_colr_rep(1,11,&colr_rep);
/*
**  RoyalBlue
*/
        colr_rep.rgb.red = 0.25;
        colr_rep.rgb.green = 0.45;
        colr_rep.rgb.blue = 0.95;
        gset_colr_rep(1,12,&colr_rep);
/*
**  SlateBlue
*/
        colr_rep.rgb.red = 0.4;
        colr_rep.rgb.green = 0.35;
        colr_rep.rgb.blue = 0.8;
        gset_colr_rep(1,13,&colr_rep);
/*
**  DarkViolet
*/
        colr_rep.rgb.red = 0.6;
        colr_rep.rgb.green = 0.0;
        colr_rep.rgb.blue = 0.8;
        gset_colr_rep(1,14,&colr_rep);
/*
**  Orchid
*/
        colr_rep.rgb.red = 0.85;
        colr_rep.rgb.green = 0.45;
        colr_rep.rgb.blue = 0.8;
        gset_colr_rep(1,15,&colr_rep);
/*
**  Done.
*/
}

