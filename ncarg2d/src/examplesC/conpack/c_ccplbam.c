#include <stdio.h>
#include <math.h>
#include <ncarg/gks.h>
#include <ncarg/ncargC.h>

/*
** Define error file, Fortran unit number, and workstation type,
** and workstation ID.
*/
#define IERRF  "stdout"
#define LUNIT  "gmeta"
#define IWTYPE SED_WSTYPE
#define IWKID  1
#define ISZDM  0

#define MREG    50	/* rows of contour data */
#define NREG    50	/* columns of contour data */
#define LRWK  5000	/* size of real work array */
#define LIWK  5000	/* size of integer work array */
#define LMAP 50000	/* size of map data array */
#define NOGRPS   5 	/* number of groups */
#define NRAN    30	/* number of random data elements */


int main()
{
        float xreg[MREG];		/* x coord. of output grid points */
	float yreg[NREG];		/* y coord. of output grid points */
	float zreg[MREG][NREG]; 	/* contour data */

	extern void getdat(float*,float*,float*); 	/* get data */
	extern void ccplbam(float*);  			/* demo driver */

/* Get data array */
      	getdat(xreg,yreg,&zreg[0][0]);

/* Open GKS */
        gopen_gks(IERRF, ISZDM);
        gopen_ws(IWKID, LUNIT, IWTYPE);
        gactivate_ws(IWKID);
        gset_clip_ind (0);

/* Call Conpack color fill routine */
      	ccplbam(&zreg[0][0]);

/* Close frame and close GKS */
        c_frame();
        gdeactivate_ws(IWKID);
        gclose_ws(IWKID);
        gclose_gks();
        return (0);
}


void ccplbam(float* zreg)
/*
** Demo Driver.
**
** zreg    - contour data.
*/	
{
        float rwrk[LRWK];	/* real work array */
	float xwrk[LRWK];	/* real work array */
	float ywrk[LRWK];	/* real work array */

      	int iwrk[LIWK];		/* integer work array */
        int map[LMAP];		/* map data array */
	int iarea[NOGRPS];	/* area identifiers */
	int igrp[NOGRPS];	/* group identifiers */
	int ncl;		/* number of contour levels */

	extern void color(int);	/* set up color table */

				/* Area processing routine */
	extern int fill(float*, float*, int*, int*, int*, int*);

/* Initialize Areas */
      	c_arinam(map, LMAP);

/* Set contour interval and min and max contours */
      	c_cpsetr("CIS",0.1);
      	c_cpsetr("CMN",-0.4);
      	c_cpsetr("CMX",1.8);

/* Initialize Conpack and pick contour levels */
      	c_cprect(zreg, MREG, MREG, NREG, rwrk, LRWK, iwrk, LIWK);
      	c_cppkcl (zreg, rwrk, iwrk);

/* Set up color table */
      	c_cpgeti("NCL - NUMBER OF CONTOUR LEVELS",&ncl);
      	color(ncl+1);

/* Add contours to area map */
      	c_cpclam(zreg, rwrk, iwrk, map);

/* Add label boxes to area map */
      	c_cplbam(zreg, rwrk, iwrk, map);

/* Set fill style to solid, and fill contours */
      	gset_fill_int_style(1);
      	c_arscam(map, xwrk, ywrk, LRWK, iarea, igrp, NOGRPS, fill);

/* Draw Perimeter */
      	c_cpback(zreg, rwrk, iwrk);

/* Draw Labels */
      	c_cplbdr(zreg, rwrk, iwrk);
}


int fill (float*xwrk, float*ywrk, int*nwrk, int*iarea, int*igrp, int*ngrps)
{
/*
** Retrieve area id for geographic area
**
** xwrk,ywrk,nwrk  - working data arrays
** iarea           - area identifier
** igrp            - group identifier
** ngrps           - total number of group identifiers
*/
	int i;		/* counter */
	int iarea3;	/* area id of group 3 */

        Gpoint_list point_list;		/* points of area to be filled */

	for(i=0; i<*ngrps; ++i)
	{
         	if (igrp[i] == 3) iarea3=iarea[i];
	}

      	if (iarea3 > 0)
	{
/* If the area is defined by 3 or more points, fill it */
         	gset_fill_colr_ind(iarea3+2);
		point_list.num_points = *nwrk;
                point_list.points = (Gpoint *)malloc(point_list.num_points
                                                     *sizeof(Gpoint));
        	if(!point_list.points) 
		{
            		fprintf(stderr, "fill:  Not enough memory");
			fprintf(stderr, " to create fill area array\n" );
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


void getdat(float* xreg, float* yreg, float* zreg)
/*
** Initialize data.
**
** xreg, yreg, zreg	- X, Y, and Z coordinate data.
*/
{
	int i;			/* counter */
      	int iwrk[LIWK];		/* integer work array */

      	float xran[NRAN];	/* x coordinate data */
	float yran[NRAN];	/* y coordinate data */
	float zran[NRAN];	/* z coordinate data */
      	float rwrk[LRWK];	/* real work array */
	float xmin, xmax;	/* x min/max data values */
	float ymin, ymax;	/* y min/man data values */

        xran[0]=12.;   xran[1]= 60.;  xran[2]= 14.;  xran[3]= 33.;
        xran[4]= 8.;   xran[5]= 12.;  xran[6]= 43.;  xran[7]= 57.;
        xran[8]= 22.;  xran[9]= 15.;  xran[10]=19.;  xran[11]= 12.;
        xran[12]= 64.; xran[13]= 19.; xran[14]= 15.; xran[15]= 55.; 
        xran[16]= 31.; xran[17]= 32.; xran[18]= 33.; xran[19]= 29.;
        xran[20]=18.;  xran[21]=  1.; xran[22]= 18.; xran[23]= 42.; 
        xran[24]= 56.; xran[25]=  9.; xran[26]=  6.; xran[27]= 12.; 
        xran[28]= 44.; xran[29]= 19.;

        yran[0]=  1.; yran[1]=  2.; yran[2]=  3.; yran[3]= 53.;
        yran[4]=  7.; yran[5]= 11.; yran[6]= 13.; yran[7]= 17.;
        yran[8]= 19.; yran[9]= 49.; yran[10]= 1.; yran[11]=31.;
        yran[12]=37.; yran[13]= 5.; yran[14]= 7.; yran[15]=47.;
        yran[16]=61.; yran[17]=17.; yran[18]= 5.; yran[19]=23.;
        yran[20]=29.; yran[21]= 3.; yran[22]= 5.; yran[23]=41.;
        yran[24]=43.; yran[25]= 9.; yran[26]=13.; yran[27]=59.;
        yran[28]= 1.; yran[29]=67.;

        zran[0]=1.0;   zran[1]= 1.5;  zran[2]= 1.7;  zran[3]= 1.4;
        zran[4]= 1.9;  zran[5]= 1.0;  zran[6]= 1.5;  zran[7]= 1.2;
        zran[8]= 1.8;  zran[9]= 1.4;  zran[10]= 1.8; zran[11]= 1.7;
        zran[12]= 1.9; zran[13]= 1.5; zran[14]= 1.2; zran[15]= 1.1;
        zran[16]= 1.3; zran[17]= 1.7; zran[18]= 1.2; zran[19]= 1.6;
        zran[20]=1.9;  zran[21]= 1.0; zran[22]= 1.6; zran[23]= 1.3;
        zran[24]= 1.4; zran[25]= 1.8; zran[26]= 1.7; zran[27]= 1.5;
        zran[28]= 1.1; zran[29]= 1.0;
/*
** Set the min and max data values.
*/
      	xmin = 0.0;
      	xmax = 65.0;
      	ymin =  0.0;
      	ymax = 68.0;
/*
** Choose the X and Y coordinates for interpolation points on the 
** regular grid.
*/
	for(i=0; i<MREG; ++i)
	{
         	xreg[i] = xmin + (xmax - xmin) * (float)(i)/MREG;
	}
 
	for(i=0; i<NREG; ++i)
	{
         	yreg[i] = ymin + (ymax - ymin) * (float)(i)/NREG;
	}

/* Interpolate data onto a regular grid */
      	c_idsfft(1,NRAN,xran,yran,zran,MREG,NREG,MREG,xreg,yreg,zreg,iwrk,rwrk);
}


void color (int n)
/*
** Set up color table.
**
** n    - number of different colors.
*/
{
	int i;		/* counter */
      	int icnt;	/* index marker */
      	int lap;	/* border index marker */

      	float redln;	/* line between red and violet values */
      	float heus;	/* hue increment */
      	float xhue;	/* hue value */
	float red, green, blue;	/* red, green, and blue color values */

        Gcolr_rep colr_rep;     /* RGB color structure */
/*
** BACKGROUND color
** BLACK
*/
        colr_rep.rgb.red = 0.0;
        colr_rep.rgb.green = 0.0;
        colr_rep.rgb.blue = 0.0;
        gset_colr_rep (IWKID,0,&colr_rep);

/* First foreground color is white */
        colr_rep.rgb.red = 1.0;
        colr_rep.rgb.green = 1.0;
        colr_rep.rgb.blue = 1.0;
        gset_colr_rep (IWKID,1,&colr_rep);

/* Second foreground color is gray */
        colr_rep.rgb.red = 0.75;
        colr_rep.rgb.green = 0.75;
        colr_rep.rgb.blue = 0.75;
        gset_colr_rep (IWKID,2,&colr_rep);

/* Choose other foreground colors spaced equally around the spectrum */
      	icnt=0;
      	heus=360./n;
/* redln is intended to be the line between red and violet values */
      	redln=36.0;
      	lap=(int)(redln/heus);
	for(i=0; i<n; ++i)
	{
         	xhue=(i+1)*heus;
         	c_hlsrgb(xhue,60.,75.,&red,&green,&blue);
        	colr_rep.rgb.red = red;
        	colr_rep.rgb.green = green;
        	colr_rep.rgb.blue = blue;

/* Sort colors so that the redest is first, and violetest is last */
         	if (xhue <= redln)
		{
            		gset_colr_rep(IWKID,(n+3)-(lap-i),&colr_rep);
            		++icnt;
		}
        	else
		{
            		gset_colr_rep(IWKID,i-icnt+3,&colr_rep);
		}
	}
}
