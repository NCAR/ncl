#include <stdio.h>
#include <math.h>
#include <ncarg/gks.h>
#include <ncarg/ncargC.h>
#include "c_ggdini.c"

/*
** Define error file, Fortran unit number, and workstation type,
** and workstation ID.
*/
#define IERRF  "stdout"
#define LUNIT  "gmeta"
#define IWTYPE SED_WSTYPE
#define IWKID  1
#define ISZDM  0

#define M 30		/* rows of data */
#define N 20		/* columns of data */
#define LRWK 3500	/* size of real work array */
#define LIWK 3500 	/* size of integer work array */

#define RMNLON (-190.0) /* minimum longitude value */
#define RMXLON 20.0	/* maximum longitude value */
#define RMNLAT (-20.0)  /* minimum latitude value */
#define RMXLAT 50.0	/* maximum latitude value */


int main()
{
	int IWRK[LIWK];		/* integer work array */

				/* create contour data */
	extern void mkdat(int,float*,int,int); 

	float z[M][N];		/* contour data */
	float rwrk[LRWK];	/* real work array */
/*
**  United States corner latitude and longitudes
*/
	float rlat1[2];		/* lower latitude */
	float rlat2[2];		/* upper latitude */
	float rlon1[2];		/* lower longitude */
	float rlon2[2];		/* upper longitude */


	rlat1[0]=RMNLAT; rlat1[1]=0.0;
	rlat2[0]=RMXLAT; rlat2[1]=0.0;
	rlon1[0]=RMNLON; rlon1[1]=0.0;
	rlon2[0]=RMXLON; rlon2[1]=0.0;

	mkdat (1, &z[0][0], M, N); 
/*
** Open GKS
*/
        gopen_gks(IERRF, ISZDM);
        gopen_ws(IWKID, LUNIT, IWTYPE);
        gactivate_ws(IWKID);
/*
** Turn off clipping.
*/
        gset_clip_ind (0);
/*
** Draw Lat/Lon lines at 10 degree intervals.
** Draw political & continental outlines.
*/
	c_mapsti ("GR - GRID",10);
        c_mapstc ("OU - OUTLINE DATASET","PO");
/*
** Draw a Satellite view over the United States
*/
	c_maproj ("SV - SATELLITE-VIEW",35.,-100.,0.);
	c_mapset ("MA",rlat1,rlon1,rlat2,rlon2);
/*
** Don't draw a square around the globe
*/
	c_mapsti ("PE - PERIMETER FLAG", 0);
/*
** Draw map.
*/
	c_mapdrw();
/*
** Tell CONPACK that the SET call has been done, force it to generate X
** coordinates that are longitudes and Y coordinates that are latitudes,
** turn on mapping to an EZMAP background. Define the out-of-range value
** (returned by MAPTRN for an unprojectable point).
*/
        c_cpseti ("SET - DO SET-CALL FLAG",0);
        c_cpsetr ("XC1 - X COORDINATE AT I = 1",RMNLON);
        c_cpsetr ("XCM - X COORDINATE AT I = M",RMXLON);
        c_cpsetr ("YC1 - Y COORDINATE AT J = 1",RMNLAT);
        c_cpsetr ("YCN - Y COORDINATE AT J = N",RMXLAT);
        c_cpseti ("MAP - MAPPING FLAG",1);
        c_cpsetr ("ORV - OUT-OF-RANGE VALUE",1.0e12);
/*
** Initialize the drawing of the first contour plot.
*/
        c_cprect (&z[0][0],N,N,M,rwrk,LRWK,IWRK,LIWK);
/*
** Draw Contours
*/
	c_cpcldr(&z[0][0],rwrk,IWRK);

/* Close frame and close GKS */
        c_frame();
        gdeactivate_ws(IWKID);
        gclose_ws(IWKID);
        gclose_gks();
        return (0);
}


void mkdat(int nctfr, float* zdat, int n, int m)
{
/*
** Create some data to contour
**
** nctfr	- random number generator
** zdat		- contour data
** m,n		- rows, columns of data
*/
	int i,j;		/* counters */

	float zmin, zmax;	/* min, max values */
	float rlon, rlat;	/* longitude, latitude */
/*
** nctfr is used to generate a random number, zdat is the data array
*/
        c_ggdini(0.,1.,nctfr,.9);
        zmin = 1.0e36;
        zmax = 1.0e36*(-1.0);
	for (i=0; i<m; ++i)
	{
           	rlon=.017453292519943*(-180.+360.*(i*1.0)/(m-1.0));
		for (j=0; j<n; ++j)
		{
              		rlat=.017453292519943*(-90.+180.*(j*1.0)/(n-1.0));
              		zdat[((j*m)+i)]=c_ggdpnt(rlat,rlon)+.5*cos(4.*rlat);
			if (zdat[((j*m)+i)] < zmin)
			{
              			zmin=zdat[((j*m)+i)];
			}
			if (zdat[((j*m)+i)] > zmax)
			{
              			zmax=zdat[((j*m)+i)];
			}
		}
	}

	for (i=0; i<m; ++i)
	{
		for (j=0; j<n; ++j)
		{
              		zdat[((j*m)+i)]=((zdat[((j*m)+i)]-zmin) /
                                        (zmax-zmin))*130.-50.;
		}
	}
}
