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

#define N 7 		/* data rows */
#define K 9		/* data columns */
#define LRWK 1000       /* length of real work array */
#define LIWK 1000       /* length of integer work array */
#define LZDT 2000       /* interpolated data array length */


int main()
{
	int i;			/* counter */
	int m;			/* second dimension of sparse data */
	int iwrk[LIWK];		/* integer work array */

	float z[N][K];		/* data */
	float zdat[LZDT];	/* interpolated data */
	float rwrk[LRWK];	/* real work array */

	extern void getdat (float*,int,int*,int);   /* Get data */
	extern void color();			    /* Define color table */

	getdat (&z[0][0], K, &m, N); 

/* Open GKS */
        gopen_gks(IERRF, ISZDM);
        gopen_ws(IWKID, LUNIT, IWTYPE);
        gactivate_ws(IWKID);
        gset_clip_ind (0);

/* Set up color table */
        color();

/* Set each contour level value */
	c_cpseti ("CLS - CONTOUR LEVEL SELECTION",0);
	c_cpseti ("NCL - NUMBER OF CONTOUR LEVELS",13);
	for(i=1; i<=13; ++i)
	{
	   c_cpseti ("PAI - PARAMETER ARRAY INDEX",i);
	   c_cpsetr ("CLV - CONTOUR LEVEL VALUE", (i-1.0)*1000.0-500.0);
	   if ((i%3) == 0)
	   {
/* Make the contour line  dashed */
	     c_cpseti ("PAI - PARAMETER ARRAY INDEX",i);
	     c_cpseti ("CLD - CONTOUR LINE DASH PATTERN", 21845);
	   }
	   else if ((i%3) == 1)
	   {
/* Make the contour line three times as thick */
	     c_cpseti ("PAI - PARAMETER ARRAY INDEX",i);
	     c_cpsetr ("CLL - CONTOUR LINE LINE WIDTH", 3.);
	   }
	   else if ((i%3) == 2)
	   {
/* Make the contour line  red */
	     c_cpseti ("PAI - PARAMETER ARRAY INDEX",i);
	     c_cpseti ("CLC - CONTOUR LINE COLOR INDEX",2);
	   }
	}

/* Initialize Conpack */
	c_cpsps1(&z[0][0],K,m,N,rwrk,LRWK,iwrk,LIWK,zdat,LZDT);

/* Draw perimeter */
	c_cpback(zdat, rwrk, iwrk);
/* Draw Contours */
	c_cpcldr(zdat,rwrk,iwrk);

/* Close frame and close GKS */
        c_frame();
        gdeactivate_ws(IWKID);
        gclose_ws(IWKID);
        gclose_gks();
        return (0);
}


void getdat (float* z, int k, int* m, int n)
/*
** Generate data for plot.
**
** z	- data array
** n,k	- rows, columns of data to generate
** m	- returned dimension of sparse data
*/
{
	int i,j;		/* counters */

	float rmin, rmax;	/* minimum and maximum data values */

	*m=k;
	rmin = 0.;
	rmax = 0.;
	for(i=0; i<k; ++i)
	{
	  for(j=0; j<n; ++j)
	  {
	    z[((j*k)+i)] = -16.*pow((i+1.0),2.0)*(j+1.0)+ 
                           34.0*pow((j+1.0),2.0)*(i+1.0)-(6.0*(i+1.0))+93.;
	    if (rmin > z[((j*k)+i)]) rmin=z[((j*k)+i)];
	    if (rmax < z[((j*k)+i)]) rmax=z[((j*k)+i)];
	  }
	}
}


void color()
/*
** Initialize color table.
*/
{
        Gcolr_rep colr_rep;     /* red, green, blue color values */

/*
**    BACKGROUND color
** Black
*/
        colr_rep.rgb.red = 0.0;
        colr_rep.rgb.green = 0.0;
        colr_rep.rgb.blue = 0.0;
        gset_colr_rep(1,0,&colr_rep);
/*
**     FOREGROUND color
** White
*/
        colr_rep.rgb.red = 1.0;
        colr_rep.rgb.green = 1.0;
        colr_rep.rgb.blue = 1.0;
        gset_colr_rep(1,1,&colr_rep);
/*
** Red
*/
        colr_rep.rgb.red = 0.9;
        colr_rep.rgb.green = 0.25;
        colr_rep.rgb.blue = 0.0;
        gset_colr_rep(1,2,&colr_rep);
}
