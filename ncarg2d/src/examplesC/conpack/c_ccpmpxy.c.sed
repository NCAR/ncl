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

#define KX 26		/* rows of data */
#define JX 60		/* columns of data */
#define LRWK 5000	/* size of real work array */
#define LIWK 5000 	/* size of integer work array */
 
float Z[JX][KX];	/* interpolation grid */

int main()
{
	int i,j;		/* counters */
	int iwrk[LIWK];		/* integer work array */

	float cntr[KX][JX];	/* contour data */
	float rwrk[LRWK];	/* real work array */

	FILE* fp;		/* data file pointer */

				/* Transform contours to overlay mapping type */
	extern int cpmpxy_(int*, float*, float*, float*, float*);

/*
** Read arrays containing data 
*/
	fp = fopen("cpmpxy1.dat","r");
	for(i=0; i<JX; ++i)
	{
		for(j=0; j<KX; ++j)
		{
	    		fscanf(fp,"%f",&Z[i][j]);
		}
	}
	fclose(fp);

	fp = fopen("cpmpxy2.dat","r");
	for(i=0; i<JX; ++i)
	{
		for(j=0; j<KX; ++j)
		{
	    		fscanf(fp,"%f",&cntr[j][i]);
		}
	}
	fclose(fp);
/*
** Do a contour plot
*/
        gopen_gks(IERRF, ISZDM);
        gopen_ws(IWKID, LUNIT, IWTYPE);
        gactivate_ws(IWKID);

	c_set(.1,.95,.25,.85,-110.,-60.,1000.,0.,1);
	c_cpseti("SET - DO-SET-CALL FLAG",0);
	c_cpseti("MAP - MAPPING FLAG",4);
	c_cpsetr("xc1 - X COORDINATE AT INDEX 1",-110.);
	c_cpsetr("XCM - X COORDINATE AT INDEX M",-60.);
	c_cpsetr("SPV - SPECIAL VALUE",-9999.);
	c_cprect(&cntr[0][0],JX,JX,KX,rwrk,LRWK,iwrk,LIWK);
	c_cpcldr(&cntr[0][0],rwrk,iwrk);
	c_labmod("(F5.0)","(F5.0)",0,0,0,0,0,0,0);
	c_gridal(5,0,10,0,1,1,5,-110.,1000.);
	c_set(0.,1.,0.,1.,0.,1.,0.,1.,1);
	c_pchiqu(.03,.6,"PRESSURE",.012,90.,0.);
	c_pchiqu(.5,.2,"LONGITUDE",.012,0.,0.);

        gdeactivate_ws(IWKID);
        gclose_ws(IWKID);
        gclose_gks();
        return (0);
}


int cpmpxy_(int* imap, float* xinp, float* yinp, float* xotp, float* yotp)
{
/*
** Transform contours to overlay various mapping transformations:
** imap= 0 - Cartesian data: no transformation necessary
** imap= 1 - Lat/Lon transformation
** imap=-1 - inverse Lat/Lon transformation
** imap= 2 - Rho/Theta transformation
** imap=-2 - inverse Rho/Theta transformation
** imap= 3 - height in the X direction
** imap= 4 - Pressure in the X direction
** imap= 5 - height in the X direction
** imap= 6 - Pressure in the X direction
**
** xinp, yinp	- x and y input location
** xotp, yotp	- x and y output transformations
*/
	int iix,iy;		/* integer value of xinp and yinp */
	int ixp1,iyp1;  	/* point above and to the right of inputs */

	float xc1;		/* x coordinate */
	float x,y;		/* inputs less integer value of inputs */
	float difx,dify;	/* difference between low point and low cont. */
	float z1,z2,zr; 	/* interpolation values */

/*
** Handle the EZMAP case ...
*/
      	if (abs(*imap) == 1)
	{
        	if (*imap > 0)
		{
          		c_maptra (*yinp,*xinp,xotp,yotp);
		}
        	else
		{
          		c_maptri (*xinp,*yinp,yotp,xotp);
		}
	}
/*
** ... the polar coordinate case ...
*/
      	else if (abs(*imap) == 2)
	{
        	if (*imap > 0)
		{
          		*xotp=*xinp*cos(.017453292519943*(*yinp));
          		*yotp=*xinp*sin(.017453292519943*(*yinp));
		}

        	else
		{
          		*xotp=pow(((*xinp)*(*xinp)+(*yinp)*(*yinp)),0.5);
          		*yotp=57.2957795130823*atan2(*yinp,*xinp);
		}
	}
/*
** Height & Pressure Data in the X direction
*/
/* Pressure transformation in the X direction */
      	else if (*imap == 3 || *imap == 4)
	{
/* The height transformation in X direction is linear */
        	*xotp = *xinp;
/*
** Find next lowest X data point & transform it so that it can be
** used as an array index
*/
		c_cpgetr("XC1",&xc1);
        	x = *xinp-(int)(xc1);
/* Distance between next lowest data point and contour point */
        	iix=(int)(x);
        	difx=x-(float)(iix);
/* Find next lowest y data point */
        	y = *yinp;
/* Distance between next lowest data point and contour point */
        	iy=(int)(y);
        	dify=y-(float)(iy);
/*
** Find next highest X and Y data points, 
** and make sure they are in the domain.
*/
        	ixp1 = iix+1;
		if (ixp1 > JX) ixp1 = JX;
        	iyp1 = iy+1;
		if (iyp1 > KX) ixp1 = KX;
/* Linear interpolation between points to give height at contour point */
        	z1 = Z[iix-1][iy-1]+dify*(Z[iix-1][iyp1-1]-Z[iix-1][iy-1]);
        	z2 = Z[ixp1-1][iy-1]+dify*(Z[ixp1-1][iyp1-1]-Z[ixp1-1][iy-1]);
        	zr = z1 + difx*(z2-z1);
        	*yotp=zr;
/* Pressure Data in the X direction */
        	if (*imap == 4) *yotp = 1000. * exp(-zr/7.);
	}
/*
** If imap isn't specified as above, then do an identity transformation.
*/
        else
	{
          *xotp = *xinp;
          *yotp = *yinp;
	}
	return 1;
} 
