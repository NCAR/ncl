/*
**      PROGRAM BNCHMK
**
**     This program produces plots illustrating non-trivial
**     usage of the NCAR Utilities.  This program is an
**     exact copy of an implementation that runs on a UNIX
**     machine at NCAR.  To implement this program on another
**     machine only the following steps should be required:
**
**      1.)  Modify the OPEN to the EZMAP dataset that
**           appears in the main program.
**
**      2.)  Modify the OPEN to the file of random numbers
**           RANFDAT in subroutine RANDNO.
**
**    The utilities AUTOGRPH, VELVCT, conrec_, EZMAP, GRIDAL,
**    and DASHCHAR are required.  Since conrec_ and VELVCT plots
**    are overlaid on EZMAP, the arithmetic statement functions
**    for FX and FY in these two utilities should be changed
**    to the internal functions:
**
**     EXTERNAL FX, FY
**
**    This will force the functions fx_ and fy_ supplied in this
**    package to be loaded.  The documentation for fx_ explains
**    the transformations provided.
**
**    This program has been translated from the FORTRAN program
**    bnchmk.f to C language.
*/

#include <stdio.h>
#include <string.h>
#include <math.h>
#include <ncarg/gks.h>
#include <ncarg/ncargC.h>

#define M 	70		/* number of columns of data */
#define NPR 	155		/* number of rows of data */
#define N 	150		/* number of visualized rows of data */	
#define PI 	3.14159		/* PI */
#define TWOPI 	(PI*2.0)	/* 2*PI */
#define EPS 	(PI/6.0)	/* epsilon, PI/6 */

#define WSTYPE SED_WSTYPE
#define WKID   1


int Ma;			/* number of data columns to be transformed */
int Na;			/* number of data rows to be transformed */
int Trans;		/* transformation selector */
float A1;		/* angle 1, in radians, for polar conversion */
float A2;		/* angle 2, in radians, for polar conversion */
float D1;		/* radius of A1 */
float D2;		/* radius of A2 */
float Alnmn;		/* minimum map longitude */
float Alnmx;		/* maximum map longitude */
float Altmn;		/* minimum map latitude */
float Altmx;		/* maximum map latitude */

int main()
{
	extern int Ma;		/* number of data columns to be transformed */
	extern int Na;		/* number of data rows to be transformed */
	extern int Trans;	/* transformation selector */
	extern float A1;	/* angle 1, in radians, for polar conversion */
	extern float A2;	/* angle 2, in radians, for polar conversion */
	extern float D1;	/* radius of A1 */
	extern float D2;	/* radius of A2 */
	extern float Alnmn;	/* minimum map longitude */
	extern float Alnmx;	/* maximum map longitude */
	extern float Altmn;	/* minimum map latitude */
	extern float Altmx;	/* maximum map latitude */

      	char* ldash[1];		/* plotted line pattern */

	int j;			/* counter */
	int iers;		/* error condition */
	int idm;		/* dummy variable */
        int nset;		/* plot scaling control */
        int nhi;		/* extra information print control */
        int ndot;		/* contour line pattern */

 	float lo;		/* low contour value */
        float hi;		/* high contour value */
        float inc;		/* contour increment */
	float data[NPR][M];	/* data array */
	float yt;		/* viewport top margin */
	float yb;		/* viewport bottom margin */
	float xmin;		/* left window margin, in user coordinates */
	float xmax;		/* right window margin, in user coordinates */
	float ymin;		/* bottom window margin, in user coordinates */
	float ymax;		/* top window margin, in user coordinates */
	float delx;		/* calculated left/right margin */
	float dely;		/* calculated upper/lower margin */
	float fdm;		/* dummy variable */
	float plm1,plm2,plm3,plm4;	/* supmap parameters */

				/* generate coordinate data */
	extern void Genera (float*,float*,int,int);	

/*
** Open GKS, open workstation, activate workstation.
*/
	gopen_gks ("stdout",0);
	gopen_ws (WKID, NULL, WSTYPE);
	gactivate_ws(WKID);
/*
** Generate the input array.
*/
      	Genera(&data[4][0],&data[0][0],M,N);

/*
** Frame 1 -- three curves on same plot using AUTOGRAPH.
*/
	ldash[0] = (char*)malloc(16*(sizeof(char)));	
	strcpy((char*)ldash,"$$'$$'$$'$$'$$'$");
      	c_agseti("DASH/LENGTH.",16);
      	c_displa(2,0,1);
	for(j=0; j<3; ++j)
	{
      		yt = 0.3 * (float)(j+1);
      		yb = 0.3 * (float)(j) + 0.05;
      		c_set(0.1,0.9,yb,yt,0.0,1.0,0.0,1.0,1);
      		c_anotat("X$","Y$",3,2,1,ldash);
      		c_ezy(&data[(3*(j+1)-1)][0],60," $");
	}
      	c_frame();
/*
** Frame 2 -- conrec_ plot with polar coordinate Transformation.
*/
      	Trans = 2;
      	D1 = 20.0;
      	Ma = 30;
      	Na = 70;
      	Genera(&data[4][0],&data[0][0],Ma,Na+12);
      	xmin = -60.0;
      	xmax =  60.0;
      	ymin = -80.0;
      	ymax =  80.0;

      	c_set(0.1,0.9,0.1,0.9,xmin,xmax,ymin,ymax,1);
      	c_labmod ("(F5.0)","(F5.0)",5,5,0,0,0,0,0);
      	c_periml(6,2,6,2);
      	D2 = 30.0;
      	A1 = EPS;
      	A2 = TWOPI - 0.5 * A1;
      	delx = 1.25 * (D1+D2);
      	dely = 1.50 * (D1+D2);
      	c_set(0.05,0.95,0.05,0.95,-delx,delx,-dely,dely,1);

	lo = 0.0;
	hi = 0.0;
	inc = 0.0;
	nset = 1;
	nhi = -1;
	ndot  = 0;
	conrec_(&data[3][0],&Ma,&Ma,&Na,&lo,&hi,&inc,&nset,&nhi,&ndot);
      	c_frame();

/*
** Frame 3 -- conrec_ overlaid on EZMAP.
*/
      	Ma = 55;
      	Na = 45;
      	Trans = 1;
      	Genera(&data[4][0],&data[0][0],Ma,Na);

	plm1 = 0.0;
	plm2 = 0.0;
	plm3 = 0.0;
	plm4 = 0.0;
      	c_supmap(2,40.0,-90.0,0.0,&plm1,&plm2,&plm3,&plm4,1,20,4,0,&iers);

      	Alnmn = -160.0;
      	Alnmx = -20.0;
      	Altmn = 0.0;
      	Altmx = 60.0;
	
	lo = -1.4;
	hi = 1.8;
	inc = 0.2;
	nset = 1;
	nhi = 0;
	ndot  = -585;
	conrec_(&data[0][0],&Ma,&Ma,&Na,&lo,&hi,&inc,&nset,&nhi,&ndot);
      	c_frame();
/*
** Frame 4 -- conrec_ overlaid on EZMAP.
*/
      	Ma = 55;
      	Na = 45;
      	Trans = 1;
      	Genera(&data[4][0],&data[0][0],Ma,Na);

	plm1 = 50.0;
	plm2 = -130.0;
	plm3 = 20.0;
	plm4 = -75.0;
      	c_supmap(3,45.0,-100.0,45.0,&plm1,&plm2,&plm3,&plm4,2,10,3,1,&iers);

      	Alnmn = -120.0;
      	Alnmx = -75.0;
     	Altmn = 25.0;
      	Altmx = 50.0;

	lo = -1.2;
	hi = 1.1;
	inc = 0.1;
	nset = 1;
	nhi = -1;
	ndot  = 1;
	conrec_(&data[0][0],&Ma,&Ma,&Na,&lo,&hi,&inc,&nset,&nhi,&ndot);
      	c_frame();
/*
** Frame 5 --  conrec_ with modified background.
*/
      	Ma = 40;
      	Na = 40;
      	Trans = 0;
      	Genera(&data[4][0],&data[0][0],40,40);

      	c_set(0.1,0.9,0.1,0.9,-1.0,1.0,-2.0,2.0,1);
      	c_labmod("(F3.0)","(F3.0)",3,3,0,0,0,0,0);
      	c_periml(2,10,4,5);
	c_cpseti("LLP - LINE LABEL POSITIONING", 2);

	lo = -1.0;
	hi = 1.0;
	inc = 0.5;
	nset = -1;
	nhi = -1;
	ndot  = -682;
	conrec_(&data[0][0],&Ma,&Ma,&Na,&lo,&hi,&inc,&nset,&nhi,&ndot);
      	c_frame();
/*
** Frame 6 -- VELVCT overlaid on EZMAP.
*/
      	Trans = 2;
      	Genera(&data[4][0],&data[0][0],60,60);

      	c_supmap(3,45.0,-100.0,45.0,&plm1,&plm2,&plm3,&plm4,2,15,3,0,&iers);

      	Alnmn = -120.0;
      	Alnmx = -75.0;
      	Altmn = 25.0;
      	Altmx = 50.0;

	c_vvseti("MAP -- Mapping Mode", 1);
	c_vvseti("SET -- Do SET Call Flag", 0);
	c_vvsetr("XC1 -- Lower X Bound", -120.0);
	c_vvsetr("XCM -- Upper X Bound", -75.0);
	c_vvsetr("YC1 -- Lower Y Bound", 25.0);
	c_vvsetr("YCN -- Upper Y Bound", 50.0);

      	Ma = 25;
      	Na = 25;
      	idm = 0;
	fdm = 0.0;
	c_vvinit(&data[9][0],60,&data[24][0],60,&fdm,60,Ma,Na,&fdm,idm);
	c_vvectr(&data[9][0],&data[24][0],&fdm,0,0,0);
      	c_frame();
/*
** Deactivate and close workstation, close GKS.
*/
	gdeactivate_ws(WKID);
	gclose_ws(WKID);
	gclose_gks();
	return 0;
}


void Genera (float* wrk_dat, float* ret_data, int cols, int rows)
{
/*
** This subroutine generates a smooth array in output array ret_data.
** The array is dependent on the random number function Randno.
** Randno can be replaced by a random number generator on a
** given machine, or for consistency across machines, the
** supplied function Randno may be used.  Randno reads its random
** numbers from the file ranfdat.
*/ 
      	int i,j,k,l;		/* counters */
      	int rc_ave;		/* (rows+columns)/10 */
      	int plow_indx;		/* lower exp index */
      	int phigh_indx;         /* upper exp index */
      	int lwr_col;		/* lower column index */
      	int uppr_col;		/* upper column index */
      	int lwr_row;		/* lower row index */
      	int uppr_row;		/* upper row index */
      	int wghtd_pwr;		/* weighted average of surrounding data */
      	int min_pwr;		/* minimum allowable power exponent */

      	float pos_neg;		/* alternating plus-minus variable */
      	float pwr_val;		/* power exponent used to calculate data */
     
	extern float Randno();	/* random number generator */
/*
** Null the working data area
*/
	for(j=0; j<rows; ++j)
        {
		for(i=0; i<cols; ++i)
               	{
			wrk_dat[((j*cols)+i)] = 0.0;
		}
	}
/*
** Set up working data area with relative weights 
** corresponding to data values.
*/
      	rc_ave = (cols+rows) / 10;
      	pos_neg = 1.0;
	for(l=0; l<2; ++l) 
	{
      		for(k=0; k<rc_ave; ++k)
      		{
      	  		plow_indx = 3.0 + (cols - 4) * Randno();
      	 		phigh_indx = 3.0 + (rows - 4) * Randno();

	  		for(j=0; j<rows; ++j)
          		{
      				wghtd_pwr = abs((j+1)-phigh_indx);
				for(i=0; i<cols; ++i)
               	 		{
				/*
				** Make sure the exponent is not
				** below the minimum allowable value.
				*/
      					min_pwr = abs((i+1) - plow_indx);
					pwr_val = wghtd_pwr*1.0;
					if (pwr_val < (min_pwr*1.0))
					{
      						pwr_val = min_pwr*1.0;
					}
      					wrk_dat[((j*cols)+i)] = 
                                                          wrk_dat[((j*cols)+i)]+
							  pos_neg *
						          pow(0.8,pwr_val);
                		}
          		}
      		}
	  	pos_neg = -1.0;
      	}
	
/*
** Smooth the working data and store the results
** in ret_data.
*/
	for(j=0; j<rows; ++j)
        {
	/*
	** Make sure lower and upper row indexes
	** are within bounds.	
	*/
                lwr_row = j - 1;
		if (lwr_row < 0)
		{
			lwr_row = 0;
		}
		uppr_row = j + 1;
		if (uppr_row >= rows)
		{
      			uppr_row = rows - 1;
		}
		for(i=0; i<cols; ++i)
               	{
		/*
		** Make sure lower and upper column indexes
		** are within bounds.	
		*/
			lwr_col = i - 1;
			if (lwr_col < 0)
			{
				lwr_col = 0;
			}
			uppr_col = i + 1;
			if (uppr_col >= cols)
			{
      				uppr_col = cols - 1;
			}
     			ret_data[((j*cols)+i)] = (4.0* wrk_dat[(i+(j*cols))] + 
				         2.0*(wrk_dat[(i+(lwr_row*cols))] +
			                 wrk_dat[(lwr_col+(j*cols))] + 
                                         wrk_dat[(uppr_col+(j*cols))] + 
				         wrk_dat[(i+(uppr_row*cols))]) + 
                                         wrk_dat[(lwr_col+(lwr_row*cols))] + 
					 wrk_dat[(uppr_col+(lwr_row*cols))] + 
                                         wrk_dat[(lwr_col+(uppr_row*cols))] +
                                         wrk_dat[(uppr_col+(uppr_row*cols))]) 
                                         / 16.0;
		}	
        }
}


float Randno()
{
/*
**     This function is used to produce random numbers for the
**     Genera calls.  Random numbers are read from file
**     Ranfdat, and in this way consistency is maintained across
**     machines (the results on one machine should be the same
**     as on another.)  if consistency is not required, the
**     function Randno can be replaced by a local random number
**     generator.  Ranfdat contains 2000 random numbers in
**     250 card-image lines in format 8F10.8 .
*/
	int i,j;			/* counters */
	int i_indx;			/* random table row index */

	FILE* fp;			/* file pointer */

	static int counter;		/* flag to fill random data table */
	static float rand_dat[2000];	/* random data */

	extern FILE* Openr();		/* open file in read only mode */
 

      	++counter;
/*
** Read in random numbers if this is the first function call.
*/
      	if (counter == 1)
      	{
		fp = Openr();
		if (fp != NULL)
		{
			for(i=0; i<250; ++i)
			{
        			i_indx = 8 * i;
				for(j=0; j<8; ++j)
				{
					fscanf(fp,"%f",&rand_dat[i_indx+j]);
				}
			}
			fclose(fp);
		}
		else
		{
			printf("ERROR READING DATA FILE- ");
			printf("PROGRAM ABORTED\n");
			exit (1);
		}
      	}
	i_indx = counter - 1;
      	return (rand_dat[i_indx]);
}


FILE* Openr ()
{
/*
** Open the $NCARG_ROOT/database file in read only mode
*/
	int i;			/* counter */
	int stat;		/* gngpat status */

	FILE* unit;		/* data file */

    	char *filenm2 = "database";	/* strings to create full path name */
    	char *filenm = "                                                                                                                                ";
      	static int open;	/* file open flag */

	/*
	** Cray machine dependencies require
	** extra variables.
	*/
#if defined (cray)
    	int len;		/* path length */
    	_fcd cftfilenm;		/* full path */
    	_fcd cftfilenm2;	/* append file name */
#endif


      	if (open == 0)
	{
		/*
		** Take care of Cray machine dependencies
		*/
#if !defined (cray)
      		gngpat_(filenm,filenm2,&stat,119,9);
#else
          	cftfilenm = _cptofcd(filenm, strlen(filenm));
        	cftfilenm2 = _cptofcd(filenm2, strlen(filenm2));
        	gngpat_(cftfilenm,cftfilenm2,&stat);
        	len = _fcdlen(cftfilenm);
        	filenm = (char*)malloc(len*sizeof(char));
        	filenm = _fcdtocp(cftfilenm);
#endif

      		if (stat != -1)
		{
		/*
		** Append file name to root path
		*/
			for(i=0; i<119; ++i)
			{
              			if (filenm[i] == '\0')
				{
					strcat(filenm,"/ranfdata");
  					unit = fopen(filenm, "r");
					if (unit == NULL)
					{
  						printf("ERROR OPENING ");
						printf("RANFDATA DATA FILE ");
						printf("- FILE ");
						printf("NAME: %s\n",filenm);
					}
					return(unit);
				}
			}
 			printf("ERROR OPENING RANFDATA DATA FILE - ");
			printf("FILE NAME: %s\n",filenm);
			unit = NULL;
			return(unit);
		}
      		else
		{
       		        printf("Openr - %s\n",filenm);
			unit = NULL;
			return(unit);
		}
	}
	return NULL;
}


float fx_(float* X, float* Y)
{
/*
**
** This function implements three transformations on the input
** coordinate pair (X,Y) .  The coordinate (X,Y) is assumed to
** be contained in the rectangle 1 to Ma by 1 to Na.  The
** transformation selected depends on the value of Trans:
**
** Trans=0     The identity Transformation is used.
**
** Trans=1     Provides a linear mapping of the rectangular
**             input array onto the region  Alnmn  to  Alnmx
**             by  Altmn  to  Altmx.  The four parameters
**             Alnmn, Alnmx, Altmn, and Altmx are assumed
**             to specify two longitudes (Alnmn--minimum
**             longitude; Alnmx--maximum longitude) and
**             and two latitudes (Altmn--minimum latitude;
**             Altmx--maximum latitude.)  The latitudes and
**             longitudes in the transformed space are then
**             projected onto the plot using the current
**             EZMAP projection (hence EZMAP must be called
**             prior to calling FX with Trans=1.)
**
** Trans=2     Polar transformation.  The rectangular region
**             bounded by the corner points (1,1), (Ma,1),
**             (Ma,Na), (1,Na)  is transformed onto the region
**             in polar coordinates bounded by the polar
**             coordinates (D1,A1), (D1,A2), (D2,A2), (D1,A2)
**             where the angles A1 and A1 are expressed in radians.
*/
	extern int Ma;		/* number of data columns to be transformed */
	extern int Na;		/* number of data rows to be transformed */
	extern int Trans;	/* transformation selector */
	extern float A1;	/* angle 1, in radians, for polar conversion */
	extern float A2;	/* angle 2, in radians, for polar conversion */
	extern float D1;	/* radius of A1 */
	extern float D2;	/* radius of A2 */
	extern float Alnmn;	/* minimum map longitude */
	extern float Alnmx;	/* maximum map longitude */
	extern float Altmn;	/* minimum map latitude */
	extern float Altmx;	/* maximum map latitude */

	float ydum=0.0;		/* dummy variable */
	float fxlon;		/* return longitude value */
	float ylat;		/* y coordinate corresponding latitude */
	float xlon;		/* x coordinate corresponding longitude */

/*
** Identity transformation.
*/
	if (Trans == 0)
	{
      		return *X;
	}
/*
** EZMAP overlaying.
*/
	else if (Trans == 1)
	{
      		xlon = Alnmn + (*X-1.0)*(Alnmx-Alnmn)/((float)(Ma)-1.0);
      		ylat = Altmn + (*Y-1.0)*(Altmx-Altmn)/((float)(Na)-1.0);
      		c_maptrn(ylat, xlon, &fxlon, &ydum);
      		return fxlon;
	}
/*
** Polar coordinate transformation.
*/
	else if (Trans == 2)
	{
      		return (( D1+D2*(*X-1.0) / ((float)(Ma)-1.0)) *
                       cos(A1+A2*(*Y-1.0)/((float)(Na)-1.0) ));
	}
/*
** Undefined transformation
*/
	else
	{
		return *X;
	}
}
      

float fy_(float* X, float* Y)
{
/*
**
** The function fy_ behaves in a similar manner to fx_ as
** described above.
*/
	extern int Ma;		/* number of data columns to be transformed */
	extern int Na;		/* number of data rows to be transformed */
	extern int Trans;	/* transformation selector */
	extern float A1;	/* angle 1, in radians, for polar conversion */
	extern float A2;	/* angle 2, in radians, for polar conversion */
	extern float D1;	/* radius of A1 */
	extern float D2;	/* radius of A2 */
	extern float Alnmn;	/* minimum map longitude */
	extern float Alnmx;	/* maximum map longitude */
	extern float Altmn;	/* minimum map latitude */
	extern float Altmx;	/* maximum map latitude */

	float xdum=0.0;		/* dummy variable */
	float fylat;		/* return latitude value */
	float ylat;		/* y coordinate corresponding latitude */
	float xlon;		/* x coordinate corresponding longitude */
/*
** The identity transformation.
*/
	if (Trans == 0)
	{
      		return *Y;
	}
/*
** EZMAP overlaying.
*/
	else if (Trans == 1)
	{
      		xlon = Alnmn + (*X-1.0)*(Alnmx-Alnmn)/((float)(Ma)-1.0);
      		ylat = Altmn + (*Y-1.0)*(Altmx-Altmn)/((float)(Na)-1.0);
      		c_maptrn(ylat, xlon, &xdum, &fylat);
      		return fylat;
	}
/*
** Polar coordinate transformation.
*/
	else if (Trans == 2)
	{
      		return ((D1+D2*(*X-1.0)/((float)(Ma)-1.0))*
                         sin(A1+A2*(*Y-1.0)/((float)(Na)-1.0)));
	}
/*
** Undefined transformation
*/
	else
	{
		return *Y;
	}
}
