/*
**      PROGRAM VVEX02
**
** This program plots a randomly generated data set of velocity vector
** components on 10 different EZMAP projections. The vector 
** directions are transformed into map coordinate space. The last
** five frames make additional adjustments to the vector
** rendering parameters.
**
** This file is a conversion of vvex02.f to vvex02.c.
*/

#include <stdio.h>
#include <string.h>
#include <math.h>
#include <ncarg/gks.h>
#include <ncarg/ncargC.h>

#define M 70	 	/* vector data, number of columns */
#define NPR 155	 	/* vector data, number of rows */
#define N 150		/* vector data, visualized number of rows */

#define WSTYPE SED_WSTYPE
#define WKID   1

int main()
{
	int i;				/* counter */
	int err;			/* error flag */
	int ll;				/* getset parameter */
	int ma;				/* vvinit parameter */
	int na;				/* vvinit parameter */

	float vctr_dat[NPR][M];		/* vector coordinate data */
	float scl_dat[N][M];		/* scalar vector data */

	float vmn;			/* minimum vector */
	float vmx;			/* maximum vector */
	float dmx;			/* distance of max vector */
	float vrl;			/* vector realized length */
	float vfr;			/* vector fractional minimum */
	float plm1, plm2, plm3, plm4; 	/* supmap parameters */
	float vl, vr, vb, vt;		/* getset parameters */
	float ul, ur, ub, ut;		/* getset parameters */
	float flt_dum = 0.0;		/* dummy parameter */

	extern void Dfclrs();		/* define colors */

					/* generate coordinate data */
	extern void Genera(float*,float*,int,int);	 
					/* make scalar data */
	extern void Gendat (float*,int,int,int,int,float,float); 

/*
** Open GKS, open workstation, activate workstation.
*/
	gopen_gks ("stdout",0);
	gopen_ws (WKID, NULL, WSTYPE);
	gactivate_ws(WKID);
/*
** Generate the input arrays and set up the color table
*/
      	Genera(&vctr_dat[4][0],&vctr_dat[0][0],M,N);
      	Gendat(&scl_dat[0][0],60,60,25,25,(float)-1.0e+2,(float)1.0e+2);
      	Dfclrs();
/*
** Set up the EZMAP mapping, and the data coordinate space, 
*/
      	c_vvseti("SET -- Set Call Flag", 0);
      	c_vvseti("MAP -- Mapping Flag", 1);
      	c_vvsetr("XC1 -- Lower X Bound", -180.0);
      	c_vvsetr("XCM -- Upper X Bound", 180.0);
      	c_vvsetr("YC1 -- Lower X Bound", -90.0);
      	c_vvsetr("YCN -- Upper Y Bound", 90.0);
/*
** Set up Vector's color index array
*/
      	c_vvseti("NLV -- Number of Levels", 14);
	for(i=1; i<15; ++i)
	{
         	c_vvseti("PAI -- Parameter Array Index", i);
         	c_vvseti("CLR -- GKS Color Index", i+1);
	}
/*
** Miscellaneous rendering parameters
*/ 
      	c_vvsetr("LWD -- Vector Line Width", 2.25);
      	c_vvseti("VPO -- Vector Position Method", 0);
      	c_vvsetr("VFR -- Vector Fractional Minimum", 0.33);
 
	for(i=1; i<11; ++i)
	{
/*
** Alternately color vectors based on magnitude or scalar array value
*/
         	if ((i%2) == 0)
		{
            		c_vvseti("CTV -- Color Thresholds Value", 1);
		}
         	else
		{
            		c_vvseti("CTV -- Color Thresholds Value", 2);
		}
/*
** Do 10 different easy map projections
*/
         	if (i == 3)
		{
			plm1 = 90.0;
			plm2 = 80.0;
			plm3 = 0.0;
			plm4 = 0.0;
            		c_supmap(3,0.0,80.0,70.0,&plm1,&plm2,&plm3,&plm4,
				 2,20,4,0,&err);
		}
         	else
		{
			plm1 = 0.0;
			plm2 = 0.0;
			plm3 = 0.0;
			plm4 = 0.0;
            		c_supmap(i,0.0,0.0,0.0,&plm1,&plm2,&plm3,&plm4,
				 1,20,2,0,&err);
		}
 
         	ma = 25;
         	na = 25;

		c_vvinit(&vctr_dat[9][0],M,&vctr_dat[24][0],M,&scl_dat[0][0],
		         60,ma,na,&flt_dum,0);
/* 
** For the last 5 frames adjust the Vector rendering parameters
** based on the projection and dataset contents
*/
         	if (i >= 5)
		{
            		c_vvgetr("VMN -- Minimum Vector", &vmn);
            		c_vvgetr("VMX -- Maximum Vector", &vmx);
            		c_vvsetr("VLC -- Vector Low Cutoff",vmn+(vmx-vmn)/5.0);
            		c_vvgetr("VFR -- Vector Fractional Minimum",&vfr);
            		c_vvgetr("DMX -- Distance of Max Vector",&dmx);
            		c_getset(&vl,&vr,&vb,&vt,&ul,&ur,&ub,&ut,&ll);
            		vrl = 1.5 * dmx / (vr - vl);
            		c_vvsetr("VRL - Vector Realized Length", vrl);
		}
 
 		c_vvectr(&vctr_dat[9][0],&vctr_dat[24][0],&scl_dat[0][0],0,0,0);
		c_frame();
	}
/*
** Deactivate and close work station, close GKS.
*/
        gdeactivate_ws(WKID);
        gclose_ws(WKID);
        gclose_gks();
	return 0;
}


void Gendat (float* data, int clmns, int rows, int mlow,
             int mhgh, float dlow, float dhgh)
{
/*
** Fill the array data(i,j) with a two-dimensional field of data
** having approximately "mlow" lows and "mhgh" highs, a minimum
** value of exactly "dlow" and a maximum value of exactly "dhgh".
**
** "mlow" and "mhgh" are each forced to be greater than or equal to 1
** and less than or equal to 25.
**
** The function used is a sum of exponentials.
*/
	int i,j,k;		/* counters */
	int indx;		/* data[i][j] linear index */
	int nlow;		/* number of low points */
	int nhgh;		/* number of high points */
	int ncnt;		/* total of high and low points */

        float ccnt[3][50];	/* columns of random data */
	float fovm, fovn;	/* exponential column/row parameters */
	float dmin, dmax;	/* min/max data values */
	float temp;		/* temporary storage */
	float temp2;		/* temporary storage */

	extern float Fran();	/* find random number */
 

        fovm = 9.0 / ((float)clmns);
        fovn = 9.0 / ((float)rows);
/*
** Set maximum allowable low points
*/
	nlow = 25;
	if (nlow > mlow)
        {
		nlow = mlow;
	} 
        if (nlow < 1)
        {
		nlow = 1;
	} 
/*
** Set maximum allowable high points
*/
	nhgh = 25;
	if (nhgh > mhgh)
        {
		nhgh = mhgh;
	} 
        if (nhgh < 1)
        {
		nhgh = 1;
	} 
        ncnt = nlow+nhgh;
/*
** Get random data
*/
        for(k=0; k<ncnt; ++k)
        {
          	ccnt[0][k] = 1.0 + (float)(clmns-1.0) * Fran();
          	ccnt[1][k] = 1.0 + (float)(rows-1.0) * Fran();
          	if (k <= (nlow-1))
          	{
            		ccnt[2][k] = -1.0;
          	}
          	else
          	{
            		ccnt[2][k] = 1.0;
          	}
        }
/*
** Perform sum of exponentials
*/
        dmin = 1.0e36;
        dmax = -1.0e36;
        for(j=0;j<rows;++j)
        {
          	for(i=0;i<clmns;++i)
          	{
	    		indx = (j*clmns)+i;	
           		data[indx] = 0.5 * (dlow+dhgh);
           		for(k=0; k<ncnt; ++k)
            		{
              			temp = fovm * (((float)(i+1))-ccnt[0][k]);
				temp = temp * temp;
			        temp2 = fovn * (((float)(j+1))-ccnt[1][k]);
				temp2 = temp2 * temp2;
  				temp = -(temp + temp2);
              			if (temp >= -20.0) 
              			{
                  			data[indx] = data[indx]+0.5*(dhgh-dlow)
                                	             *ccnt[2][k]*exp(temp);
              			}
            		}
		/* 
		** Set min and max data values
		*/
	    		if (dmin > data[indx])
	    		{
                  		dmin = data[indx];
	    		}
	   		if (dmax < data[indx])
	    		{
                  		dmax = data[indx];
	    		}
          	}
        }
/*
** Set generated data within allowable limits
*/
        for(j=0;j<rows;++j)
        {
          	for(i=0;i<clmns;++i)
          	{
	    		indx = (j*clmns)+i;	
            		data[indx] = (data[indx]-dmin)/(dmax-dmin)*
				      (dhgh-dlow)+dlow;
          	}
        }
}


float Fran()
{
/*
** Return data from a table of random numbers.  The next value
** in sequence is returned with each call.
*/
        static int sequence;	/* current table index */

        float rtsq[100]; 

         rtsq[0]=0.749;  rtsq[1]=0.973;  rtsq[2]=0.666;  rtsq[3]=0.804;
         rtsq[4]=0.081;  rtsq[5]=0.483;  rtsq[6]=0.919;  rtsq[7]=0.903; 
         rtsq[8]=0.951;  rtsq[9]=0.960; rtsq[10]=0.039; rtsq[11]=0.269; 
        rtsq[12]=0.270; rtsq[13]=0.756; rtsq[14]=0.222; rtsq[15]=0.478; 
        rtsq[16]=0.621; rtsq[17]=0.063; rtsq[18]=0.550; rtsq[19]=0.798;
        rtsq[20]=0.027; rtsq[21]=0.569; rtsq[22]=0.149; rtsq[23]=0.697; 
        rtsq[24]=0.451; rtsq[25]=0.738; rtsq[26]=0.508; rtsq[27]=0.041; 
        rtsq[28]=0.266; rtsq[29]=0.249; rtsq[30]=0.019; rtsq[31]=0.191; 
        rtsq[32]=0.266; rtsq[33]=0.625; rtsq[34]=0.492; rtsq[35]=0.940; 
        rtsq[36]=0.508; rtsq[37]=0.406; rtsq[38]=0.972; rtsq[39]=0.311;
        rtsq[40]=0.757; rtsq[41]=0.378; rtsq[42]=0.299; rtsq[43]=0.536; 
        rtsq[44]=0.619; rtsq[45]=0.844; rtsq[46]=0.342; rtsq[47]=0.295; 
        rtsq[48]=0.447; rtsq[49]=0.499; rtsq[50]=0.688; rtsq[51]=0.193; 
        rtsq[52]=0.225; rtsq[53]=0.520; rtsq[54]=0.954; rtsq[55]=0.749; 
        rtsq[56]=0.997; rtsq[57]=0.693; rtsq[58]=0.217; rtsq[59]=0.273;
        rtsq[60]=0.961; rtsq[61]=0.948; rtsq[62]=0.902; rtsq[63]=0.104; 
        rtsq[64]=0.495; rtsq[65]=0.257; rtsq[66]=0.524; rtsq[67]=0.100; 
        rtsq[68]=0.492; rtsq[69]=0.347; rtsq[70]=0.981; rtsq[71]=0.019; 
        rtsq[72]=0.225; rtsq[73]=0.806; rtsq[74]=0.678; rtsq[75]=0.710; 
        rtsq[76]=0.235; rtsq[77]=0.600; rtsq[78]=0.994; rtsq[79]=0.758;
        rtsq[80]=0.682; rtsq[81]=0.373; rtsq[82]=0.009; rtsq[83]=0.469; 
        rtsq[84]=0.203; rtsq[85]=0.730; rtsq[86]=0.588; rtsq[87]=0.603; 
        rtsq[88]=0.213; rtsq[89]=0.495; rtsq[90]=0.884; rtsq[91]=0.032; 
        rtsq[92]=0.185; rtsq[93]=0.127; rtsq[94]=0.010; rtsq[95]=0.180; 
        rtsq[96]=0.689; rtsq[97]=0.354; rtsq[98]=0.372; rtsq[99]=0.429;
       
        sequence = (sequence%100) + 1;
        return (rtsq[sequence-1]);
}


void Dfclrs()
{
/*
** Define the color table for coloring of vectors.
*/
	int i;			/* counter */

	Gcolr_rep color;	/* red, green, and blue color values */

/*
** Define a set of RGB color triples for colors 0 through 15.
*/
      	float rgbv[48];
/*
** Define the RGB color triples needed below.
*/
           rgbv[0]=0.00;   rgbv[1]=0.00;   rgbv[2]=0.00; 
           rgbv[3]=1.00;   rgbv[4]=1.00;   rgbv[5]=1.00; 
           rgbv[6]=0.70;   rgbv[7]=0.70;   rgbv[8]=0.70; 
           rgbv[9]=0.75;  rgbv[10]=0.50;  rgbv[11]=1.00; 
          rgbv[12]=0.50;  rgbv[13]=0.00;  rgbv[14]=1.00; 
          rgbv[15]=0.00;  rgbv[16]=0.00;  rgbv[17]=1.00; 
          rgbv[18]=0.00;  rgbv[19]=0.50;  rgbv[20]=1.00; 
          rgbv[21]=0.00;  rgbv[22]=1.00;  rgbv[23]=1.00; 
          rgbv[24]=0.00;  rgbv[25]=1.00;  rgbv[26]=0.60; 
          rgbv[27]=0.00;  rgbv[28]=1.00;  rgbv[29]=0.00; 
          rgbv[30]=0.70;  rgbv[31]=1.00;  rgbv[32]=0.00; 
          rgbv[33]=1.00;  rgbv[34]=1.00;  rgbv[35]=0.00; 
          rgbv[36]=1.00;  rgbv[37]=0.75;  rgbv[38]=0.00; 
          rgbv[39]=1.00;  rgbv[40]=0.38;  rgbv[41]=0.38; 
          rgbv[42]=1.00;  rgbv[43]=0.00;  rgbv[44]=0.38; 
          rgbv[45]=1.00;  rgbv[46]=0.00;  rgbv[47]=0.00; 
/*
** Define 16 different color indices, for indices 0 through 15.  The
** color corresponding to index 0 is black and the color corresponding
** to index 1 is white.
*/
	for(i=0; i<16; ++i)
	{
        	color.rgb.red = rgbv[(i*3)];
                color.rgb.green = rgbv[(i*3+1)];
                color.rgb.blue = rgbv[(i*3+2)];
   		gset_colr_rep(WKID, i, &color);
	}
}



void Genera (float* wrk_dat, float* ret_data, int cols, int rows)
{
/*
**     This subroutine generates a smooth array in output array ret_data.
**     The array is dependent on the random number function Randno.
**     Randno can be replaced by a random number generator on a
**     given machine, or for consistency across machines, the
**     supplied function Randno may be used.  Randno reads its random
**     numbers from the file ranfdat.
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


FILE* Openr ()
{
/*
** Open the $NCARG_ROOT/database file in read only mode
*/
	int i;			/* counter */
	int stat;		/* gngpat status */

	FILE* unit;		/* data file */

    	char *filenm2 = "database";	/* strings to create full path name */
    	char *filenm;
      	static int open;	/* file open flag */

	/*
	** Cray machine dependencies require
	** extra variables.
	*/
#if defined (cray)
    	int len;		/* path length */
    	_fcd cftfilenm;	/* full path */
    	_fcd cftfilenm2;	/* append file name */
#endif


      	if (open == 0)
	{
        	filenm = (char*)malloc(129*sizeof(char));
    		strcpy(filenm,"                                                                                                                                ");
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
/*
** Modification for UNIX Version.
*/
		fp = Openr();
/*
** End of modification for UNIX Version.
*/
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
