/*
** This program demonstrates the use of the new dashed-line package
** dashpack, which became a part of ncar graphics in august, 1994.
*/

#include <stdio.h>
#include <ncarg/ncargC.h>
#include <ncarg/gks.h>
#include <math.h>

/*
** Define min and max functions.
*/
#define min(x,y)   ((x) < (y) ? (x) : (y))
#define max(x,y)   ((x) < (y) ? (y) : (x))

/*
** Define fortran unit number, workstation type, and
** workstation id.
*/
#define lunit "gmeta"
#define iwtype SED_WSTYPE
#define iwkid 1


void main()
{
/*
** Counters.
*/
	int i,j,k,l;
	int iocl,nocl;
/*
** Character work arrays
*/
	char work[70];
	char work2[70];
/*
** Number of contour levels
*/
	float clev;
/*
** Generate data
*/
	extern void gendat();
/*
** Declare the routine which will draw contour lines but avoid drawing
** them through labels.
*/
	extern int NGCALLF(cpdrpl,CPDRPL)(float*,float*,int*,int*,int*,int*);
/*
** Declare an array to hold the data to be contoured.
*/
        float zdat[27][37];
/*
** Declare the required real and integer workspaces.
*/
        float rwrk[5000];
	int iwrk[1000];
/*
** Declare an array to hold an area map.
*/
        int iama[25000];
/*
** Declare a couple of character temporaries in which to manipulate the
** numeric labels for the contour lines.
*/
        char ctm1[70];
        char ctm2[139];
/*
** Open gks.
*/
        gopen_gks ("stdout",0);
        gopen_ws (iwkid, lunit, iwtype);
        gactivate_ws(iwkid);

/*
** Turn off clipping by gks.
*/
        gset_clip_ind (GIND_NO_CLIP);
/*
** Generate an array of test data.
*/
        gendat (&zdat[0][0],37,37,27,13,13,-512.148,489.834);
/*
** Change the range of the data somewhat.
*/
	for(i=0; i<37; ++i)
	{
		for(j=0; j<27; ++j)
		{
            		zdat[j][i]=zdat[j][i]+10000.;
		}
	}
/*
** Put explanatory labels at the top and bottom of the plot.
*/
        c_pchiqu (c_cfux(.5),c_cfuy(.930),"A CONPACK EXAMPLE",.018,0.,0.);
 
        c_pchiqu (c_cfux(.5),c_cfuy(.897),
                  "Using the New Dashed-Line Package DASHPACK",.015,0.,0.);
 
        c_pchiqu (c_cfux(.5),c_cfuy(.093),"Note that contour labels are written by PLCHHQ and that they can be made to :C:bend with the line (which works best when, as here, the lines are very smooth).", .010,0.,0.);

/*
** Tell conpack to choose contour levels for itself and to use more than
** the usual number of them.
*/
        c_cpseti ("CLS - contour level selector",32);
/*
** Tell conpack to use characters a little smaller than the default size
** in the contour-line labels.
*/
        c_cpsetr ("DPS - dash pattern character size",.0075);
/*
** Tell conpack to use the new dash package (because "dpu" is negative)
** and to use 1 repetition of the dash pattern between each line label
** and the next (because the absolute value of "dpu" is 1).
*/
        c_cpseti ("DPU - dash package use flag",-1);
/*
** Tell conpack to use gaps and solids that are half the default size
** when drawing the contour lines.
*/
        c_cpsetr ("DPV - dash pattern vector size",.0025);
/*
** Turn on the drawing of the high and low label boxes.
*/
        c_cpseti ("HLB - high/low label box flag",1);
/*
** Tell conpack to delete high/low labels which overlap the informational
** label, another high/low label, or the edge.
*/
        c_cpseti ("HLO - high/low label overlap flag",7);
/*
** Force the use of an exponent form in all numeric labels.
*/
        c_cpseti ("NEU - numeric exponent use flag",0);
/*
** Tell conpack to smooth the contour lines.
*/
        c_cpsetr ("T2D - tension on 2d splines",2.5);
/*
** Initialize the drawing of the contour plot.
*/
        c_cprect (&zdat[0][0],37,37,27,rwrk,5000,iwrk,1000);
/*
** Force the selection of contour levels and numeri** labels for them,
** so that the labels can be manipulated as required by dashpack.
*/
        c_cppklb (&zdat[0][0],rwrk,iwrk);
/*
** Find out how many levels were chosen and loop through all the levels.
*/
        c_cpgeti ("NCL - number of contour levels",&nocl);
	for(iocl=1; iocl<=nocl; ++iocl)
	{
/*
** Set "pai" to the index of the next level.
*/
          c_cpseti ("PAI - parameter array index",iocl);
/*
** Pick up the contour level value and use it to determine whether to
** use a dashed line, indicating a value below 10000, or a solid line,
** indicating a value above 10000.
*/
          c_cpgetr ("CLV - contour level",&clev);
 
          if (clev < 10000.)
	  {
            c_cpsetc ("CLD - contour level dash pattern",
                      "$_$_$_$_$_$_$_$_$_$_$_$_$_$_$");
	  }
          else
	  {
            c_cpsetc ("CLD - contour level dash pattern",
                      "$$$$$$$$$$$$$$$$$$$$$$$$$$$$$");
	  }
/*
** Retrieve the chosen numeric label for the level and preprocess it to
** include characters that tell dashpack where it is permitted to break
** the label.  (each piece is written with a separate call to plchhq and
** the overall effect is to make the label bend with the curve.)  This
** is done in such a way as to leave function codes with the characters
** they affect.  This code assumes that the default function-code control
** character (a colon) is being used by plotchar and that the default
** break character (a vertical bar) is being used by dashpack; it could
** be made more general by interrogating plotchar and dashpack to see
** what characters are really in use.
*/
          c_cpgetc ("LLT - line label text",ctm1,69);
 
          if (ctm1[0] != ' ')
	  {
/*
** j is the index of the last character examined in the input string, k
** is the index of the last character stored in the output string, and
** l is a flag indicating whether or not a "10:s:" has been seen in the
** input (so we can avoid putting break characters in the middle of the
** exponent).
*/
            j=0;
            k=0;
            l=0;
            strncpy(work,"",69);	
/*
** What follows is a simple loop to copy characters from the input
** string to the output string, inserting break characters where they
** are needed.
*/
 start:
 	    if (j <= 64)
	    {
              strncpy(work,&ctm1[j],5);	
              strncpy(work2,&ctm1[j],3);	
              if (strcmp(work,":L1:4")==0)
	      {
                strcpy(&ctm2[k],":L1:4|",6);
                j=j+5;
                k=k+6;
                goto start;
	      }	
              else if (strcmp(work,"10:S:")==0)
	      {	
                strncpy(&ctm2[k],"1|0:S:",6);
                j=j+5;
                k=k+6;
                l=1;
                goto start;
	      }
              else if (strcmp(work2,":N:")==0)
	      {
                strncpy(&ctm2[k],":N:",3);
                j=j+3;
                k=k+3;
	      }
              else if (ctm1[j] != ' ')
	      {
                ctm2[k] = ctm1[j];
                j=j+1;
                k=k+1;
                if ((l==0) && (ctm1[j] != ' '))
	        {
                  ctm2[k] = '|';
                  k=k+1;
	        }
                goto start;
	      }
	    }
/*
** Done - pass the string with break characters in it back to conpack.
*/
	    strncpy(work,ctm2,k);
            c_cpsetc ("LLT - line label text",work);
	  }

/*
** End of loop through contour levels.
*/
	}
/*
** Draw the default background.
*/
        c_cpback (&zdat[0][0],rwrk,iwrk);
/*
** Initialize the area map.
*/
        c_arinam (iama,25000);
/*
** Put label boxes into the area map.
*/
        c_cplbam (&zdat[0][0],rwrk,iwrk,iama);
/*
** Draw contour lines, omitting parts inside label boxes.
*/
        c_cpcldm (&zdat[0][0],rwrk,iwrk,iama,NGCALLF(cpdrpl,CPDRPL));
/*
** Add high, low, and informational labels.
*/
        c_cplbdr (&zdat[0][0],rwrk,iwrk);
/*
** Advance the frame.
*/
        c_frame();
/*
** Close gks.
*/
        gdeactivate_ws(iwkid);
        gclose_ws(iwkid);
        gclose_gks();
}


void gendat (data,idim,m,n,mlow,mhgh,dlow,dhgh)
float *data;
int idim;
int m; 
int n;
int mlow;
int mhgh;
float dlow;
float dhgh;
{
/*
** This is a routine to generate test data for two-dimensional graphics
** routines.  Given an array "DATA", dimensioned "IDIM x 1", it fills
** the sub-array ((DATA(I,J),I=1,M),J=1,N) with a two-dimensional field
** of data having approximately "MLOW" lows and "MHGH" highs, a minimum
** value of exactly "DLOW" and a maximum value of exactly "DHGH".
**
** "MLOW" and "MHGH" are each forced to be greater than or equal to 1
** and less than or equal to 25.
**
** The function used is a sum of exponentials.
*/
    float ccnt[3][50], fovm, fovn, dmin, dmax, temp;
    extern float fran();
    int nlow, nhgh, ncnt, i, j, k, ii;
    float fm, fn;

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
		fm = fovm*((float)(i)-ccnt[0][k-1]);
		fn = fovn*((float)(j)-ccnt[1][k-1]);
                temp = -( (fm*fm) + (fn*fn) );
                if (temp >= -20.){ 
                     data[ii]=data[ii]+.5*(dhgh-dlow)*ccnt[2][k-1]*exp(temp);
		}
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


float fran()
{
        static double x=2.718281828459045;
       	int i;
 
        x = (9821.0*x) + .2113270;
        i = x;
	x = x - i;
        return ((float)x);
}
