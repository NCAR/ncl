/*
 *  $Id: c_vvex01.c,v 1.1 1994-10-31 04:16:59 haley Exp $
 */
#include <stdio.h>
#include <math.h>

#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define WSTYPE SED_WSTYPE
#define WKID   1

#define min(x,y)   ((x) < (y) ? (x) : (y))
#define max(x,y)   ((x) > (y) ? (x) : (y))
#define pow2(x)   ((x)*(x))

/*
 * This example overlays vectors on a polar contour plot using 
 * data generated with a randomizing algorithm. The first frame colors
 * the vectors according to the data used to generate the contour plot,
 * with the result that the color of the vectors corresponds to the 
 * contour level at each location. In the second frame the vectors 
 * are colored by magnitude.
 *
 * Define error file, Fortran unit number, and workstation type,
 * and workstation ID.
 */
#define MSIZE  33
#define NSIZE  33

float u[60][60], v[60][60];

main()
{
	int i, j, k;
	float vl,vr,vb,vt,ul,ur,ub,ut;
	int ll, iclu, iclv, nclu, nclv;
	float vmn, vmx, dmx, vrl;
/* 
 * The contour, vector field component, and area map array declarations:
 */
	float zdat[NSIZE][MSIZE];
	int iama[25000];
	extern void gendat(), genara(), dfclrs(), bndary();
/*
 * Workspace arrays for Conpack:
 */
	float rwrk[5000];
	int iwrk[1000];
/*
 * ARSCAM arrays:
 */
	float xcra[1000],ycra[1000];
	int iara[10],igra[10];
/*
 * Declare the masked rendering routines for drawing and shading the
 * contour plot, as well as for drawing the vectors
 */
	extern int drawcl();
	extern int shader();
	extern int NGCALLF(vvudmv,VVUDMV)();
/*
 * Initialization
 * ==================================================================
 * Open GKS.
 */
    gopen_gks("stdout",0);
    gopen_ws(WKID, NULL, WSTYPE);
    gactivate_ws(WKID);
	gset_clip_ind (GIND_NO_CLIP);
/*
 * Generate a scalar data array, then create a special value region
 */
	gendat (zdat,MSIZE,MSIZE,NSIZE,20,20,-1.,1.);

	for( i = 1; i <= MSIZE; i++ ) {
		for( j = 1; j <= NSIZE; j++ ) {
            if (pow((float)i-20.,2)+pow((float)j-10.,2) < 25) zdat[j-1][i-1]=1.e36;
		}
	}
/*
 * Subroutine GENARA generates smoothly varying random data in its 
 * second array argument based upon the contents of the first. Call it
 * twice to randomize both the U and V vector component data arrays.
 * Then set up the color table.
 */
    genara(60,60);
    genara(60,60);
    dfclrs();
/*
 * Conpack setup:
 * ===============================================================
 * Set up a polar coordinate system mapping for Conpack
 */
    c_set    (0.05,0.95,0.05,0.95,-1.0,1.0,-1.0,1.0,1);
    c_cpseti ("MAP - Mapping Function",2);
    c_cpseti ("SET - Do-Set-Call Flag",0);
    c_cpsetr ("XC1 - Rho At I = 1",.1);
    c_cpsetr ("XCM - Rho At I = M",1.);
    c_cpsetr ("YC1 - Theta At J = 1",0.0);
    c_cpsetr ("YCN - Theta At J = N",360.0);
/*
 * Enable special value processing and outline special value regions
 */
	c_cpsetr ("SPV - Special Value",1.e36);
	c_cpseti ("PAI - Parameter Array Index",-2);
	c_cpseti ("CLU - Contour Level Use Flag",1);
	c_cpsetr ("CLL - Contour Level Line Width",2.);
/*
 * Adjust Conpack labelling and outline the data field.
 */
	c_cpseti ("LLP - Line Label Positioning",3);
	c_cpseti ("RWC - Real Workspace For Contours",200);
	c_cpseti ("HLB - High/Low Label Box Flag",1);
	c_cpseti ("HLO - High/Low Label Overlap Flag",11);
	c_cpsetr ("CWM - Character Width Multiplier",1.25);
	c_cpseti ("PAI - Parameter Array Index",-1);
	c_cpseti ("CLU - Contour Level Use Flag",1);
	c_cpsetr ("CLL - Contour Level Line Width",2.);
/*
 * Initialize the drawing of the contour plot, and tell Conpack 
 * to pick the contour levels.
 */
	c_cprect ((float *)zdat,MSIZE,MSIZE,NSIZE,rwrk,5000,iwrk,1000);
	c_cppkcl ((float *)zdat,rwrk,iwrk);
/*
 * Set the attributes of the contour lines
 */
	c_cpgeti ("NCL - Number Of Contour Levels",&nclv);
	for( iclv = 1; iclv <= nclv; iclv++ ) {
		c_cpseti ("PAI - Parameter Array Index",iclv);
		c_cpgeti ("CLU - Contour Level Use Flag",&iclu);
		if (iclu==3) {
            c_cpseti ("CLL - Contour-Line Line Width",2);
		}
		c_cpseti ("AIA - Area Identifier Above Level",0);
		c_cpseti ("AIB - Area Identifier Below Level",0);
	}
/*
 * Add two new levels for which no contour lines are to be drawn, but
 * between which shading is to be done.
 */
	nclv=nclv+2;
	c_cpseti ("NCL - Number Of Contour Levels",nclv);
	c_cpseti ("PAI - Parameter Array Index",nclv-1);
	c_cpsetr ("CLV - Contour Level Value",-.15);
	c_cpseti ("CLU - Contour Level Use Flag",0);
	c_cpseti ("AIA - Area Identifier Above Level",1);
	c_cpseti ("AIB - Area Identifier Below Level",2);
	c_cpseti ("PAI - Parameter Array Index",nclv);
	c_cpsetr ("CLV - Contour Level Value", .15);
	c_cpseti ("CLU - Contour Level Use Flag",0);
	c_cpseti ("AIA - Area Identifier Above Level",3);
	c_cpseti ("AIB - Area Identifier Below Level",1);
/*
 * Initialize the area map and draw the contour labels into it.
 */
	c_arinam (iama,25000);
	c_cplbam ((float *)zdat,rwrk,iwrk,iama);
/*
 * Vectors setup:
 * ==================================================================
 * Set the mapping flag for the polar transformation.
 * Set the X,Y array index mapping parameters to the same values used
 * bythe Conpack CPSET routines above. Turn on masking and turn the
 * Set call flag off.
 */
	c_vvseti("MAP -- Mapping Flag", 2);
	c_vvseti("SET -- Set Call Flag", 0);
	c_vvsetr("XC1 -- Lower X Bound", 0.1);
	c_vvsetr("XCM -- Upper X Bound", 1.0);
	c_vvsetr("YC1 -- Lower X Bound", 0.0);
	c_vvsetr("YCN -- Upper Y Bound", 360.0);
	c_vvseti("MSK -- Area Mask Flag", 1);
/*
 * Enable special value processing for the P array to eliminate
 * vectors from the special value region. This is not really required
 * in this case since the masking routine eliminates these vectors.
 */
	c_vvseti("SPC -- P Special Value Color", 0);
	c_vvsetr("PSV -- P Special Value", 1.e36);
/*
 * Enable vector coloring
 */
	c_vvseti("NLV - Number of Levels", 14);
	for( i = 1; i <= 14; i++ ) {
		c_vvseti("PAI -- Parameter Array Index", i);
		c_vvseti("CLR -- GKS Color Index", i+1);
	}
/*
 * Set up miscellaneous attribute parameters
 */
	c_vvsetr("LWD -- Vector Linewidth", 2.0);
	c_vvsetr("AMN -- Arrow Minimum Size", 0.01);
	c_vvseti("VPO -- Vector Position Method", 0);
	c_vvseti("XIN - X Grid Increment", 2);
/*     
 * Move the minimum and maximum vector text blocks out of the
 * way of the text that Conpack puts out.
 */
	c_vvsetr("MNX - Minimum Vector X Pos", 0.0);
	c_vvsetr("MXX - Maximum Vector X Pos", 0.33);
	c_vvseti("MNP - Minimum Vector Justification", 2);
	c_vvseti("MXP - Maximum Vector Justification", 4);
/*
 * Turn on statistics
 */
	c_vvseti("VST - Vector statistics", 1);
/*
 * Drawing loop
 * ===================================================================
 * Draw the contour plot with vectors overlaid twice. In the first
 * plot Vectors uses the same data as Conpack for the independent
 * scalar array. Therefore the colors of the vectors correspond to the
 * contours. The second plot shows the vectors colored by magnitude.
 */
	for( k = 1; k <= 2; k++ ) {
/*
 * First draw masked contour lines, then labels, then put the
 * contour lines in the area map for shading by ARSCAM
 */
		c_cpcldm ((float *)zdat,rwrk,iwrk,iama,drawcl);
		c_cplbdr ((float *)zdat,rwrk,iwrk);
		c_cpclam ((float *)zdat,rwrk,iwrk,iama);
		c_arscam (iama,xcra,ycra,1000,iara,igra,10,shader);
/*
 * Choose between vector magnitude and scalar array coloring
 */
		if (k == 1) {
            c_vvseti("CTV -- Color Threshold Value", 2);
		}
		else {
            c_vvseti("CTV -- Color Threshold Value", 1);
		}
/*
 * Initialize Vectors
 */
		c_vvinit ((float *)u,60,(float *)v,60,(float *)zdat,MSIZE,MSIZE,NSIZE,0,0);
/*
 * Remove the bottom 05% of the vectors
 */
		c_vvgetr("VMX -- Max Vector Magnitude",&vmx);
		c_vvgetr("VMN -- Min Vector Magnitude",&vmn);;
		c_vvsetr("VLC -- Vector Low Cutoff", vmn+0.05*(vmx-vmn));
/*
 * Increase the size of the longest vector by 50% from its default
 * value and make the shortest one fourth the length of the longest.
 */
		c_vvgetr("DMX -- Max Vector Device Magnitude",&dmx);
		c_getset(&vl,&vr,&vb,&vt,&ul,&ur,&ub,&ut,&ll);
		vrl = 1.5 * dmx / (vr - vl);
		c_vvsetr("VRL - Vector Realized Length", vrl);
		c_vvsetr("VFR -- Vector Fractional Minimum", 0.25);
/*
 * Call VVECTR to draw the vectors, using the same area map that
 * the Conpack routines used. The "Draw Masked Vector" routine 
 * used is the one supplied with the Velocity Vector Utility.
 */
		c_vvectr ((float *)u,(float *)v,(float *)zdat,iama,NGCALLF(vvudmv,VVUDMV),0);
/*
 * Put a boundary line at the edge of the plotter frame.
 */
		bndary();
/*
 * Advance the frame.
 */     
		c_frame();
	}
/*     
 * Close GKS.
 */
    gdeactivate_ws (WKID);
    gclose_ws(WKID);
    gclose_gks();
}


int drawcl (xcs,ycs,ncs,iai,iag,nai)
float *xcs, *ycs;
int *ncs, *iai, *iag, *nai;
{
/*
 * Routine for masked drawing of contour and grid lines
 *
 * This version of drawcl draws the polyline defined by the points
 * ((xcs(i),ycs(i)),i=1,ncs) if and only if none of the area identifiers
 * for the area containing the polyline are negative.  The dash package
 * routine curved is called to do the drawing.
 *
 * Turn on drawing.
 */
	int i, idr;

	idr = 1;
/*
 * If any area identifier is negative, turn off drawing.
 */
	for( i = 0; i < *nai; i++ ) {
		if (iai[i] < 0) idr=0;
	}
/*
 * If drawing is turned on, draw the polyline.
 */
	if (idr) c_curved (xcs,ycs,*ncs);
/*
 * Done.
 */
	return(0);
}

int  shader (xcs,ycs,ncs,iai,iag,nai)
float *xcs, *ycs;
int *ncs, *iai, *iag, *nai;
{
/*
 * This version of SHADER shades the polygon whose edge is defined by
 * the points ((XCS(I),YCS(I)),I=1,NCS) if and only, relative to edge
 * group 3, its area identifier is a 1.  The package SOFTFILL is used
 * to do the shading.
 */
	int ish, i;
/*
 * Define workspaces for the shading routine.
 */
	float dst[1100];
	int ind[1200];
/*
 * Turn off shading.
 */
	ish=0;
/*
 * If the area identifier for group 3 is a 1, turn on shading.
 */
	for( i = 0; i < *nai; i++ ) {
		if (iag[i] == 3 && iai[i] == 1) ish=1;
	}
/*
 * If shading is turned on, shade the area.  The last point of the
 * edge is redundant and may be omitted.
 */
	if (ish != 0) {
		c_sfseti ("ANGLE",45);
		c_sfsetr ("SPACING",.006);
		c_sfwrld (xcs,ycs,*ncs-1,dst,1100,ind,1200);
		c_sfseti ("ANGLE",135);
		c_sfnorm (xcs,ycs,*ncs-1,dst,1100,ind,1200);
	}
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

#define PI     3.14159 
#define TWOPI  2.*PI 
#define EPS    PI/6.

void genara (id,jd)
int id, jd;
{
/*
 *  This subroutine generates a smooth array in output array b.
 *  the array is dependent on the random number function randno.
 *  Randno can be replaced by a random number generator on a
 *  given machine, or for consistency across machines, the
 *  supplied function randno may be used.  Randno reads its random
 *  numbers from the file ranfdat.
 */
	float aa, ee;
	int i, j, ie, je, cie, ii, jj,  k, di, dj, nn;
	int jm1, jp1, im1, ip1;
    extern float randno();

	nn=(id+jd)/10;
	aa=1.;
	di=id-4;
	dj=jd-4;

two:
    for( k = 1; k <= nn; k++ ) {
		ii = 3.+di*randno();
		jj = 3.+dj*randno();
		for( j = 1; j <= jd; j++ ) {
			je = abs(j-jj);
			for( i = 1; i <= id; i++ ) {
				ie = abs(i-ii);
				ee = max(ie,je);
				u[j-1][i-1] = u[j-1][i-1]+aa*(pow(.8,ee));
			}
		}
	}

	if (aa != 1.) goto six;
	aa = -1.;
	goto two;

six:
	for( j = 1; j <= jd; j++ ) {
		jm1 = max(1,j-1);
		jp1 = min(jd,j+1);
		for( i = 1; i <= id; i++ ) {
			im1 = max(1,i-1);
			ip1 = min(id,i+1);
			v[j-1][i-1] = (4.*u[j-1][i-1]+2.*(u[jm1-1][i-1]+u[j-1][im1-1]+u[j-1][ip1-1]+u[jp1-1][i-1])+u[jm1-1][im1-1]+u[jm1-1][ip1-1]+u[jp1-1][im1-1]+u[jp1-1][ip1-1])/16.;
		}
	}
	return;
}

FILE *fp;

void openr()
{
    char *filenm;
    char *filenm2 = "database";
    static int iopen = 0;
    int i, istat;
/*
 * Cray machine dependencies require
 * extra variables.
*/
#if defined (cray)
    int len;		/* path length */
    _fcd cftfilenm;		/* full path */
    _fcd cftfilenm2;	/* append file name */
#endif

    if (!iopen) {
       	filenm = (char*)malloc(129*sizeof(char));
    	strcpy(filenm,"                                                                                                                                ");
#if !defined (cray)
        NGCALLF(gngpat,GNGPAT)(filenm,filenm2,&istat,119,9);
#else
      	cftfilenm = _cptofcd(filenm, strlen(filenm));
       	cftfilenm2 = _cptofcd(filenm2, strlen(filenm2));
       	NGCALLF(gngpat,GNGPAT)(cftfilenm,cftfilenm2,&istat);
       	len = _fcdlen(cftfilenm);
       	filenm = (char*)malloc(len*sizeof(char));
       	filenm = _fcdtocp(cftfilenm);
#endif
  		if( istat != -1 ) {
	        for( i = 0; i < 119; i++ ) {
    	        if (filenm[i] == '\0') {
        	        strcat(&filenm[i], "/ranfdata" );
            	    break;
	            }	
	        }	
			fp = fopen( filenm, "r" );
		}
	}
	return;
}


float randno()
{
/*
 *     This function is used to produce random numbers for the
 *     GENARA calls.  The random numbers are read from file
 *     RANFDAT, and in this way consistency is maintained across
 *     machines (the results on one machine should be the same
 *     as on another.)  If consistency is not required, the
 *     function RANDNO can be replaced by a local random number
 *     generator.  RANFDAT contains 2000 random numbers in
 *     250 card-image lines in format 8F10.8 .
 */
	static icnt = 0;
	static float a[2000];
	extern void openr();
	int i, j, indx, jndx;

	icnt = icnt+1;
/*
 * Read in random numbers if this is the first function call.
 */
	if (icnt == 1) {
/*
 * Modification for UNIX Version.
 */
		openr(3);
/*
 * End of modification for UNIX Version.
 */
        for( i=1; i <= 250; i++ ) {
			indx = 8*(i-1)+1;
			jndx = indx+7;
			for( j = indx; j <= jndx; j++ ) {
				fscanf( fp, "%f10.8", &a[j-1] );
			}
		}
	}
	return(a[icnt-1]);
}

#define NCLRS  16

void dfclrs()
{
	Gcolr_rep rgb[NCLRS];
	int i;
/*
 * Define the RGB color triples needed below.
 */
	rgb[0].rgb.red = 0.00; rgb[0].rgb.green = 0.00; rgb[0].rgb.blue = 0.00;
	rgb[1].rgb.red = 1.00; rgb[1].rgb.green = 1.00; rgb[1].rgb.blue = 1.00;
	rgb[2].rgb.red = 0.70; rgb[2].rgb.green = 0.70; rgb[2].rgb.blue = 0.70;
	rgb[3].rgb.red = 0.75; rgb[3].rgb.green = 0.50; rgb[3].rgb.blue = 1.00;
	rgb[4].rgb.red = 0.50; rgb[4].rgb.green = 0.00; rgb[4].rgb.blue = 1.00;
	rgb[5].rgb.red = 0.00; rgb[5].rgb.green = 0.00; rgb[5].rgb.blue = 1.00;
	rgb[6].rgb.red = 0.00; rgb[6].rgb.green = 0.50; rgb[6].rgb.blue = 1.00;
	rgb[7].rgb.red = 0.00; rgb[7].rgb.green = 1.00; rgb[7].rgb.blue = 1.00;
	rgb[8].rgb.red = 0.00; rgb[8].rgb.green = 1.00; rgb[8].rgb.blue = 0.60;
	rgb[9].rgb.red = 0.00; rgb[9].rgb.green = 1.00; rgb[9].rgb.blue = 0.00;
	rgb[10].rgb.red = 0.70;rgb[10].rgb.green = 1.00;rgb[10].rgb.blue = 0.00;
	rgb[11].rgb.red = 1.00;rgb[11].rgb.green = 1.00;rgb[11].rgb.blue = 0.00;
	rgb[12].rgb.red = 1.00;rgb[12].rgb.green = 0.75;rgb[12].rgb.blue = 0.00;
	rgb[13].rgb.red = 1.00;rgb[13].rgb.green = 0.38;rgb[13].rgb.blue = 0.38;
	rgb[14].rgb.red = 1.00;rgb[14].rgb.green = 0.00;rgb[14].rgb.blue = 0.38;
	rgb[15].rgb.red = 1.00;rgb[15].rgb.green = 0.00;rgb[15].rgb.blue = 0.00;
/*
 * Define 16 different color indices, for indices 0 through 15.  The
 * color corresponding to index 0 is black and the color corresponding
 * to index 1 is white.
 */
	for( i = 0; i < NCLRS; i++ ) {
		gset_colr_rep(WKID,i,&rgb[i]);
	}
/*
 * Done.
 */
	return;
}

void bndary()
{
/*
 * Draw a line showing where the edge of the plotter frame is.
 */
	c_plotif (0.,0.,0);
	c_plotif (1.,0.,1);
	c_plotif (1.,1.,1);
	c_plotif (0.,1.,1);
	c_plotif (0.,0.,1);
	c_plotif (0.,0.,2);
	
	return;
}

