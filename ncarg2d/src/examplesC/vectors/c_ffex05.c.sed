/*
 *  $Id: c_ffex05.c.sed,v 1.1 1994-08-01 14:22:12 haley Exp $
 */
#include <stdio.h>
#include <math.h>

#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define WSTYPE SED_WSTYPE
#define WKID   1

/*
 * This program requires the input data file 'ffex05.dat'
 * It reads the data from standard input, e.g.: ffex05 < ffex05.dat
 */
#define MSIZE  73
#define NSIZE  73
#define MCRA   64
#define NOGI   64
#define NCLRS1 14
#define NCLRS2  7
#define NROWS  11

float u[NSIZE][MSIZE], v[NSIZE][MSIZE], p[NSIZE][MSIZE];

/*
 * Conpack work space and Areas area map
 */
#define ICPIWL   1000
#define ICPRWL   5000
#define IAREAL   20000

/*
 * Empirically determined vector thinning data
 * Starting from the pole, each row of vectors is weeded, mod the
 * value of this data.
 */
int ithin[NROWS] = {90,15,5,5,4,4,3,3,2,2,2};

/*
 * Indices used for vector coloring
 */
int iclr1[NCLRS1] = {10, 17, 24, 31, 38, 45, 52, 60, 67, 74, 81, 88, 95, 102};

/*
 * Indices used for coloring other plot features; be careful not
 * to duplicate any color indices in ICLR1
 */
int iclr2[NCLRS2] = {0, 1, 2, 149, 225, 175, 176};

main()
{
	float vl,vr,vb,vt,ul,ur,ub,ut,xdm;
	float vmn, vmx, dmx;
	float p1[2],p2[2],p3[2],p4[2];
	int i, j, idm, ll, icol, nclv;
	float rwrk[ICPRWL];
	int iwrk[ICPIWL], iam[IAREAL];
	extern void rddata(), setcgt(), setcla();
/*
 * Arrays for drawing masked grids
 */
	float xcra[MCRA],ycra[MCRA];
	int iaai[NOGI],iagi[NOGI];
/*
 * External subroutine declarations
 */
	extern int drawcl();
	extern int vvudmv_();
	Gcolr_rep rgb1[NCLRS1], rgb2[NCLRS2];
/*
 * Define a set of RGB color triples for vector colors
 */
	rgb1[0].rgb.red=0.0000000; rgb1[0].rgb.green=1.0000000; rgb1[0].rgb.blue=1.000000;
	rgb1[1].rgb.red=0.0745098; rgb1[1].rgb.green=0.9254900; rgb1[1].rgb.blue=0.925490;
	rgb1[2].rgb.red=0.1529410; rgb1[2].rgb.green=0.8470590; rgb1[2].rgb.blue=0.847059;
	rgb1[3].rgb.red=0.2313730; rgb1[3].rgb.green=0.7686270; rgb1[3].rgb.blue=0.768627;
	rgb1[4].rgb.red=0.3058820; rgb1[4].rgb.green=0.6941180; rgb1[4].rgb.blue=0.694118;
	rgb1[5].rgb.red=0.3843140; rgb1[5].rgb.green=0.6156860; rgb1[5].rgb.blue=0.615686;
	rgb1[6].rgb.red=0.4627450; rgb1[6].rgb.green=0.5372550; rgb1[6].rgb.blue=0.537255;
	rgb1[7].rgb.red=0.5372550; rgb1[7].rgb.green=0.4627450; rgb1[7].rgb.blue=0.462745;
	rgb1[8].rgb.red=0.6156860; rgb1[8].rgb.green=0.3843140; rgb1[8].rgb.blue=0.384314;
	rgb1[9].rgb.red=0.6941180; rgb1[9].rgb.green=0.3058820; rgb1[9].rgb.blue=0.305882;
	rgb1[10].rgb.red=0.7686270;rgb1[10].rgb.green=0.2313730;rgb1[10].rgb.blue=0.231370;
	rgb1[11].rgb.red=0.8470590;rgb1[11].rgb.green=0.1529410;rgb1[11].rgb.blue=0.152941;
	rgb1[12].rgb.red=0.9254900;rgb1[12].rgb.green=0.0745098;rgb1[12].rgb.blue=0.074509;
	rgb1[13].rgb.red=1.0000000;rgb1[13].rgb.green=0.0000000;rgb1[13].rgb.blue=0.000000;
/*
 * RGB values for other plot features
 */
	rgb2[0].rgb.red=1.0;rgb2[0].rgb.green=1.0;rgb2[0].rgb.blue=1.0;
	rgb2[1].rgb.red=0.0;rgb2[1].rgb.green=0.0;rgb2[1].rgb.blue=0.0;
	rgb2[2].rgb.red=0.9;rgb2[2].rgb.green=0.9;rgb2[2].rgb.blue=0.9;
	rgb2[3].rgb.red=0.6;rgb2[3].rgb.green=0.6;rgb2[3].rgb.blue=0.6;
	rgb2[4].rgb.red=0.3;rgb2[4].rgb.green=0.3;rgb2[4].rgb.blue=0.3;
	rgb2[5].rgb.red=0.8;rgb2[5].rgb.green=0.9;rgb2[5].rgb.blue=1.0;
	rgb2[6].rgb.red=0.25;rgb2[6].rgb.green=0.0;rgb2[6].rgb.blue=0.5;
/*
 * -----------------------------------------------------------------
 *
 *  Open GKS, open and activate a workstation.
 */
    gopen_gks("stdout",0);
    gopen_ws(WKID, NULL, WSTYPE);
    gactivate_ws(WKID);
/*
 * Set up auxiliary colors
 */
	for( i = 0; i < NCLRS2; i++ ) {
		gset_colr_rep(WKID,iclr2[i],&rgb2[i]);
	}
/*
 * Read the input array data
 */
	rddata();
/*
 * Message the data to eliminate surplus of vectors near the pole
 */
	for( j=NSIZE; j >= NSIZE-NROWS+1; j-- ) {
		for( i=1; i <= MSIZE; i++ ) {
            if ( (i % ithin[NSIZE-j]) != 0) {  
				u[j-1][i-1] = -9999.0;
			}
		}
	}
/*
 * Set up the EZMAP projection
 */         
    p1[0] = 20.;
    p2[0] = -180.;
    p3[0] = 20.;
    p4[0] = 0.;
	c_mapset("CO",p1,p2,p3,p4);
	c_maproj("ST",90.0,180.0,45.0);
	c_mapint();
/*
 * Tell Vectors to use the mapping established by EZMAP
*/
	c_vvseti("MAP -- Mapping Flag", 1);
	c_vvseti("SET -- Set Call Flag", 0);
/*
 * Set up data coordinate boundaries and special value processing 
 * appropriately for the dataset 
 */
	c_vvsetr("XC1 -- Lower X Bound", -180.0);
	c_vvsetr("XCM -- Upper X Bound", 180.0);
	c_vvsetr("YC1 -- Lower X Bound", -90.0);
	c_vvsetr("YCN -- Upper Y Bound", 90.0);

	c_vvseti("SVF -- Special Values Flag", 3);
	c_vvsetr("USV -- U Special Value", -9999.0);
	c_vvsetr("VSV -- V Special Value", -9999.0);
	c_vvsetr("PSV - P Special Value", -9999.0);
	c_vvseti("SPC - P Special Color", 1);
/*
 * Do the equivalent Conpack setup; note that the special value
 * parameter works a bit differently, and also Conpack requires
 * an Out of Range value to be set whenever the data grid extends
 * outside the map boundaries. The standard value for the default
 * version of CPMPXY is 1.0E12
 */
	c_cpseti("MAP -- Mapping Flag", 1);
	c_cpseti("SET -- Set Call Flag", 0);
	c_cpsetr("XC1 -- Lower X Bound", -180.0);
	c_cpsetr("XCM -- Upper X Bound", 180.0);
	c_cpsetr("YC1 -- Lower X Bound", -90.0);
	c_cpsetr("YCN -- Upper Y Bound", 90.0);
	c_cpsetr("SPV -- Special Value",-9999.0);
	c_cpsetr("ORV -- Out of Range Value",1.0e12);
/*
 * Set Conpack graphics text parameters
 */
	setcgt();
/*
 * Turn on statistics reporting
 */
	c_vvseti("VST -- Vector Statistics", 1);
/*
 * Initialize the drawing of the contour plot, and tell Conpack
 * to pick contour levels.
 */
	c_cprect ((float *)p,MSIZE,MSIZE,NSIZE,rwrk,ICPRWL,iwrk,ICPIWL);
	c_cppkcl ((float *)p,rwrk,iwrk);
/*
 * Set up contour line attributes
 */
	c_cpgeti("NCL - Number Of Contour Levels",&nclv);
	setcla(nclv, iclr2[5],iclr2[2]);
/*
 * Initialize the area map, and add the Conpack labels to the area map
 */
	c_arinam (iam,IAREAL);
	c_cplbam ((float *)p,rwrk,iwrk,iam);
/*
 * Set up vector color processing
 */
	c_vvseti("MSK -- Vector Masking",1);
	c_vvseti("CTV -- Color Thresholds Value", 2);
	c_vvseti("NLV -- Number Of Levels", NCLRS1);
	for( i = 1; i <= NCLRS1; i++ ) {
		gset_colr_rep(WKID,iclr1[i-1],&rgb1[i-1]);
		c_vvseti("PAI -- Parameter Array Index", i);
		c_vvseti("CLR -- GKS Color Index", iclr1[i-1]);
	}
	for( i = 1; i <= 6; i++ ) {
/*
 * For the last frame modify the color table for a black background
 * and modify the contour attributes
 */
		if (i==6) {
            gset_colr_rep(WKID,iclr2[0],&rgb2[1]);
            gset_colr_rep(WKID,iclr2[1],&rgb2[0]);
            setcla(nclv, iclr2[6],iclr2[2]);
		}
/*
 * Draw the masked contour lines
 */
		if (i==3 || i>=5) {
            c_cpcldm ((float *)p,rwrk,iwrk,iam,drawcl);
		}
/*
 * Draw the continental outline with a wide line
 */
		if (i==2 || i>=5) {
            icol=iclr2[2];
            if (i>5) icol=iclr2[4];
            c_mapsti("c5 - continental outline color",icol);
            gset_linewidth(8.0);
            c_maplot();
		}
		gset_linewidth(1.0);
/*
 * Draw the map grid
 */
		if (i==1 || i>=5) {
			c_mapsti("C2 - GRID",iclr2[3]);
            gset_line_colr_ind(iclr2[1]);
            c_mapgrm (iam,xcra,ycra,MCRA,iaai,iagi,NOGI,drawcl);
            gset_line_colr_ind(1);
		}
		if (i==4 || i>=5) {
/*
 * Initialize vectors
 */
            c_vvinit ((float *)u,MSIZE,(float *)v,MSIZE,(float *)p,MSIZE,MSIZE,NSIZE,&xdm,idm);
/*
 * Adjust vector rendering options
 */
            c_vvsetr("AMN -- Arrow Minimum Size",0.007);
            c_vvsetr("LWD -- Vector Line Width",1.75);
            c_vvgetr("VMN -- Minimum Vector",&vmn);
            c_vvgetr("VMX -- Maximum Vector",&vmx);
            c_vvsetr("VLC -- Vector Low Cutoff",vmn+0.1*(vmx-vmn));
            c_vvgetr("DMX -- Device Maximum Vector Length",&dmx);
            c_getset(&vl,&vr,&vb,&vt,&ul,&ur,&ub,&ut,&ll);
            c_vvsetr("VRL - Vector Realized Length",4.0*dmx/(vr-vl));
            c_vvsetr("VFR -- Vector Fractional Minimum",0.4);
/*
 * Draw the vector field plot
 */
            c_vvectr ((float *)u,(float *)v,(float *)p,iam,vvudmv_,&xdm);
		}
/*
 * Draw continental outlines again with thin line and brighter
 */
		if (i==2 || i>=5) {
            c_mapsti("c5 - Continental Outline Color",iclr2[1]);
            c_maplot();
		}
/*
 * Draw labels last
 */
		if (i==3 || i>=5) {
            if (i>5) {
				c_cpseti("HLB",1);
				gset_fill_int_style(0);
			}
            else {
				c_cpseti("HLB",3);
				gset_fill_int_style(1);
            }
            c_cplbdr ((float *)p,rwrk,iwrk);
		}
/*
 * Draw a perimeter boundary and eject the frame
 */
		c_perim(1,0,1,0);
		c_frame();
	}
/*
 *     Deactivate and close workstation, close GKS.
 */
    gdeactivate_ws (WKID);
    gclose_ws(WKID);
    gclose_gks();
}

void rddata()
{
	char ch;
	int i, j;
/*
 * Read the data arrays from the standard input 
 */
	for( j = 0; j < NSIZE; j++ ) {
		for( i = 0; i < MSIZE; i++ ) {
			fscanf( stdin, "%g", &u[j][i]);
			fscanf( stdin, "%c", &ch );
		}
	}
	for( j = 0; j < NSIZE; j++ ) {
		for( i = 0; i < MSIZE; i++ ) {
			fscanf( stdin, "%g", &v[j][i]);
			fscanf( stdin, "%c", &ch );
		}
	}
	for( j = 0; j < NSIZE; j++ ) {
		for( i = 0; i < MSIZE; i++ ) {
			fscanf( stdin, "%g", &p[j][i]);
			fscanf( stdin, "%c", &ch );
		}
	}
	return;
}

int drawcl (xcs,ycs,ncs,iai,iag,nai)
float *xcs, *ycs;
int *ncs, *iai, *iag, *nai;
{
/*
 * Routine for masked drawing of contour and grid lines
 *
 * This version of DRAWCL draws the polyline defined by the points
 * ((XCS(I),YCS(I)),I=1,NCS) if and only if none of the area identifiers
 * for the area containing the polyline are negative.  The dash package
 * routine CURVED is called to do the drawing.
 *
 * Turn on drawing.
 */
	int i, idr;

	idr=1;
/*
 * If any area identifier is negative, turn off drawing.
 */
	for( i = 0; i < *nai; i++ ) {
		if (iai[i] < 0) idr=0;
	}
/*
 * If drawing is turned on, draw the polyline.
 */
	if (idr != 0) c_curved (xcs,ycs,*ncs);
/*
 * Done.
 */
	return(0);
}

void setcgt()
{
/*
 * Sets Conpack Graphics Text Parameters
 */
	c_cpseti ("LLP - LINE LABEL POSITIONING",3);
	c_cpseti ("RWC - REAL WORKSPACE FOR CONTOURS",200);
	c_cpseti ("HLB - HIGH/LOW LABEL BOX FLAG",1);
	c_cpseti ("LLB - HIGH/LOW LABEL BOX FLAG",0);
	c_cpsetc ("ILT - INFORMATION LABEL TEXT"," ");
	c_cpseti ("HLO - HIGH/LOW LABEL OVERLAP FLAG",11);
	c_cpsetr ("CWM - CHARACTER WIDTH MULTIPLIER",1.25);
	
	return;
}


void setcla (nclv, iclclr, iaxclr)
int nclv, iclclr, iaxclr;
{
/*
 * Sets Contour Line Attributes
 */
	int iclu, iclv;

	for( iclv=1; iclv <= nclv;  iclv++ ) {
		
		c_cpseti ("PAI - PARAMETER ARRAY INDEX",iclv);
		c_cpgeti ("CLU - CONTOUR LEVEL USE FLAG",&iclu);
		c_cpseti ("CLL - CONTOUR-LINE LINE WIDTH",3);
		c_cpseti ("CLC - CONTOUR-LINE COLOR", iclclr);
		if (iclu==3) {
            c_cpseti ("CLL - CONTOUR-LINE LINE WIDTH",6);
		}
	}
/*
 * "Special" contour lines - grid, special values, and out of range
 * boundaries 
 */
	c_cpseti ("PAI - PARAMETER ARRAY INDEX",-1);
	c_cpseti ("CLU - CONTOUR LEVEL USE FLAG",0);
	c_cpsetr ("CLL - CONTOUR LEVEL LINE WIDTH",2.);
	c_cpseti ("CLC - CONTOUR LEVEL LINE COLOR",iaxclr);

	c_cpseti ("PAI - PARAMETER ARRAY INDEX",-2);
	c_cpseti ("CLU - CONTOUR LEVEL USE FLAG",1);
	c_cpsetr ("CLL - CONTOUR LEVEL LINE WIDTH",2.);
	c_cpseti ("CLC - CONTOUR LEVEL LINE COLOR",iaxclr);

	c_cpseti ("PAI - PARAMETER ARRAY INDEX",-3);
	c_cpseti ("CLU - CONTOUR LEVEL USE FLAG",1);
	c_cpsetr ("CLL - CONTOUR LEVEL LINE WIDTH",2.);
	c_cpseti ("CLC - CONTOUR LEVEL LINE COLOR",iaxclr);

	return;
}
