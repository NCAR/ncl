/*
 *  $Id: c_ffex03.c,v 1.4 1997-04-21 14:38:40 haley Exp $
 */
#include <stdio.h>
#include <math.h>

#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define IWTYPE 1
#define WKID   1

#define MSIZE   36
#define NSIZE   33
#define ISTWKL  2*MSIZE*NSIZE
#define ICPIWL  1000
#define ICPRWL  5000
#define IAREAL  20000
#define MCRA    64
#define NOGI    64
#define NCLRS   3

/*
 * Define a set of rgb color triples
 */
int iclr[NCLRS];

float u[NSIZE][MSIZE], v[NSIZE][MSIZE], p[NSIZE][MSIZE];

main()
{
	int i, iclrix, nclv, idm=0;
	float xdm;
    float p1[2],p2[2],p3[2],p4[2];
/*
 * This program requires the input data file 'ffex02.dat'
 * It reads the data from standard input, e.g.: ffex03 < ffex02.dat
 */
	float wrk[ISTWKL];
/*
 * Conpack work space and Areas area map
 */
	float rwrk[ICPRWL];
	int iwrk[ICPIWL], iam[IAREAL];
/*
 * Arrays for drawing masked grids
 */
	float xcra[MCRA],ycra[MCRA];
	int iaai[NOGI],iagi[NOGI];
/*
 * External subroutine declarations
 */
#ifdef NeedFuncProto
	extern int drawcl(float *,float *,int *, int *, int *, int *);
	extern void setcla(int);
#else
	extern int drawcl();
	extern void setcla();
#endif
	extern int NGCALLF(stumsl,STUMSL)();
	extern void rddata();
    extern void setcgt();
	Gcolr_rep rgb[NCLRS];

	rgb[0].rgb.red = 1.0; rgb[0].rgb.green = 0.0; rgb[0].rgb.blue = 0.0;
	rgb[1].rgb.red = 0.0; rgb[1].rgb.green = 0.0; rgb[1].rgb.blue = 1.0;
	rgb[2].rgb.red = 0.5; rgb[2].rgb.green = 0.5; rgb[2].rgb.blue = 0.5;
/*
 *  Open gks, open and activate a workstation.
 */
    gopen_gks("stdout",0);
    gopen_ws(WKID, NULL, IWTYPE);
    gactivate_ws(WKID);
/*
 * Read the input array data
 */
	rddata();
/*
 * Set up colors for fixed table grayscale and color workstations
 * colors allocated as follows: 1, map outline; 2, contour lines;
 *  3, grid lines; (streamlines and labels use default black or white.
 */
	for( i = 1; i <= NCLRS; i++ ) {
		iclrix=64+(i-1)*196/NCLRS;
		gset_colr_rep(WKID,iclrix,&rgb[i-1]);
		iclr[i-1] = iclrix;
	}
/*
 * Set up the ezmap transformation
 */
    p1[0] = 60.;
    p2[0] = -120.;
    p3[0] = 23.;
    p4[0] = -60.;
    p1[1] = p2[1] = p3[1] = p4[1] = 0.;
	c_mapset("co",p1,p2,p3,p4);
	c_maproj("lc",0.0,-75.0,45.0);
	c_mapint();
	c_mapsti("c5 - continental outline color",iclr[0]);
/*
 * Tell streamlines to use the mapping established by ezmap
 */
	c_stseti("map -- mapping flag", 1);
	c_stseti("set -- set call flag", 0);
/*
 * Set up data coordinate boundaries and special value processing 
 * appropriately for the dataset 
 */
	c_stsetr("xc1 -- lower x bound", -140.0);
	c_stsetr("xcm -- upper x bound", -52.5);
	c_stsetr("yc1 -- lower x bound", 20.0)   ;
	c_stsetr("ycn -- upper y bound", 60.0);

	c_stseti("svf -- special values flag", 1);
	c_stsetr("usv -- u special value", -9999.0);
	c_stsetr("vsv -- v special value", -9999.0);
/*
 * Do the equivalent Conpack setup; note that the special value
 * parameter works a bit differently, and also conpack requires
 * an out of range value to be set whenever the data grid extends
 * outside the map boundaries. The standard value for the default
 * version of cpmpxy is 1.0e12
 */
    c_cpseti("map -- mapping flag", 1);
    c_cpseti("set -- set call flag", 0);
    c_cpsetr("xc1 -- lower x bound", -140.0);
    c_cpsetr("xcm -- upper x bound", -52.5);
    c_cpsetr("yc1 -- lower x bound", 20.0)   ;
    c_cpsetr("ycn -- upper y bound", 60.0);
    c_cpsetr("spv -- special value",-9999.0);
    c_cpsetr("orv -- out of range value",1.0e12);
/*
 * Set conpack graphics text parameters
 */
    setcgt();
/*
 * Draw the continental outline using a wide line
 */
    gset_linewidth(4.0);
    c_maplot();
    gset_linewidth(1.0);
/*
 * Initialize the drawing of the contour plot, and tell conpack
 * to pick contour levels.
 */
    c_cprect ((float *)p,MSIZE,MSIZE,NSIZE,rwrk,ICPRWL,iwrk,ICPIWL);
    c_cppkcl ((float *)p,rwrk,iwrk);
/*
 * Set up contour line attributes
 */
    c_cpgeti("ncl - number of contour levels",&nclv);
    setcla(nclv);
/*
 * Initialize the area map, add the conpack labels to the area map,
 * then draw the labels, draw masked contour lines, and finally
 * draw the masked map grids. Note that there is currently no way
 * to draw a masked continental outline.
 */
    c_arinam (iam,IAREAL);
    c_cplbam ((float *)p,rwrk,iwrk,iam);
    c_cplbdr ((float *)p,rwrk,iwrk);
    c_cpcldm ((float *)p,rwrk,iwrk,iam,drawcl);
    c_mapsti("c2 - grid",iclr[2]);
    gset_line_colr_ind(iclr[2]);
    c_mapgrm (iam,xcra,ycra,MCRA,iaai,iagi,NOGI,drawcl);
    gset_line_colr_ind(1);
/*
 * Adjust streamline rendering options and turn on statistics
 */
    c_stsetr("lwd -- streamline line width",1.75);
    c_stseti("msk -- streamline masking",1);
    c_stsetr("ssp -- stream spacing", 0.012);
    c_stsetr("dfm -- differential magnitude", 0.012);
    c_stseti("sst -- streamline statistics", 1);
/*
 * Initialize streamlines
 */
    c_stinit ((float *)u,MSIZE,(float *)v,MSIZE,&xdm,idm,MSIZE,NSIZE,wrk,ISTWKL);
/*
 * Draw the streamline field plot
 */
    c_stream ((float *)u,(float *)v,&xdm,iam,NGCALLF(stumsl,STUMSL),wrk);
/*
 * Draw a perimeter boundary and eject the frame
 */
	c_perim(1,0,1,0);
	c_frame();
/*
 * Deactivate and close workstation, close gks.
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

int drawcl
#ifdef NeedFuncProto
(float *xcs,float *ycs,int *ncs,int *iai,int *iag,int *nai)
#else
(xcs,ycs,ncs,iai,iag,nai)
float *xcs, *ycs;
int *ncs, *iai, *iag, *nai;
#endif
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

void setcgt()
{
/*
 * Sets conpack graphics text parameters
 */
	c_cpseti ("llp - line label positioning",3);
	c_cpseti ("rwc - real workspace for contours",200);
	c_cpseti ("hlb - high/low label box flag",1);
	c_cpseti ("llb - high/low label box flag",0);
	c_cpsetc ("ilt - information label text"," ");
	c_cpseti ("hlo - high/low label overlap flag",11);
	c_cpsetr ("cwm - character width multiplier",1.25);

	return;
}

void setcla
#ifdef NeedFuncProto
(int nclv)
#else
(nclv)
int nclv;
#endif
{
/*
 * Sets contour line attributes
 */
	int iclu, iclv;

	for( iclv = 1; iclv <= nclv; iclv++ ) {
		c_cpseti ("pai - parameter array index",iclv);
		c_cpgeti ("clu - contour level use flag",&iclu);
		c_cpseti ("cll - contour-line line width",3);
		c_cpseti ("clc - contour-line color", iclr[1]);
		if (iclu == 3) {
            c_cpseti ("cll - contour-line line width",6);
		}
	}
/*
 * "Special" contour lines - grid, special values, and out of range
 * boundaries
 */
	c_cpseti ("pai - parameter array index",-1);
	c_cpseti ("clu - contour level use flag",1);
	c_cpsetr ("cll - contour level line width",2.);
	c_cpseti ("clc - contour level line color",iclr[2]);

	c_cpseti ("pai - parameter array index",-2);
	c_cpseti ("clu - contour level use flag",1);
	c_cpsetr ("cll - contour level line width",2.);
	c_cpseti ("clc - contour level line color",iclr[2]);

	c_cpseti ("pai - parameter array index",-3);
	c_cpseti ("clu - contour level use flag",1);
	c_cpsetr ("cll - contour level line width",2.);
	c_cpseti ("clc - contour level line color",iclr[2]);

	return;
}
