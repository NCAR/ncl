/*
 *	$Id: c_fcover.c.sed,v 1.2 1994-05-26 21:34:11 haley Exp $
 */
#include <stdio.h>
#include <math.h>
#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define MSIZE     73
#define NSIZE     73
#define ICPIWL     1000
#define ICPRWL     5000
#define IAREAL     20000
#define MCRA     64
#define NOGI     64
#define LMAP     150000
#define NWRK     10000
#define ISIZ     5
#define NCLRS1     15
#define NCLRS2     9
#define NROWS     11

int ifilix[2];
float rlwfac;

/*
 * indices used for vector coloring
 */
	int iclr1[NCLRS1+1] = {0, 10, 17, 24, 31, 38, 45, 52, 60, 67, 74, 81, 88, 95, 102, 109};

/*
 * indices used for coloring other plot features; be careful not
 * to duplicate any color indices in iclr1
 */
	int iclr2[NCLRS2+1] = {0, 0, 1, 2, 149, 225, 175, 176, 200, 3};


/*
 * empirically determined vector thinning data
 * starting from the pole, each row of vectors is weeded, mod the
 * value of this data.
 */
	int ithin[NROWS] = {90,15,5,5,4,4,3,3,2,2,2};

main()
{
/*
 * This program requires the input data file 'fun-cover.dat'
 * It reads the data from standard input, e.g.: fun-cover < fun-cover.dat
 */
	float u[NSIZE][MSIZE], v[NSIZE][MSIZE], p[NSIZE][MSIZE];
/*
 * Conpack work space and Areas area map
 */
	float rwrk[ICPRWL];
	int iwrk[ICPIWL], iam[IAREAL];
/*
 * arrays for drawing masked grids
 */
	float xcra[MCRA],ycra[MCRA];
	int iaai[NOGI],iagi[NOGI];
/*
 * map filling arrays
 */
	int map[LMAP], iarea[ISIZ], igrp[ISIZ];
	float xwrk[NWRK], ywrk[NWRK];
	int i, j, nclv, idm;
	float vmn, vmx, dmx, *xdm;
    float vl,vr,vb,vt,ul,ur,ub,ut;
    int ll;
	float p1[2], p2[2], p3[2], p4[2];
/*
 * external subroutine declarations
 */
	extern int fill();
	extern int drawcl();
	extern int vvudmv_();
    extern void setcgt();
    extern void setcla();
	extern void rddata();
	Gcolr_rep rgb1[NCLRS1+1];
	Gcolr_rep rgb2[NCLRS2+1];
/*
 * define a set of rgb color triples for vector colors
 */
	rgb1[0].rgb.red=0.00000;  rgb1[0].rgb.green=0.00000;  rgb1[0].rgb.blue=0.00000;
	rgb1[1].rgb.red=0.00000;  rgb1[1].rgb.green=1.00000;  rgb1[1].rgb.blue=0.00000;
	rgb1[2].rgb.red=0.14286;  rgb1[2].rgb.green=1.00000;  rgb1[2].rgb.blue=0.00000;
	rgb1[3].rgb.red=0.28571;  rgb1[3].rgb.green=1.00000;  rgb1[3].rgb.blue=0.00000;
	rgb1[4].rgb.red=0.42857;  rgb1[4].rgb.green=1.00000;  rgb1[4].rgb.blue=0.00000;
	rgb1[5].rgb.red=0.57143;  rgb1[5].rgb.green=1.00000;  rgb1[5].rgb.blue=0.00000;
	rgb1[6].rgb.red=0.71429;  rgb1[6].rgb.green=1.00000;  rgb1[6].rgb.blue=0.00000;
	rgb1[7].rgb.red=0.85714;  rgb1[7].rgb.green=1.00000;  rgb1[7].rgb.blue=0.00000;
	rgb1[8].rgb.red=1.00000;  rgb1[8].rgb.green=1.00000;  rgb1[8].rgb.blue=0.00000;
	rgb1[9].rgb.red=1.00000;  rgb1[9].rgb.green=0.85714;  rgb1[9].rgb.blue=0.00000;
	rgb1[10].rgb.red=1.00000; rgb1[10].rgb.green=0.71429; rgb1[10].rgb.blue=0.00000;
	rgb1[11].rgb.red=1.00000; rgb1[11].rgb.green=0.57143; rgb1[11].rgb.blue=0.00000;
	rgb1[12].rgb.red=1.00000; rgb1[12].rgb.green=0.42857; rgb1[12].rgb.blue=0.00000;
	rgb1[13].rgb.red=1.00000; rgb1[13].rgb.green=0.28571; rgb1[13].rgb.blue=0.00000;
	rgb1[14].rgb.red=1.00000; rgb1[14].rgb.green=0.14286; rgb1[14].rgb.blue=0.00000;
	rgb1[15].rgb.red=1.00000; rgb1[15].rgb.green=0.00000; rgb1[15].rgb.blue=0.00000;
/*
 * rgb values for other plot features
 */
	rgb2[0].rgb.red = 0.0; rgb2[0].rgb.green = 0.0; rgb2[0].rgb.blue = 0.0;
	rgb2[1].rgb.red = 1.0; rgb2[1].rgb.green = 1.0; rgb2[1].rgb.blue = 1.0;
	rgb2[2].rgb.red = 0.0; rgb2[2].rgb.green = 0.0; rgb2[2].rgb.blue = 0.0;
	rgb2[3].rgb.red = 0.9; rgb2[3].rgb.green = 0.9; rgb2[3].rgb.blue = 0.9;
	rgb2[4].rgb.red = 0.6; rgb2[4].rgb.green = 0.6; rgb2[4].rgb.blue = 0.6;
	rgb2[5].rgb.red = 0.3; rgb2[5].rgb.green = 0.3; rgb2[5].rgb.blue = 0.3;
	rgb2[6].rgb.red = 0.8; rgb2[6].rgb.green = 0.9; rgb2[6].rgb.blue = 1.0;
	rgb2[7].rgb.red = 0.5; rgb2[7].rgb.green = 0.0; rgb2[7].rgb.blue = 0.5; 
/*
 * this is the ocean color; the color actually used on the 
 * fundamentals cover is the following commented out value --
 * for viewing on a workstation a bit darker color makes the
 * lettering easier to read.
 *
 *     +     0.0,0.9,1.0,
 */
	rgb2[8].rgb.red = 0.0; rgb2[8].rgb.green = 0.5; rgb2[8].rgb.blue = 0.7;
	rgb2[9].rgb.red = 0.0; rgb2[9].rgb.green = 0.0; rgb2[9].rgb.blue = 0.0;
/*
 * -----------------------------------------------------------------
 *
 * open gks, open workstation, activate workstation.
 */
	c_opngks();
/*
 * give initial value to fill color index stored common block cbfill
 */
	ifilix[0] = 149;
	ifilix[1] = 200;
	rlwfac=1.0;
/*
 * set up auxiliary colors
 */
	for( i = 1; i <= NCLRS2; i++ ) {
		gset_colr_rep(1,iclr2[i],&rgb2[i]);
	}
/*
 * read the input array data
 */
	rddata(u,v,p,MSIZE,NSIZE);
/*
 * message the data to eliminate surplus of vectors near the pole
 */
	for( j=NSIZE; j >= NSIZE-NROWS+1; j-- ) {
		for( i = 1; i <= MSIZE; i++ ) {
            if (i % ithin[NSIZE-j] != 0) {
				u[j-1][i-1] = -9999.0;
			}
		}
	}
/*
 * set up the ezmap projection
 */
	c_mapstc ("ou - outline dataset selector","co");
	p1[0] = 10.0;
	p2[0] = -180.0;
	p3[0] = 10.0;
	p4[0] = 0.0;
	c_mapset("co",p1,p2,p3,p4);
	c_maproj("st",90.0,180.0,45.0);
	c_mapint();
/*
 * initialize maps and areas
 */
	c_mapint();
	c_arinam (map,LMAP);
	c_mapbla (map);
/*
 * tell vectors to use the mapping established by ezmap
 */
	c_vvseti("map -- mapping flag", 1);
	c_vvseti("set -- set call flag", 0);
/*
 * set up data coordinate boundaries and special value processing 
 * appropriately for the dataset 
 */
	c_vvsetr("xc1 -- lower x bound", -180.0);
	c_vvsetr("xcm -- upper x bound", 180.0);
	c_vvsetr("yc1 -- lower x bound", -90.0);
	c_vvsetr("ycn -- upper y bound", 90.0);

	c_vvseti("svf -- special values flag", 3);
	c_vvsetr("usv -- u special value", -9999.0);
	c_vvsetr("vsv -- v special value", -9999.0);
	c_vvsetr("psv - p special value", -9999.0);
	c_vvseti("spc - p special color", 1);
/*
 * do the equivalent conpack setup; note that the special value
 * parameter works a bit differently, and also conpack requires
 * an out of range value to be set whenever the data grid extends
 * outside the map boundaries. the standard value for the default
 * version of cpmpxy is 1.0e12
 */
	c_cpseti("map -- mapping flag", 1);
	c_cpseti("set -- set call flag", 0);
	c_cpsetr("xc1 -- lower x bound", -180.0);
	c_cpsetr("xcm -- upper x bound", 180.0);
	c_cpsetr("yc1 -- lower x bound", -90.0);
	c_cpsetr("ycn -- upper y bound", 90.0);
	c_cpsetr("spv -- special value",-9999.0);
	c_cpsetr("orv -- out of range value",1.0e12);
/*
 * set conpack graphics text parameters
 */
    setcgt();
/*
 * turn on statistics reporting, turn off vector text blocks
 */
    c_vvseti("vst -- vector statistics", 1);
    c_vvsetc("mnt - minimum vector text block", " ");
    c_vvsetc("mxt - maximum vector text block", " ");
/*
 * initialize the drawing of the contour plot, and tell conpack
 * to pick contour levels.
 */
	c_cprect ((float *)p,MSIZE,MSIZE,NSIZE,rwrk,ICPRWL,iwrk,ICPIWL);
	c_cppkcl ((float *)p,rwrk,iwrk);
/*
 * set up contour line attributes
 */
	c_cpgeti("ncl - number of contour levels",&nclv);
	setcla(nclv, iclr2[6],iclr2[3]);
/*
 * initialize the area map, and add the conpack labels to the area map
 */
	c_arinam (iam,IAREAL);
	c_cplbam ((float *)p,rwrk,iwrk,iam);
/*
 * set up vector color processing
 */
	c_vvseti("msk -- vector masking",1);
	c_vvseti("ctv -- color thresholds value", 2);
	c_vvseti("nlv -- number of levels", NCLRS1);
	for( j=1; j <= NCLRS1; j++ ) {
		gset_colr_rep(1,iclr1[j],&rgb1[j]);
		c_vvseti("pai -- parameter array index", j);
		c_vvseti("clr -- gks color index", iclr1[j]);
	}
/*
 * modify the color table for a blue background
 * and modify the contour attributes
 */
	gset_colr_rep(1,iclr2[1],&rgb2[8]);
	gset_colr_rep(1,iclr2[2],&rgb2[1]);
    setcla(nclv, iclr2[7],iclr2[3]);
/*
 * draw four frames showing first the complete picture, then the
 * plot decomposed into 1) ezmap components 2) conpack components
 * and 3) vectors components
 */
    for( i = 1; i <= 4; i++ ) {
/*
 * solid file continental boundaries
 */
        if (i == 1 || i == 2) {
/*
 * color fill land masses using a gray scale value
 */
            gset_fill_int_style (GSTYLE_SOLID);
            c_arscam (map, xwrk, ywrk, NWRK, iarea, igrp, ISIZ, fill);
/*
 * draw boundaries, including the limb
 */
            c_mapsti("c5 - continental outline color",iclr2[9]);
            c_maplot();
        }
/*
 * draw the masked contour lines
 */
        if (i == 1 || i == 3) {
            c_cpcldm ((float *)p,rwrk,iwrk,iam,drawcl);
        }
/*
 * draw the map grid
 */
        if (i == 1 || i == 2) {
            c_mapsti("c2 - grid",iclr2[9]);
            gset_line_colr_ind(iclr2[9]);
            c_mapgrm (iam,xcra,ycra,MCRA,iaai,iagi,NOGI,drawcl);
            gset_line_colr_ind(1);
        }
        if (i == 1 || i == 4) {
/*
 * initialize vectors
 */
            c_vvinit ((float *)u,MSIZE,(float *)v,MSIZE,(float *)p,MSIZE,MSIZE,NSIZE,xdm,idm);
/*
 * adjust vector rendering options
 */
            c_vvsetr("amn -- arrow minimum size",0.007);
            c_vvsetr("lwd -- vector line width",3.00*rlwfac);
            c_vvgetr("vmn -- minimum vector",&vmn);
            c_vvgetr("vmx -- maximum vector",&vmx);
            c_vvsetr("vlc -- vector low cutoff",vmn+0.1*(vmx-vmn));
            c_vvgetr("dmx -- device maximum vector length",&dmx);
            c_getset(&vl,&vr,&vb,&vt,&ul,&ur,&ub,&ut,&ll);
            c_vvsetr("vrl - vector realized length",4.0*dmx/(vr-vl));
            c_vvsetr("vfr -- vector fractional minimum",0.4);
/*
 * draw the vector field plot
 */
            c_vvectr ((float *)u,(float *)v,(float *)p,iam,vvudmv_,xdm);
        }
/*
 * draw labels last
 */
        if (i == 1 || i == 3) {
            c_cpseti("hlb",1);
            gset_fill_int_style(GSTYLE_HOLLOW);
            c_cplbdr ((float *)p,rwrk,iwrk);
		}
/*
 * draw a perimeter boundary and eject the frame
 */
		c_perim(1,0,1,0);
		c_frame();
	}
/*
 *     deactivate and close workstation, close gks.
 */
	c_clsgks();
}

void rddata(u,v,p,m,n)
float *u, *v, *p;
{
	int i;
	char stmp[257];
/*
 * read the data arrays from the standard input 
 */
	for( i = 0; i < (m*n)-1; i++ ) {
		fscanf( stdin, "%g", &u[i] );
		fscanf(stdin, "%2s", stmp);
	}
	fscanf( stdin, "%g", &u[i] );
	for( i = 0; i < (m*n)-1; i++ ) {
		fscanf( stdin, "%g", &v[i] );
		fscanf(stdin, "%2s", stmp);
	}
	fscanf( stdin, "%g", &v[i] );
	for( i = 0; i < m*n; i++ ) {
		fscanf( stdin, "%g", &p[i] );
		fscanf(stdin, "%2s", stmp);
	}
	return;
}

/*
 * =====================================================================
 */
int drawcl (xcs,ycs,ncs,iai,iag,nai)
float *xcs, *ycs;
int *ncs, *iai, *iag, *nai;
{
/*
 * routine for masked drawing of contour and grid lines
 *
 * this version of drawcl draws the polyline defined by the points
 * ((xcs(i),ycs(i)),i=1,ncs) if and only if none of the area identifiers
 * for the area containing the polyline are negative.  the dash package
 * routine curved is called to do the drawing.
 *
 * turn on drawing.
 */
	int i, idr;
	idr = 1;
/*
 * if any area identifier is negative, turn off drawing.
 */
	for( i = 0; i < *nai; i++ ) {
		if (iai[i] < 0) idr=0;
	}
/*
 * if drawing is turned on, draw the polyline.
 */
	if (idr!=0) c_curved (xcs,ycs,*ncs);
/*
 * done.
 */
	return(0);
}
/*
 * =====================================================================
 */
void setcgt()
{
/*
 * sets conpack graphics text parameters
 */
	c_cpseti ("llp - line label positioning",0);
	c_cpseti ("rwc - real workspace for contours",200);
	c_cpseti ("hlb - high/low label box flag",1);
	c_cpseti ("llb - line label box flag",0);
	c_cpsetc ("ilt - information label text"," ");
	c_cpseti ("hlo - high/low label overlap flag",11);
	c_cpsetr ("cwm - character width multiplier",1.25);
	return;
}
/*
 * =====================================================================
 */

void setcla (nclv, iclclr, iaxclr)
int nclv, iclclr, iaxclr;
{
/*
 * sets contour line attributes
 */
	int iclv, iclu;

	for( iclv = 1; iclv <= nclv; iclv++ ) {
		c_cpseti ("pai - parameter array index",iclv);
		c_cpgeti ("clu - contour level use flag",&iclu);
		c_cpsetr ("cll - contour-line line width",3.0*rlwfac);
		c_cpseti ("clc - contour-line color", iclclr);
		if (iclu == 3) {
            c_cpsetr ("cll - contour-line line width",6.0*rlwfac);
		}
	}
/*
 * "special" contour lines - grid, special values, and out of range
 * boundaries 
 */
	c_cpseti ("pai - parameter array index",-1);
	c_cpseti ("clu - contour level use flag",0);
	c_cpsetr ("cll - contour level line width",2.);
	c_cpseti ("clc - contour level line color",iaxclr);

	c_cpseti ("pai - parameter array index",-2);
	c_cpseti ("clu - contour level use flag",1);
	c_cpsetr ("cll - contour level line width",2.);
	c_cpseti ("clc - contour level line color",iaxclr);

	c_cpseti ("pai - parameter array index",-3);
	c_cpseti ("clu - contour level use flag",1);
	c_cpsetr ("cll - contour level line width",2.);
	c_cpseti ("clc - contour level line color",iaxclr);

	return;
}

int fill (xwrk,ywrk,nwrk,iarea,igrp,idsiz)
float *xwrk, *ywrk;
int *nwrk, *iarea, *igrp, *idsiz;
{
/*
 * retrieve area id for geographic area
 */
    Gpoint_list fill_area;
	int i, id;

	id = 0;
	for( i = 0; i < *idsiz; i++ ) {
		if (igrp[i] == 1) id = iarea[i];
	}
/*
 * if it's not water, draw it
 */
	if (id >= 1) {
		if (c_mapaci(id)!=1) {
            gset_fill_colr_ind(ifilix[0]);
		}
		else {
            gset_fill_colr_ind(ifilix[1]);
		}
/*
 * Create structure to pass to gfill_area
 */
        fill_area.num_points = *nwrk;
        fill_area.points = (Gpoint *) malloc(fill_area.num_points*sizeof(Gpoint));
        if( !fill_area.points ) {
            fprintf( stderr, "fill: Not enough memory to create fill area structure\n" );
            gemergency_close_gks();
            exit(1);
        }
        for( i = 0; i < *nwrk; i++ ) {
            fill_area.points[i].x = xwrk[i];
            fill_area.points[i].y = ywrk[i];
        }
		gfill_area(&fill_area);
        free((Gpoint *)fill_area.points);
	}
/*
 * otherwise, do nothing
 */
	return(0);
}
