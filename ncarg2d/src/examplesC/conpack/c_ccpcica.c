/*
 * $Id: c_ccpcica.c,v 1.1 1994-05-18 15:55:35 haley Exp $
 */

#include <stdio.h>
#include <math.h>

#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define min(x,y)   ((x) < (y) ? (x) : (y))
#define max(x,y)   ((x) > (y) ? (x) : (y))

#define NLON     361
#define NLAT     181
#define LRWK     5000
#define LIWK     5000
#define ICAM     512
#define ICAN     512


main()
{
	float zdat[NLAT][NLON],rwrk[LRWK];
	Gasfs iasf;
    int i, j, iwrk[LIWK];
    Gpat_rep icra;
    Grect rect;
	extern void color(), setmap(), setctr(), mkdat();
/*
 * Define GKS aspect source flags.
 */
    iasf.linetype = 1;
    iasf.linewidth = 1;
    iasf.line_colr_ind = 1;
    iasf.marker_type = 1;
    iasf.marker_size = 1;
    iasf.marker_colr_ind = 1;
    iasf.text_font_prec = 1;
    iasf.char_expan = 1;
    iasf.char_space = 1;
    iasf.text_colr_ind = 1;
    iasf.fill_int_style = 1;
    iasf.fill_style_ind = 1;
    iasf.fill_colr_ind = 1;
/*
 * Open GKS, turn clipping off, and set up GKS flags
 */
	c_opngks();
	gset_clip_ind(GIND_NO_CLIP);
	gset_asfs(&iasf);
/*
 * Set up a color table
 */
	color();
/*
 * Create some data
 */
	mkdat (2,zdat);
/*
 * Initialize cell array
 */
    icra.colr_array = (Gint *)malloc(ICAM*ICAN*sizeof(Gint));
    icra.dims.size_x = ICAN;
    icra.dims.size_y = ICAM;
/*
 * Setup a map
 */
	setmap ("SV",40.,-105.);
/*
 * Setup contour options
 */
	setctr (0,1,-180.,180.,-90.,90.,1.e36);
/*
 * Initialize Conpack
 */
	c_cprect (&zdat[0][0],NLON,NLON,NLAT,rwrk,LRWK,iwrk,LIWK);
/*
 * Set cell array values and map it to user coordinates
 */
	c_cpcica (&zdat[0][0],rwrk,iwrk,(int *)icra.colr_array,ICAM,ICAM,ICAN,0.,0.,1.,1.);
/*
 * Draw cell array and flush buffer
 */
    rect.p.x = c_cfux(0.);
    rect.p.y = c_cfuy(0.);
    rect.q.x = c_cfux(1.);
    rect.q.y = c_cfuy(1.);

	gcell_array (&rect,&icra);
	c_sflush();
/*
 * Draw contour lines
 */
	gset_line_colr_ind (0);
	c_cpcldr (&zdat[0][0],rwrk,iwrk);
	c_sflush();
/*
 * Draw Map
 */
	gset_line_colr_ind (1);
	gset_linewidth (3.);
	c_maplot();
	gset_linewidth(1.);
	c_mapgrd();
	c_sflush();
/*
 * Draw title
 */
	c_set (0.,1.,0.,1.,0.,1.,0.,1.,1);
	gset_line_colr_ind(1);
	c_plchhq (.5,.95,"Cell Array in Conpack",.03,0.,0.);
/*
 * Close Frame and GKS
 */
	c_frame();
	c_clsgks();

}

void mkdat(nctfr,zdat)
int nctfr;
float *zdat;
{
    int i, j, l;
    float x, y, z, c, zmin, zmax, rlon, rlat;
    extern float c_ggdpnt();
/*
 * nctfr is used to generate a random number, ZDAT is the data array
 */
    x = 0.;
    y = 1.;
    z = .9;
    l = 0;
	c_ggdini(x,y,nctfr,z);
	zmin = 1.e36;
	zmax = -1.e36;
    for( j = 1; j <= NLON; j++ ) {
    	rlon=.017453292519943*(-180.+360.*(float)(j-1)/(float)(NLON-1));
    	for( i = 1; i <= NLAT; i++ ) {
	    	rlat=.017453292519943*(-90.+180.*(float)(i-1)/(float)(NLAT-1));
            x = (float)c_ggdpnt(rlat,rlon);
            c = cos(rlat*4.);
            l = (i-1)*NLON + (j-1);
			zdat[l] = x+.5*c;
			zmin = min(zmin,zdat[l]);
			zmax = max(zmax,zdat[l]);
		}
	}
    l = 0;
	for( i = 0; i < NLAT; i++ ) {
		for( j = 0; j < NLON; j++ ) {
			zdat[l] =((zdat[l]-zmin)/(zmax-zmin))*130.-10.;
			l++;
		}
	}
	for( i = 85; i <= 95; i++ ) {
		for( j = 175; j <= 185; j++ ) {
			l = (i-1)*NLON + (j-1);
			zdat[l] = 1.e36;
		}
	}
	return;
}

void setmap (ptype,plat,plon)
char *ptype;
float plat, plon;
{
    float pm1[2], pm2[2], pm3[2], pm4[2];
/*
 * Set up a map projection with desired options, but don't draw it.
 */
	c_mappos (.05,.90,.05,.90);
	c_maproj (ptype,plat,plon,0.);
    pm1[0] = pm2[0] = pm3[0] = pm4[0] = 0.;
	c_mapset ("MA - MAXIMAL AREA",pm1,pm2,pm3,pm4);
	c_mapint();
	return;
}

void setctr (iset,mapflg,xmin,xmax,ymin,ymax,spval)
int iset, mapflg;
float xmin, xmax, ymin, ymax, spval;
{
/*
 * Set up Conpack options, but don't do anything
 */
	c_cpseti ("SET - DO-SET-CALL FLAG",iset);
	c_cpseti ("MAP - MAPPING FLAG",mapflg);
	c_cpsetr ("XC1 - X COORDINATE AT I=1",xmin);
	c_cpsetr ("XCM - X COORDINATE AT I=M",xmax);
	c_cpsetr ("YC1 - Y COORDINATE AT J=1",ymin);
	c_cpsetr ("YCN - Y COORDINATE AT J=N",ymax);
	c_cpseti ("CLS - CONTOUR LEVEL SELECTOR",1);
	c_cpsetr ("CMN - CONTOUR LEVEL MINIMUM",0.);
	c_cpsetr ("CMX - CONTOUR LEVEL MAXIMUM",110.);
	c_cpsetr ("CIS - CONTOUR INTERVAL SPECIFIER",10.);
	c_cpsetr ("ORV - OUT-OF-RANGE VALUE",1.e12);
	c_cpsetr ("SPV - SPECIAL VALUE",spval);
	c_cpseti ("CAF - CELL ARRAY FLAG",2);
	c_cpseti ("PAI - PARAMETER ARRAY INDEX",-1);
	c_cpseti ("AIA - AREA IDENTIFIER OUTSIDE THE GRID",-100);
	c_cpseti ("PAI - PARAMETER ARRAY INDEX",-2);
	c_cpseti ("AIA - AREA IDENTIFIER - SPECIAL-VALUE AREAS",-100);
	c_cpseti ("LLP - LINE LABEL POSITIONING",0);
	c_cpsetc ("HLT - HIGH/LOW TEXT"," '' ");
	return;
}

void color()
{
    int i;
	Gcolr_rep rgb[16];

    rgb[0].rgb.red = rgb[0].rgb.green = rgb[0].rgb.blue = 0.;
    rgb[1].rgb.red = rgb[1].rgb.green = rgb[1].rgb.blue = 1.;
    rgb[2].rgb.red = 0.5;
	rgb[2].rgb.green = rgb[2].rgb.blue = 1.;
	for( i = 3; i <= 15; i++ ) {
        rgb[i].rgb.red   = max(0.,min(1.,1.-(float)(abs(i-3)/10.)));
        rgb[i].rgb.green = max(0.,min(1.,1.-(float)(abs(i-9)/10.)));
        rgb[i].rgb.blue  = max(0.,min(1.,1.-(float)(abs(i-15)/10.)));
	}
	for( i = 0; i <= 15; i++ ) {
		gset_colr_rep(1,i,&rgb[i]);
	}
	return;
}
