
/*
 *	$Id: c_cmpgrp.c,v 1.1 1994-07-20 17:05:53 haley Exp $
 */
#include <stdio.h>
#include <math.h>

#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define WSTYPE SED_WSTYPE
#define WKID   1
#define LMAP   150000
#define NWRK   10000
#define ISIZ   5

#define max(x,y)   ((x) > (y) ? (x) : (y) )

main()
{
	float plim1[2], plim2[2], plim3[2], plim4[2];
	extern void cmpfil();

	plim1[0] =  0.; plim1[1] = 0.;
	plim2[0] =  0.; plim2[1] = 0.;
	plim3[0] =  0.; plim3[1] = 0.;
	plim4[0] =  0.; plim4[1] = 0.;
/*
 * Open GKS, Turn Clipping off
 */
	gopen_gks ("stdout",0);
	gopen_ws (WKID, NULL, WSTYPE);
	gactivate_ws(WKID);
/*
 * Invoke demo driver
 */
	cmpfil("SV",40.,-50.,0.,"PO","MA",plim1,plim2,plim3,plim4,10.);
/*
 * Advance the frame.
 */
	c_frame();
/*
 * Deactivate and close workstation, close GKS.
 */
	gdeactivate_ws (WKID);
	gclose_ws (WKID);
	gclose_gks();
}

void cmpfil(proj, plat, plon, rota, outln, jlim, plim1, plim2, plim3, plim4, grd)
char *proj, *outln, *jlim;
float plat, plon, rota, *plim1, *plim2, *plim3, *plim4, grd;
{
	extern int mask(), fill();
	extern void color();

	int i, map[LMAP], iarea[ISIZ], igrp[ISIZ];
	float xwrk[NWRK], ywrk[NWRK];
/*
 * CMPLOT demonstrates MAPLOT drawing continental and political outlines
 *
 * Set up Maps.
 */
	color();
	i = 65535;
	c_dashdb(&i);
/*
 * Set up 9 vertical strips
 */
	c_mapsti ("VS - VERTICAL STRIPS",9);
/*
 * Draw Continental, political outlines 
 */
	c_mapstc ("OU - OUTLINE DATASET SELECTOR",outln);
/*
 * Set grid spacing
 */
	c_mapstr ("GR - GRID SPACING",grd);
/*
 * Set up projection
 */
	c_maproj (proj,plat,plon,rota);
/*
 * If it's a satellite projection, choose a satellite distance
 */
	if (!strcmp(proj,"SV")) c_mapstr ("SA - SATELLITE DISTANCE",5.);
/*
 * Set limits of map
 */
	c_mapset (jlim,plim1,plim2,plim3,plim4);
/*
 * Initialize Maps and Areas
 */
	c_mapint();
	c_arinam (map,LMAP);
	c_mapbla (map);
/*
 * Color fill each country a different color
 */
	gset_fill_int_style (GSTYLE_SOLID);
	c_arscam (map, xwrk, ywrk, NWRK, iarea, igrp, ISIZ, fill);
/*
 * Draw Masked Grid Lines
 */
	c_mapgrm (map, xwrk, ywrk, NWRK, iarea, igrp, ISIZ, mask);
/*
 * Draw Continental Outlines
 */
	c_mapsti("LA - LABEL FLAG",0);
	c_mapsti("EL - ELLIPTICAL-PERIMETER SELECTOR",1);
	c_maplbl();
	c_maplot();
/*
 * Done.
 */
	return;
}

int mask(xc,yc,mcs,areaid,grpid,idsize)
float *xc, *yc;
int *mcs, *areaid, *grpid, *idsize;
{
	int i, id;
/*
 * Retrieve area id for geographical area
 */
	for( i = 0; i < *idsize; i++ ) {
		if (grpid[i] == 1) id=areaid[i];
	}
/*
 * If the line is over water, and has 2 or more points draw it.
 */
	if ((c_mapaci(id) == 1) && (*mcs >= 2)) {
		c_curved(xc,yc,*mcs);
	}
/*
 * Otherwise, don't draw the line - mask it.
 */
	return(0);
}

int fill (xwrk,ywrk,nwrk,iarea,igrp,idsiz)
float *xwrk,*ywrk;
int *iarea,*igrp,*nwrk,*idsiz;
{
	Gpoint_list area;
	int i, itmp;
/*
 * Check if the area is over the map
 */
	if ((iarea[0] >= 0) & (iarea[1] >= 0)) {
		area.num_points = *nwrk;
		area.points = (Gpoint *) malloc(area.num_points*sizeof(Gpoint));
		if( !area.points ) {
			fprintf( stderr, "fill: Not enough memory to create fill area structure\n" );
			gemergency_close_gks();
			exit(1);
		}
		for( i = 0; i < *nwrk; i++ ) {
			area.points[i].x = xwrk[i];
			area.points[i].y = ywrk[i];
		}
		itmp=max(iarea[0],iarea[1]);
		gset_fill_colr_ind(c_mapaci(itmp)+1);
		gfill_area(&area);
		free(area.points);
	}
/*
 * Otherwise, do nothing
 */
	return(0);
}

void color()
{
	Gcolr_rep rgb;
/*
 * Background color
 * Black
 */
	rgb.rgb.red = 0.; rgb.rgb.green = 0.; rgb.rgb.blue = 0.;
	gset_colr_rep(WKID,0,&rgb);
/*
 * Foreground colors
 * White
 */
	rgb.rgb.red = 1.0; rgb.rgb.green =  1.0; rgb.rgb.blue =  1.0;
	gset_colr_rep(WKID,  1,&rgb);
/*
 * Aqua
 */
	rgb.rgb.red = 0.0; rgb.rgb.green =  0.9; rgb.rgb.blue =  1.0;
	gset_colr_rep(WKID,  2,&rgb);
/*
 * Red
 */
	rgb.rgb.red = 0.9; rgb.rgb.green =  0.25; rgb.rgb.blue =  0.0;
	gset_colr_rep(WKID,  3,&rgb);
/*
 * OrangeRed
 */
	rgb.rgb.red = 1.0; rgb.rgb.green =  0.0; rgb.rgb.blue =  0.2;
	gset_colr_rep(WKID,  4,&rgb);
/*
 * Orange
 */
	rgb.rgb.red = 1.0; rgb.rgb.green =  0.65; rgb.rgb.blue =  0.0;
	gset_colr_rep(WKID,  5,&rgb);
/*
 * Yellow
 */
	rgb.rgb.red = 1.0; rgb.rgb.green =  1.0; rgb.rgb.blue =  0.0;
	gset_colr_rep(WKID,  6,&rgb);
/*
 * GreenYellow
 */
	rgb.rgb.red = 0.7; rgb.rgb.green =  1.0; rgb.rgb.blue =  0.2;
	gset_colr_rep(WKID,  7,&rgb);
/*
 * Chartreuse
 */
	rgb.rgb.red = 0.5; rgb.rgb.green =  1.0; rgb.rgb.blue =  0.0;
	gset_colr_rep(WKID,  8,&rgb);
/*
 * Celeste
 */
	rgb.rgb.red = 0.2; rgb.rgb.green =  1.0; rgb.rgb.blue =  0.5;
	gset_colr_rep(WKID,  9,&rgb);
/*
 * Green
 */
	rgb.rgb.red = 0.2; rgb.rgb.green =  0.8; rgb.rgb.blue =  0.2;
	gset_colr_rep(WKID, 10,&rgb);
/*
 * DeepSkyBlue
 */
	rgb.rgb.red = 0.0; rgb.rgb.green =  0.75; rgb.rgb.blue =  1.0;
	gset_colr_rep(WKID, 11,&rgb);
/*
 * RoyalBlue
 */
	rgb.rgb.red = 0.25; rgb.rgb.green =  0.45; rgb.rgb.blue =  0.95;
	gset_colr_rep(WKID, 12,&rgb);
/*
 * SlateBlue
 */
	rgb.rgb.red = 0.4; rgb.rgb.green =  0.35; rgb.rgb.blue =  0.8;
	gset_colr_rep(WKID, 13,&rgb);
/*
 * DarkViolet
 */
	rgb.rgb.red = 0.6; rgb.rgb.green =  0.0; rgb.rgb.blue =  0.8;
	gset_colr_rep(WKID, 14,&rgb);
/*
 * Orchid
 */
	rgb.rgb.red = 0.85; rgb.rgb.green =  0.45; rgb.rgb.blue =  0.8;
	gset_colr_rep(WKID, 15,&rgb);
/*
 * Lavender
 */
	rgb.rgb.red = 0.8; rgb.rgb.green =  0.8; rgb.rgb.blue =  1.0;
	gset_colr_rep(WKID, 16,&rgb);
/*
 * Gray
 */
	rgb.rgb.red = 0.7; rgb.rgb.green =  0.7; rgb.rgb.blue =  0.7;
	gset_colr_rep(WKID, 17,&rgb);
/*
 * Done.
 */
	return;
}
