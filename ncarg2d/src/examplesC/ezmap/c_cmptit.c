/*	PROGRAM c_maptit
**
** Produce a single frame with maximal-area views of all
** the EZMAP projections of the globe.
**
** Define area map array and size for area fill applications
**
** This program is a direct C translation of maptit.f.
*/

#include <stdio.h>
#include <math.h>
#include <ncarg/gks.h>
#include <ncarg/ncargC.h>

#define LMAP 250000	/* map work space */
#define MCS 10000	/* scratch array wok space */
#define NIDS 2		/* land/ocean id's */

/*
** Declare masking and area shading routines
*/
int MASK(float*,float*,int*,int*,int*,int*);
int SHADE1(float*,float*,int*,int*,int*,int*);
int SHADE2(float*,float*,int*,int*,int*,int*);
int CFILL(float*,float*,int*,int*,int*,int*);
void COLOR();

#define WSTYPE SED_WSTYPE
#define WKID   1

int main()
{
	int map[LMAP];		/* map data */
	int iarea[NIDS];	/* area identifiers */
	int igrp[NIDS];		/* group identifiers */
	int ipat;		/* dash pattern value */

	float xcs[MCS];		/* c_mapgrm scratch work array */
	float ycs[MCS];		/* c_mapgrm scratch work array */

/*
** Open GKS.
*/
	gopen_gks ("stdout",0);
	gopen_ws (WKID, NULL, WSTYPE);
	gactivate_ws(WKID);
/*
** Turn off the clipping indicator.
*/
        gset_clip_ind(0);
/*
** Set up color table and dash pattern
*/ 
	COLOR();
	ipat = 65535;
	c_dashdb (&ipat);
/*
** Set the outline-dataset parameter.
*/
	c_mapstc("OU - OUTLINE DATASET SELECTOR","CO");
	c_mapsti("LA - LABEL FLAG",0);
/*
** Put meridians and parallels every 15 degrees.
*/
	c_mapsti("GR - GRID SPACING",15);
/*
** Don't draw labels
*/
	c_mapsti("LA - LABEL FLAG",0);
/*
** Do draw elliptical perimeter
*/
	c_mapsti("EL - ELLIPTICAL-PERIMETER SELECTOR",1);
/*
** Lambert conformal conic.
*/
	c_mappos(.025,.24375,.63125,.85);
	c_maproj("LC",30.,0.,45.);
	c_mapint();
/*
** Yellow Grid
*/
	gset_line_colr_ind(13);
	c_mapgrd();
	c_maplbl();
/*
** Green Contenental Outline
*/
	gset_line_colr_ind(6);
	c_maplot();
/*
** Stereographic.
*/
	c_mappos(.26875,.4875,.63125,.85);
	c_maproj("ST",0.,0.,0.);
/*
** Aqua
*/
	c_mapint();
	gset_line_colr_ind(8);
	c_mapgrd();
	c_maplbl();
/*
** Chartreuse
*/
	gset_line_colr_ind(2);
	c_maplot();
/*
** Orthographic.
*/
	c_mappos(.5125,.73125,.63125,.85);
	c_maproj("OR",0.,0.,0.);
/*
** Orchid
*/
	c_mapint();
	gset_line_colr_ind(15);
	c_mapgrd();
	c_maplbl();
/*
** SlateBlue
*/
	gset_line_colr_ind(8);
	c_maplot();
/*
** Lambert equal-area.
*/
	c_mappos(.75625,.975,.63125,.85);
	c_maproj("LE",0.,0.,0.);
/*
** Red
*/
	c_mapint();
	gset_line_colr_ind(3);
	c_mapgrd();
	c_maplbl();
/*
** Yellow
*/
	gset_line_colr_ind(6);
	c_maplot();
/*
** Gnomonic.
**
** Draw lat/lon lines only over the oceans
*/ 
	gset_line_colr_ind(1);
	c_mappos(.025,.24375,.3875,.60625);
	c_maproj("GN",0.,0.,0.);
	c_arinam(map, LMAP);
	c_mapint();
	c_mapbla(map);
	c_mapgrm(map, xcs, ycs, MCS, iarea, igrp, NIDS, MASK);
	c_maplbl();
	c_maplot();
/*
** Azimuthal equidistant.
*/
	c_arinam(map, LMAP);
	c_mappos(.26875,.4875,.3875,.60625);
	c_maproj("AE",0.,0.,0.);
	c_mapint();
	c_mapbla(map);

	c_mapgrm(map, xcs, ycs, MCS, iarea, igrp, NIDS, MASK);

	gset_line_colr_ind(2);
	c_maplbl();
	c_maplot(); 
/*
** Satellite-view.
**
** Do this plot in white with Softfill over the water and no lat/lon
** lines
*/
	c_arinam(map, LMAP);
	gset_line_colr_ind(1);
	c_mappos(.5125,.73125,.3875,.60625);
	c_maproj("SV",0.,0.,0.);
	c_mapstr("SA - SATELLITE DISTANCE",2.);
	c_mapstc("OU - OUTLINE DATASET SELECTOR","PO");
	c_mapint();
	c_mapbla(map);
	c_arscam(map, xcs, ycs, MCS, iarea, igrp, NIDS, SHADE1);
	c_maplbl();
	c_maplot();
/*
** Mercator.
*/
	c_arinam(map, LMAP);
	gset_line_colr_ind(1);
	c_mappos(.75625,.975,.3875,.60625);
	c_maproj("ME",0.,0.,0.);
	c_mapint();
	c_mapbla(map);
	c_arscam(map, xcs, ycs, MCS, iarea, igrp, NIDS, SHADE2);
	c_maplbl();
	c_maplot();
/*
** Cylindrical equidistant.
*/
	c_arinam(map, LMAP);
	gset_line_colr_ind(1);
	c_mappos(.025,.4875,.13125,.3625);
	c_maproj("CE",0.,0.,0.);
	c_mapint();
	c_mapbla(map);
	c_arscam(map, xcs, ycs, MCS, iarea, igrp, NIDS, CFILL);
	c_maplbl();
	c_maplot();
/*
** Mollweide type.
*/
	c_arinam(map, LMAP);
	gset_line_colr_ind(1);
	c_mappos(.5125,.975,.13125,.3625);
	c_maproj("MO",0.,0.,0.);
	c_mapint();
	c_mapbla(map);
	c_arscam(map, xcs, ycs, MCS, iarea, igrp, NIDS, CFILL);
	c_maplbl();
	c_maplot();
/*
** and the labels under each sub-plot.
*/
	gset_linewidth(2.);
	c_set(0.,1.,0.,1.,0.,1.,0.,1.,1);
	c_pchiqu(.134375,.61875,"LAMBERT CONFORMAL CONIC",.0085,0.,0.);
	c_pchiqu(.378125,.61875,"STEREOGRAPHIC",.0085,0.,0.);
	c_pchiqu(.621875,.61875,"ORTHOGRAPHIC",.0085,0.,0.);
	c_pchiqu(.865625,.61875,"LAMBERT EQUAL-AREA",.0085,0.,0.);
	c_pchiqu(.134375,.375,"GNOMONIC",.0085,0.,0.);
	c_pchiqu(.378125,.375,"AZIMUTHAL EQUIDISTANT",.0085,0.,0.);
	c_pchiqu(.621875,.375,"SATELLITE-VIEW",.0085,0.,0.);
	c_pchiqu(.865625,.375,"MERCATOR",.0085,0.,0.);
	c_pchiqu(.25625,.11875,"CYLINDRICAL EQUIDISTANT",.0085,0.,0.);
	c_pchiqu(.74375,.11875,"MOLLWEIDE TYPE",.0085,0.,0.);
/*
** Advance the frame.
*/
	c_frame();
/*
** Close GKS.
*/
	gdeactivate_ws(WKID);
	gclose_ws(WKID);
	gclose_gks();
/*
** Done.
*/
	return 0;
}


int MASK(float* xc, float* yc, int* npts, int* areaid, int* grpid, int* idsize)
{
	int id;		/* area id */
	int i;		/* counter */

/*
** Retrieve area id for geographical area
*/
	for(i=0; i<*idsize; ++i)
	{
	   	if (grpid[i] == 1)
		{
 			id = areaid[i];
		}
	}
/*
** if the line is over water, and has 2 or more points draw it.
*/
	if ((c_mapaci(id) == 1) && (*npts >= 2))
	{
		c_curved(xc,yc,*npts);
	}
/*
** Otherwise, don't draw the line - mask it.
*/
	return 1;
}




int SHADE1(float* xc,float* yc,int* npts,int* areaid,int* grpid,int* idsize)
{
/*
** Fill area map
*/
	int iwrk[MCS];		/* work array */
	int iaid;		/* area identifier */
	int i;			/* counter */

	float rwrk[MCS];	/* work array */	

	iaid = 0;
	for(i=0; i<*idsize; ++i)
	{
	   	if (grpid[i] == 1)
		{
			iaid = areaid[i];
		}
	}
/*
** Fill Areas over land using softfill
**
** Areas over water have area color indices of 1 so we use that to 
** distinguish them.
*/
	if ((iaid > 0) && (c_mapaci(iaid) == 1))
	{
	   	c_sfsetr("SP",.005);
	   	c_sfsetr("ANGLE", 45.);
	   	c_sfwrld(xc, yc, *npts-1, rwrk, MCS, iwrk, MCS);
	}
	return 1;
}


int SHADE2(float* xc,float* yc,int* npts,int* areaid,int* grpid,int* idsize)
{
/*
** Fill area map
*/
	int iwrk[MCS];		/* work array */
	int iaid;		/* area identifier */
	int i;			/* counter */

	float rwrk[MCS];	/* work array */	

	Gpoint_list point_list; /* points for line drawing */


	gset_fill_int_style(1);
	iaid = 0;
	for(i=0; i<*idsize; ++i)
	{
	   	if (grpid[i] == 1)
		{
			iaid = areaid[i];
		}
	}

	point_list.num_points = *npts;
	point_list.points = (Gpoint*)malloc(*npts*(sizeof(point_list.points)+
			    (2 * sizeof(point_list.points[0].x))));
	for(i=0; i<*npts; ++i)
	{
		point_list.points[i].x = xc[i];
		point_list.points[i].y = yc[i];
	}

/*
** Fill Areas over land using softfill
**
** Areas over water have area color indices of 1 so we use that to 
** distinguish them.
*/
	if (iaid > 0) 
	{
		if (c_mapaci(iaid) == 1)
		{
	      		c_sfsetr("SP",.005);
	      		c_sfsetr("ANGLE", 45.);
	      		c_sfwrld(xc, yc, *npts-1, rwrk, MCS, iwrk, MCS);
		}
		else
		{
	      		gset_fill_colr_ind(c_mapaci(iaid));
	      		gfill_area(&point_list);
		}
	}
	free (point_list.points);
	return 1;
}

int CFILL(float* xc,float* yc,int* npts,int* areaid,int* grpid,int* idsize)
{
/*
** Fill area map
*/
	int icol;	/* color value */
	int iarea;	/* area identifier */
	int i;		/* counter */

	Gpoint_list point_list;		/* point list for line drawing */


	icol = 0;
	for(i=0; i<*idsize; ++i)
	{
	   	if (grpid[i] == 1)
		{
			iarea = areaid[i];
		}
	}
	   
	point_list.num_points = *npts-1;
	point_list.points = (Gpoint*)malloc(*npts*(sizeof(point_list.points)+
			    (2 * sizeof(point_list.points[0].x))));
	for(i=0; i<*npts-1; ++i)
	{
		point_list.points[i].x = xc[i];
		point_list.points[i].y = yc[i];
	}

	if (iarea >= 1)
	{
	   	icol = c_mapaci(iarea);
	   	if (icol == 1)
		{
/*
** Color the ocean blue.
*/
	      		gset_fill_colr_ind(2);
	      		gfill_area(&point_list);
		}
		else
		{
/*
** if the area is over land, fill it using the country color id.
*/
	      		gset_fill_colr_ind(icol+2);
	      		gfill_area(&point_list);
		}
	}
	free (point_list.points);
	return 1;
}


void COLOR()
{
	Gcolr_rep colr_rep;	/* red, green, blue color values */

/*
**    BACKGROUND COLOR
** Black
*/
	colr_rep.rgb.red = 0.0;
	colr_rep.rgb.green = 0.0;
	colr_rep.rgb.blue = 0.0;
      	gset_colr_rep(1,0,&colr_rep);
/*
**     FORGROUND COLORS
** White
*/
	colr_rep.rgb.red = 1.0;
	colr_rep.rgb.green = 1.0;
	colr_rep.rgb.blue = 1.0;
      	gset_colr_rep(1,1,&colr_rep);
/*
** Aqua
*/
	colr_rep.rgb.red = 0.0;
	colr_rep.rgb.green = 0.9;
	colr_rep.rgb.blue = 1.0;
      	gset_colr_rep(1,2,&colr_rep);
/*
** Red
*/
	colr_rep.rgb.red = 0.9;
	colr_rep.rgb.green = 0.25;
	colr_rep.rgb.blue = 0.0;
      	gset_colr_rep(1,3,&colr_rep);
/*
** OrangeRed
*/
	colr_rep.rgb.red = 1.0;
	colr_rep.rgb.green = 0.0;
	colr_rep.rgb.blue = 0.2;
      	gset_colr_rep(1,4,&colr_rep);
/*
** Orange
*/
	colr_rep.rgb.red = 1.0;
	colr_rep.rgb.green = 0.65;
	colr_rep.rgb.blue = 0.0;
      	gset_colr_rep(1,5,&colr_rep);
/*
** Yellow
*/
	colr_rep.rgb.red = 1.0;
	colr_rep.rgb.green = 1.0;
	colr_rep.rgb.blue = 0.0;
      	gset_colr_rep(1,6,&colr_rep);
/*
** GreenYellow
*/
	colr_rep.rgb.red = 0.7;
	colr_rep.rgb.green = 1.0;
	colr_rep.rgb.blue = 0.2;
      	gset_colr_rep(1,7,&colr_rep);
/*
** Chartreuse
*/
	colr_rep.rgb.red = 0.5;
	colr_rep.rgb.green = 1.0;
	colr_rep.rgb.blue = 0.0;
      	gset_colr_rep(1,8,&colr_rep);
/*
** Celeste
*/
	colr_rep.rgb.red = 0.2;
	colr_rep.rgb.green = 1.0;
	colr_rep.rgb.blue = 0.5;
      	gset_colr_rep(1,9,&colr_rep);
/*
** Green
*/
	colr_rep.rgb.red = 0.2;
	colr_rep.rgb.green = 0.8;
	colr_rep.rgb.blue = 0.2;
      	gset_colr_rep(1,10,&colr_rep);
/*
** DeepSkyBlue
*/
	colr_rep.rgb.red = 0.0;
	colr_rep.rgb.green = 0.75;
	colr_rep.rgb.blue = 1.0;
      	gset_colr_rep(1,11,&colr_rep);
/*
** RoyalBlue
*/
	colr_rep.rgb.red = 0.25;
	colr_rep.rgb.green = 0.45;
	colr_rep.rgb.blue = 0.95;
      	gset_colr_rep(1,12,&colr_rep);
/*
** SlateBlue
*/
	colr_rep.rgb.red = 0.4;
	colr_rep.rgb.green = 0.35;
	colr_rep.rgb.blue = 0.8;
      	gset_colr_rep(1,13,&colr_rep);
/*
** DarkViolet
*/
	colr_rep.rgb.red = 0.6;
	colr_rep.rgb.green = 0.0;
	colr_rep.rgb.blue = 0.8;
      	gset_colr_rep(1,14,&colr_rep);
/*
** Orchid
*/
	colr_rep.rgb.red = 0.85;
	colr_rep.rgb.green = 0.45;
	colr_rep.rgb.blue = 0.8;
      	gset_colr_rep(1,15,&colr_rep);
/*
** Lavender
*/
	colr_rep.rgb.red = 0.8;
	colr_rep.rgb.green = 0.8;
	colr_rep.rgb.blue = 1.0;
      	gset_colr_rep(1,16,&colr_rep);
/*
** Gray
*/
	colr_rep.rgb.red = 0.7;
	colr_rep.rgb.green = 0.7;
	colr_rep.rgb.blue = 0.7;
      	gset_colr_rep(1,17,&colr_rep);
/*
** Done.
*/
}
