/*  
** FILE CMPFIL
**
** Demonstrate map projections with continental/political
** boundary colorings.
**
** This file is a direct translation of cmpfil.f
*/

#include <stdio.h>
#include <math.h>
#include <ncarg/gks.h>
#include <ncarg/ncargC.h>

/*
** Define error file, Fortran unit number, and workstation type,
** and workstation ID.
*/
#define IERRF  "stdout"
#define LUNIT  "gmeta"
#define IWTYPE SED_WSTYPE
#define IWKID  1
#define ISZDM  0

#define LMAP 150000	/* map data elements */
#define NWRK 10000	/* working array elements */
#define ISIZ 5		/* identifier array size */

/*
** Functions needed for graphic subroutines
*/
  /* Mask latitude/longitude lines over continents. */
int mask(float*,float*,int*,int*,int*,int*);

  /* Fill areas by identifiers. */
int fill(float*,float*,int*,int*,int*,int*);

/*
** Local processing procedures.
*/
  /* define colors */
void color();

  /* demo driver */
void cmpfil(char*,float,float,float,char*,char*,float);


int main()
{
	float plat;	/* projection latitude */
	float plon;	/* projection longitude */
	float rota;	/* projection angle */
	float grd;	/* grid spacing */

/*
** Open GKS, open workstation, activate workstation.
*/
        gopen_gks(IERRF, ISZDM);
        gopen_ws(IWKID, LUNIT, IWTYPE);
        gactivate_ws(IWKID);
/*
** INVOKE DEMO DRIVER
*/
	plat = 40.0;
	plon = -50.0;
	rota = 0.0;
	grd = 10.0;
        cmpfil("SV",plat,plon,rota,"PO","MA",grd);
/*
** Advance the frame.
*/
        c_frame();
/*
** DEACTIVATE AND CLOSE WORKSTATION, CLOSE GKS.
*/
        gdeactivate_ws(IWKID);
        gclose_ws(IWKID);
        gclose_gks();
	return 0;
}


void cmpfil(char* proj, float plat, float plon, float rota, 
            char* outln, char* jlim, float grd)
{
/*
** Demo driver routine.
**
** proj		- projection type
** plat,plon	- latitude/longitude of projection
** rota		- angle of projection
** outln	- outline dataset flag
** jlim		- map limits selector
** grd		- grid spacing
*/

	int ipat;			/* line dash pattern */
        int map[LMAP];			/* map data */
	int iarea[ISIZ];		/* area identifier */
	int igrp[ISIZ];			/* group identifier */

        float xwrk[NWRK], ywrk[NWRK];	/* working arrays */

	float plim1[2],plim2[2],plim3[2],plim4[2]; 	/* dummy params for */
							/* c_mapset         */

/*
** CMPLOT demonstrates MAPLOT drawing continental and political outlines
**
** Set up Maps.
*/
        color();
	ipat = 65535;
	c_dashdb(&ipat);
/*
** Draw Continental, political outlines 
*/
        c_mapstc("OU - OUTLINE DATASET SELECTOR",outln);
/*
** Set grid spacing
*/
        c_mapstr ("GR - GRID SPACING",grd);
/*
** Set up projection
*/
        c_maproj (proj,plat,plon,rota);
/*
** If it's a satellite projection, choose a satellite distance
*/
        if (strcmp(proj,"SV") == 0) c_mapstr ("SA - SATELLITE DISTANCE",7.0);
/*
** Set limits of map
*/
        c_mapset (jlim,plim1,plim2,plim3,plim4);
/*
** Initialize Maps and Areas
*/
        c_mapint();
        c_arinam(map,LMAP);
        c_mapbla(map);
/*
** Color fill each country a different color
*/
        gset_fill_int_style (1);
        c_arscam (map, xwrk, ywrk, NWRK, iarea, igrp, ISIZ, fill);
/*
** Draw Masked Grid Lines
*/
        c_mapgrm (map, xwrk, ywrk, NWRK, iarea, igrp, ISIZ, mask);
/*
** Draw Continental Outlines
*/
        c_mapsti("LA - LABEL FLAG",0);
        c_mapsti("EL - ELLIPTICAL-PERIMETER SELECTOR",1);
        c_maplbl();
        c_maplot();
}


int mask(float* xc, float* yc, int* mcs, int* iarea, int* igrp, int* ngrps)
{
/*
** Mask latitude and longitude lines over continents.
**
** xc,yc	- X and Y coordinates of line 
** mcs		- number of line points
** iarea	- area identifier
** igrp		- group identifier
** ngrps	- total number of groups
*/

        int i;		/* counter */
        int idgeo;	/* geographic area id */

/*
** Retrieve geographic area identifier
*/
        idgeo = -1;
        for(i=0; i<*ngrps; ++i)
        {
           if (igrp[i] == 1) idgeo = iarea[i];
        }
/*
**If the line is over water, and has 2 or more points draw it.
*/
        if ((c_mapaci(idgeo) == 1)&&(*mcs >= 2))
	{ 
		c_curved(xc,yc,*mcs);
	}
/*
** Otherwise, don't draw the line - mask it.
*/
        return 1;
}



int fill(float*xwrk, float*ywrk, int*wrk, int*iarea, int*igrp, int*idsiz)
{
/*
** Retrieve area id for geographic area
**
** xwrk,ywrk,wrk   - working data arrays
** iarea           - area identifier
** igrp            - group identifier
** idsiz           - total number of group identifiers
*/
        int i;          /* counter */
        int id;         /* area identier */

        Gpoint_list point_list;

        id = 0;
        for(i=0; i<*idsiz; ++i)
        {
                if (igrp[i] == 1)
                {
                        id = iarea[i];
                }
        }
/*
** if it's not water, draw it
*/
        if (id >= 1)
        {
           	gset_fill_colr_ind(c_mapaci(id)+1);
                point_list.num_points = *wrk;
                point_list.points = (Gpoint*)malloc(*wrk *
                                    (sizeof(point_list.points[0]) +
                                     2 * sizeof(point_list.points[0].x)));
                for(i=0; i<*wrk; ++i)
                {
                        point_list.points[i].x = xwrk[i];
                        point_list.points[i].y = ywrk[i];
                }
                gfill_area(&point_list);
        }
/*
** Otherwise, do nothing
*/
        return 1;
}



void color()
{
/*
** Define color table
*/

        Gcolr_rep colr_rep; 	/* RGB color values */

/*
** Background is black
*/
        colr_rep.rgb.red = 0.0;
        colr_rep.rgb.green = 0.0;
        colr_rep.rgb.blue = 0.0;
        gset_colr_rep (IWKID,0,&colr_rep);
/*
**     FOREGROUND COLORS
** White
*/
        colr_rep.rgb.red = 1.0;
        colr_rep.rgb.green = 1.0;
        colr_rep.rgb.blue = 1.0;
        gset_colr_rep (IWKID,1,&colr_rep);
/*
** Aqua
*/
        colr_rep.rgb.red = 0.0;
        colr_rep.rgb.green = 0.9;
        colr_rep.rgb.blue = 1.0;
        gset_colr_rep (IWKID,2,&colr_rep);
/*
** Red
*/
        colr_rep.rgb.red = 0.9;
        colr_rep.rgb.green = 0.25;
        colr_rep.rgb.blue = 0.0;
        gset_colr_rep (IWKID,3,&colr_rep);
/*
** OrangeRed
*/
        colr_rep.rgb.red = 1.0;
        colr_rep.rgb.green = 0.0;
        colr_rep.rgb.blue = 0.2;
        gset_colr_rep (IWKID,4,&colr_rep);
/*
** Orange
*/
        colr_rep.rgb.red = 1.0;
        colr_rep.rgb.green = 0.65;
        colr_rep.rgb.blue = 0.0;
        gset_colr_rep (IWKID,5,&colr_rep);
/*
** Yellow
*/
        colr_rep.rgb.red = 1.0;
        colr_rep.rgb.green = 1.0;
        colr_rep.rgb.blue = 0.0;
        gset_colr_rep (IWKID,6,&colr_rep);
/*
** GreenYellow
*/
        colr_rep.rgb.red = 0.7;
        colr_rep.rgb.green = 1.0;
        colr_rep.rgb.blue = 0.2;
        gset_colr_rep (IWKID,7,&colr_rep);
/*
** Chartreuse
*/
        colr_rep.rgb.red = 0.5;
        colr_rep.rgb.green = 1.0;
        colr_rep.rgb.blue = 0.0;
        gset_colr_rep (IWKID,8,&colr_rep);
/*
** Celeste
*/
        colr_rep.rgb.red = 0.2;
        colr_rep.rgb.green = 1.0;
        colr_rep.rgb.blue = 0.5;
        gset_colr_rep (IWKID,9,&colr_rep);
/*
** Green
*/
        colr_rep.rgb.red = 0.2;
        colr_rep.rgb.green = 0.8;
        colr_rep.rgb.blue = 0.2;
        gset_colr_rep (IWKID,10,&colr_rep);
/*
** DeepSkyBlue
*/
        colr_rep.rgb.red = 0.0;
        colr_rep.rgb.green = 0.75;
        colr_rep.rgb.blue = 1.0;
        gset_colr_rep (IWKID,11,&colr_rep);
/*
** RoyalBlue
*/
        colr_rep.rgb.red = 0.25;
        colr_rep.rgb.green = 0.45;
        colr_rep.rgb.blue = 0.95;
        gset_colr_rep (IWKID,12,&colr_rep);
/*
** SlateBlue
*/
        colr_rep.rgb.red = 0.4;
        colr_rep.rgb.green = 0.35;
        colr_rep.rgb.blue = 0.8;
        gset_colr_rep (IWKID,13,&colr_rep);
/*
** DarkViolet
*/
        colr_rep.rgb.red = 0.6;
        colr_rep.rgb.green = 0.0;
        colr_rep.rgb.blue = 0.8;
        gset_colr_rep (IWKID,14,&colr_rep);
/*
** Orchid
*/
        colr_rep.rgb.red = 0.85;
        colr_rep.rgb.green = 0.45;
        colr_rep.rgb.blue = 0.8;
        gset_colr_rep (IWKID,15,&colr_rep);
/*
** Lavender
*/
        colr_rep.rgb.red = 0.8;
        colr_rep.rgb.green = 0.8;
        colr_rep.rgb.blue = 1.0;
        gset_colr_rep (IWKID,16,&colr_rep);
/*
** Gray
*/
        colr_rep.rgb.red = 0.7;
        colr_rep.rgb.green = 0.7;
        colr_rep.rgb.blue = 0.7;
        gset_colr_rep (IWKID,17,&colr_rep);
}
