/*  
** FILE c_animat.c
**
** Demonstrate world projection in animation form. 
**
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
** Function needed for graphic subroutines
*/
  /* Fill areas by identifiers. */
int fill(float*,float*,int*,int*,int*,int*);

/*
** Local processing procedures.
*/
  /* define colors */
void color();

  /* projection driver */
void mk_proj(float,float,float);


int main()
{
	float plat;	/* projection latitude */
	float plon;	/* projection longitude */
	float rota;	/* projection angle */

/*
** Print information message.
*/
	printf("\n");
	printf("THIS PROGRAM GENERATES FRAMES WHICH MAY BE\n");
	printf("VIEWED AS AN ANIMATION THROUGH IDT.\n");
	printf("\n");
	printf("THE PROGRAM MAY TAKE A FEW MINUTES TO RUN.\n");
	printf("\n");
/*
** Open GKS, open workstation, activate workstation.
*/
        gopen_gks(IERRF, ISZDM);
        gopen_ws(IWKID, LUNIT, IWTYPE);
        gactivate_ws(IWKID);
/*
** Set for drawing Continental outlines 
*/
        c_mapstc("OU - OUTLINE DATASET SELECTOR","CO");
/*
** Set up initial view coordinates.
*/
	plat = 35.0;
	plon = 0.0;
	rota = 5.0;
/*
** Initialize colors. 
*/
        color();

/*
** Plot a sequence of frames with 
** differing longitude coordinates.
*/
	while (plon > -360.0)
	{
       		mk_proj(plat,(plon=plon-9.0),rota);
        	c_frame();
	}
/*
** Deactivate and close workstatione, close GKS.
*/
        gdeactivate_ws(IWKID);
        gclose_ws(IWKID);
        gclose_gks();

	return 0;
}


void mk_proj(float plat, float plon, float rota) 
{
/*
** Projection driver routine.
**
** plat,plon	- latitude/longitude of projection
** rota		- angle of projection
*/

        int map[LMAP];			/* map data */
	int iarea[ISIZ];		/* area identifier */
	int igrp[ISIZ];			/* group identifier */

        float xwrk[NWRK], ywrk[NWRK];	/* working arrays */

	float plim1[2],plim2[2],plim3[2],plim4[2]; 	/* dummy params for */
							/* c_mapset         */
/*
** Set up projection
*/
        c_maproj ("OR",plat,plon,rota);
/*
** Set limits of map
*/
        c_mapset ("MA",plim1,plim2,plim3,plim4);
/*
** Initialize Maps and Areas
*/
        c_mapint();
        c_arinam(map,LMAP);
        c_mapbla(map);
/*
** Color fill display
*/
        gset_fill_int_style (1);
        c_arscam (map, xwrk, ywrk, NWRK, iarea, igrp, ISIZ, fill);
/*
** Draw Continental Outlines
*/
        c_mapsti("PE - PERIMETER FLAG",0);
        c_maplot();
}


int fill(float*xwrk, float*ywrk, int*wrk, int*iarea, int*igrp, int*idsiz)
{
/*
** Color geographic areas.
**
** xwrk,ywrk,wrk   - working data arrays
** iarea           - area identifier
** igrp            - group identifier
** idsiz           - total number of group identifiers
*/
        int i;          		/* counter */
        int id;         		/* area identier */

        Gpoint_list point_list;		/* area boundary points */

/*
** Retrieve geographic area id
**/
        id = 0;
        for(i=0; i<*idsiz; ++i)
        {
                if (igrp[i] == 1)
                {
                        id = iarea[i];
                }
        }
/*
** if it's not background, color the area
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
		free(point_list.points);
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
        colr_rep.rgb.green = 0.5;
        colr_rep.rgb.blue = 0.7;
        gset_colr_rep (IWKID,2,&colr_rep);
/*
** Green
*/
        colr_rep.rgb.red = 0.0;
        colr_rep.rgb.green = 0.60;
        colr_rep.rgb.blue = 0.1;
        gset_colr_rep (IWKID,3,&colr_rep);
}
