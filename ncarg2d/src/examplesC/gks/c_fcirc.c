#include <math.h>
#include <stdio.h>

#include <ncarg/ncargC.h>
#include <ncarg/gks.h>


#define WSTYPE SED_WSTYPE
#define WKID   1

#define IWTYPE   1

#define NPTS   101
#define MAPSIZ   5000
#define NGRPS   3
#define NC   200

main()
{
/*
 *  Demonstrate different GKS fill types
 */
	int i, map[MAPSIZ], iarea[NGRPS], igrp[NGRPS];
	float x1[NPTS],y1[NPTS],x2[NPTS],y2[NPTS],x3[NPTS],y3[NPTS];
	float xc[NC], yc[NC], ang, x, y;

	extern int fill();
/*
 * Convert from degrees to radians.
 */
	float d2r = .017453292519943;
/*
 * Generate three intersecting circles of radius 1.
 */
	for( i = 0; i < NPTS; i++ ) {
		ang=d2r*3.6*(float)i;
		x=cos(ang);
		y=sin(ang);
		x1[i] = x - .5;
		x2[i] = x + .5;
		x3[i] = x;
		y1[i] = y + .5;
		y2[i] = y + .5;
		y3[i] = y - .5;
	}
/*
 *  Open GKS, open and activate a workstation.
 */
	gopen_gks("stdout", 0 );
	gopen_ws(WKID, NULL, IWTYPE);
	gactivate_ws(WKID);
/*
 * Define the entire viewport and
 * a window from -2. to 2 with linear scaling.
 */
	c_set(0.,1.,0.,1.,-2.,2.,-2.,2.,1);
/*
 * Initialize the area map
 */
	c_arinam(map,MAPSIZ);
/*
 * Add the three objects as 3 edge groups
 */
	c_aredam(map,x1,y1,NPTS,1,1,0);
	c_aredam(map,x2,y2,NPTS,2,2,0);
	c_aredam(map,x3,y3,NPTS,3,4,0);
/*
 * Fill the different regions
 */
	c_arscam(map,xc,yc,NC,iarea,igrp,NGRPS,fill);
/*
 * Close the plot
 */
	c_frame();
/*
 * Deactivate and close workstation, close GKS.
 */
	gdeactivate_ws(WKID);
	gclose_ws(WKID);
	gclose_gks();
	
	printf("WARNING: fcirc.ncgm generates an error message when viewed.\n" );
}

int fill(xc, yc, nc, iarea, igrp, ngrps)
float *xc, *yc;
int *nc, *iarea, *igrp, *ngrps;
{
	int icolor, i;
	Gpoint_list fill_area;

	icolor=0;
	for( i=0; i < *ngrps; i++ ) {
        icolor = icolor + iarea[i];
	}
/*
 *  FILL THE REGION WITH THE APPROPRIATE COLOR
 */
	if (icolor > 0) {
		fill_area.num_points = *nc-1;
		fill_area.points = (Gpoint *) malloc(fill_area.num_points*sizeof(Gpoint));
		if( !fill_area.points ) {
			fprintf( stderr, "fill: Not enough memory to create fill area structure\n" );
			gemergency_close_gks();
			exit(1);
		}
		for( i = 0; i < *nc-1; i++ ) {
			fill_area.points[i].x = xc[i];
			fill_area.points[i].y = yc[i];
		}
        gset_fill_int_style(icolor%4);
        gfill_area(&fill_area);
		free(fill_area.points);
	}
	return(0);
}

