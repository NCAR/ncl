#include <math.h>
#include <stdio.h>

#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define WSTYPE SED_WSTYPE
#define WKID   1

#define IWTYPE   1
#define ILD    121
#define ID    11
#define IDM1  ID-1

/*
 *  Coordinate data defining 10 points for a five-pointed star.
 *  The ten points specify the tips of the star as well as the
 *  points toward the center between the tips.  The first point
 *  is set equal to the last point for the purposes of drawing
 *  the outline of the points.
 */
float xs[ID] = {0.00000, -0.22451, -0.95105, -0.36327, -0.58780,
                0.00000,  0.58776,  0.36327,  0.95107,  0.22453,
                0.00000};
float ys[ID] = {1.00000,  0.30902,  0.30903, -0.11803, -0.80900,
               -0.38197, -0.80903, -0.11805,  0.30898,  0.30901,
                1.00000};

/*
 *  Coordinates for labelling the stars.
 */
float xp[IDM1] = {.243,.180,.025,.138,.098,.243,.385,.345,.457,.320};
float yp[IDM1] = {.690,.540,.513,.415,.285,.340,.285,.415,.513,.540};

main()
{
/*
 *  Illustrate the fill algorithm determining what is inside
 *  a polygon.
 *
 *
 *  Define error file, Fortran unit number, and workstation type,
 *  and workstation ID.
 */
	float xd[ID],yd[ID], x, y, scl;
	Gcolr_rep rgb;
	Gpoint_list area;
	int i;
	extern void pltnum();
/*
 *  Open GKS, open and activate a workstation.
 */
	gopen_gks("stdout", 0 );
	gopen_ws(WKID, NULL, IWTYPE);
	gactivate_ws(WKID);
/*
 *  Define colors.
 */
	rgb.rgb.red = 1.0; rgb.rgb.green =  1.0; rgb.rgb.blue = 1.0;
	gset_colr_rep(WKID,0,&rgb);
	rgb.rgb.red = 0.0; rgb.rgb.green =  0.0; rgb.rgb.blue = 1.0;
	gset_colr_rep(WKID,1,&rgb);
	rgb.rgb.red = 0.4; rgb.rgb.green =  0.0; rgb.rgb.blue = 0.4;
	gset_colr_rep(WKID,2,&rgb);
	rgb.rgb.red = 1.0; rgb.rgb.green =  0.0; rgb.rgb.blue = 0.0;
	gset_colr_rep(WKID,3,&rgb);
/*
 *  Draw the star with interior style solid;
 *  use ten points in the fill area call.
 */
	x = .25;
	y = .45;
	scl = .2;
	area.num_points = ID;
	area.points = (Gpoint *)malloc(area.num_points*sizeof(Gpoint));
	if( !area.points ) {
		fprintf( stderr, "c_pgkex16: Not enough memory to create fill area structure\n" );
		gemergency_close_gks();
		exit(1);
	}
	for( i = 0; i < ID; i++ ) {
        area.points[i].x = x+scl*xs[i];
        area.points[i].y = y+scl*ys[i];
	}
	gset_fill_int_style(GSTYLE_SOLID);
	gset_fill_colr_ind(1);
	gfill_area(&area);
/*
 *  Label the points.
 */
	for( i = 0; i < 10; i++ ) {
        pltnum (xp[i],yp[i],i+1);
	}
/*
 *  Draw lines connecting the coordinate points.
 */
	gset_line_colr_ind(3);
	gset_linewidth(4.);
	gpolyline(&area);
/*
 *  Draw the star with interior style solid;
 *  use only the five tips of the star as coordinates.
 */
	x = .75;
	y = .45;
	scl = .2;
	area.num_points = 5;
	area.points[0].x = x+scl*xs[0];
	area.points[0].y = y+scl*ys[0];
	area.points[1].x = x+scl*xs[4];
	area.points[1].y = y+scl*ys[4];
	area.points[2].x = x+scl*xs[8];
	area.points[2].y = y+scl*ys[8];
	area.points[3].x = x+scl*xs[2];
	area.points[3].y = y+scl*ys[2];
	area.points[4].x = x+scl*xs[6];
	area.points[4].y = y+scl*ys[6];
	gfill_area(&area);
/*
 *  Draw lines connecting the coordinate points.
 */
	area.points[5].x = area.points[0].x;
	area.points[5].y = area.points[0].y;
	area.num_points = 6;
	gpolyline(&area);
/*
 *  Label the points.
 */
	pltnum (xp[0]+.5,yp[0],1);
	pltnum (xp[2]+.5,yp[2],4);
	pltnum (xp[4]+.5,yp[4],2);
	pltnum (xp[6]+.5,yp[6],5);
	pltnum (xp[8]+.5,yp[8],3);
/*
 *  Label the plot using Plotchar.
 */
	c_pcseti("FN",25);
	c_pcseti("CC",2);
	c_plchhq(.5,.91,"Filled areas",.035,0.,0.);
	c_plchhq(.5,.84,"What's inside, what's outside?",.035,0.,0.);
	free(area.points);
/*
 *  Close picture, deactivate and close the workstation, close GKS.
 */
	c_frame();
    gdeactivate_ws(WKID);
    gclose_ws(WKID);
    gclose_gks();
}

void pltnum(x,y,num)
float x, y;
int num;
{
/*
 *  Plot the value of the integer NUM at coordinate location (X,Y)
 */
	char label[3];

	sprintf( label, "%2d", num );
	c_pcseti("FN",22);
	c_pcseti("CC",2);
	c_plchhq(x,y,label,.023,0.,0.);
	return;
}
