#include <math.h>
#include <stdio.h>
#include <stdlib.h>

#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define WKID   1

/*
 *  Illustrate line joins, caps, and miter limits.
 */

/*
 *  Specify certain key points on the plot (indices 1-3 for vertices 
 *  of joins; 4-6 for vertices of caps; 7-8 for vertices of miter 
 *  limits).
 */
float x[8] = {0.20, 0.50, 0.80, 0.28, 0.28, 0.28, 0.75, 0.75};
float y[8] = {0.80, 0.67, 0.80, 0.35, 0.19, 0.03, 0.35, 0.14};

main()
{
#ifdef NeedFuncProto
	extern void drawv(float,float,int,int,float,float,int,float);
#else
	extern void drawv();
#endif
	Gcolr_rep rgb;
	int i;
/*
 *  Open GKS.
 */
	gopen_gks ("stdout",0);
/*
 *  Open and activate a color PostScript workstation.
 */
	gopen_ws (WKID, NULL, c_ngpswk("PS","PORT","COLOR"));
	gactivate_ws (WKID);
/*
 *  Define color indices.
 */
	rgb.rgb.red = 1.0; rgb.rgb.green = 1.0; rgb.rgb.blue = 1.0;
	gset_colr_rep(WKID, 0, &rgb );
	rgb.rgb.red = 0.0; rgb.rgb.green = 0.0; rgb.rgb.blue = 0.0;
	gset_colr_rep(WKID, 1, &rgb );
/*
 *  Set workstation "1" as the one involved in subsequent NGSETI settings.
 */
	c_ngseti("Workstation",1);
/*
 *  Line joins.
 * 
 *  Labels.
 */
	c_pcseti("FN",25);
	c_pcseti("CC",1);
	c_plchhq(.5,y[0]+0.15,"Line joins",.032,0.,0.);
	c_plchhq(x[0],y[0]+0.07,"miter",.025,0.,0.);
	c_plchhq(x[1],y[0]+0.07,"round",.025,0.,0.);
	c_plchhq(x[2],y[0]+0.07,"bevel",.025,0.,0.);
/*
 *  Loop through the three types of line join.
 */
	for( i = 1; i <=3 ; i++ ) {
        c_ngseti("Joins",i-1);
        drawv(x[i-1],y[i-1],(i-1)%2,0,90.,0.7,1,25.);
        drawv(x[i-1],y[i-1],(i-1)%2,1,90.,0.7,0,1.);
	}
/*
 *  Line caps.
 *
 * 
 *  Labels.
 */
	c_plchhq(x[3],y[3]+0.15,"Line caps",.032,0.,0.);
	c_plchhq(x[3],y[3]+0.06,"butt",.025,0.,0.);
	c_plchhq(x[3],y[4]+0.06,"round",.025,0.,0.);
	c_plchhq(x[3],y[5]+0.06,"square",.025,0.,0.);
/*
 *  Loop through the three types of line caps.
 */
	for( i = 4; i <= 6 ; i++ ) {
        c_ngseti("Caps",i-4);
        drawv(x[i-1],y[i-1],0,0,180.,0.75,1,25.);
        drawv(x[i-1],y[i-1],0,2,180.,0.75,0,1.);
	}
/*
 *  Miter limits.
 * 
 *  Labels.
 */
	c_plchhq(x[6],y[6]+0.15,"Miter limits",.032,0.,0.);
	c_plchhq(x[6],y[6]+0.07,"default (= 10.)",.025,0.,0.);
	c_plchhq(x[6],y[7]+0.04,"limit = 1.",.025,0.,0.);
/*
 *  Set line join to miter.
 */
	c_ngseti("Join",0);
/*
 *  Default.
 */
	drawv(x[6],y[6],0,0,35.,0.5,1,20.);
/*
 *  Limit = 1.
 */
	c_ngsetr("Miter",1.);
	drawv(x[7],y[7],0,0,35.,0.5,1,20.);

	c_frame();
/*
 *  Deactivate and close the workstation, close GKS.
 */
	gdeactivate_ws (WKID);
	gclose_ws (WKID);
	gclose_gks();
}

void drawv
#ifdef NeedFuncProto
(float x,float y,int iorien,int idot,float ang,float scale,int icolor,float thick)
#else
(x,y,iorien,idot,ang,scale,icolor,thick)
float x, y, ang, scale, thick;
int iorien, idot, icolor;
#endif
{
/*
 *  Draw a "V" where:
 *
 *       (X,Y)   is the coordinate of the vertex.
 *       IORIEN  flags whether the "V" is up (=1) or down (=0).
 *       IDOT    flags whether dots are to be drawn at coordinates.
 *               = 0 no dots.
 *               = 1 dots at all coordinates
 *               = 2 dots only at the end points
 *       ANG     is the angle (in degrees) at the vertex of the "V".
 *       SCALE   scales how big the "V" is.
 *       ICOLOR  is the color index to be used for the lines.
 *       THICK   is the linewidth scale factor.
 */
	Gpoint_list line;
    float xv[3], yv[3];

	float  radc = 0.0174532, dsiz = 0.25;
	float beta, xoff, yoff, sign;

	line.num_points = 3;
	line.points = (Gpoint *)malloc(line.num_points * sizeof(Gpoint));
	if( !line.points ) {
		fprintf( stderr, "drawv: Not enough memory to create line structure\n" );
		gemergency_close_gks();
		exit(1);
	}
	gset_line_colr_ind(icolor);
	gset_linewidth(thick);

	beta = radc*(90.-0.5*ang);
	xoff = scale*dsiz*cos(beta);
	yoff = scale*dsiz*sin(beta);
	if (iorien == 0) {
        sign =  1.;
	}
	else {
        sign = -1.;
	}

	xv[0] = line.points[0].x = x-xoff;
	xv[1] = line.points[1].x = x;
	xv[2] = line.points[2].x = x+xoff;
	yv[0] = line.points[0].y = y-sign*yoff;
	yv[1] = line.points[1].y = y;
	yv[2] = line.points[2].y = yv[0];

	gpolyline(&line);
	free((Gpoint *)line.points);
	if (idot == 1) {
        c_ngdots(xv,yv,3,0.005*thick,icolor);
    }
    else if (idot == 2) {
        c_ngdots(&xv[0],&yv[0],1,0.005*thick,icolor);
        c_ngdots(&xv[2],&yv[2],1,0.005*thick,icolor);
    }
    return;
}

