/*
 *	$Id: c_fthex01.c,v 1.5 2009-08-15 05:08:27 fred Exp $
 */
#include <stdio.h>
#include <math.h>

#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define IWTYPE 1
#define WKID   1

#define WHITE   1
#define BLACK   0
#define MAGENTA 2
#define BLUE    3
#define GREEN   4
#define RED     5

#define min(x,y)   ((x) < (y) ?  (x) : (y))

main()
{
	float umin, umax, vmin, vmax, wmin, wmax;
	float upos, vpos, wpos, ptmp, qtmp, utmp, vtmp;
	int i, j, ilab, ibeg, iend, ichr;
#ifdef NeedFuncProto
	extern float wfun(float,float);
#else
	extern float wfun();
#endif
	extern void color();
/*
 * Declare an array in which to put an eye position for THREED.
 */
	float peye[3];
/*
 * Define a character variable in which to form numeric labels.
 */
	char chrs[9];
/*
 *  Open GKS, open and activate a workstation.
 */
	gopen_gks("stdout",0);
	gopen_ws(WKID, NULL, IWTYPE);
	gactivate_ws(WKID);
/*
 * Define color table
 */
	color();
/*
 * Make the tick marks drawn by c_perim3 different from the default.
 */
	c_tick43 (12,8,24,16,48,32);
/*
 * Define the boundaries of the box to be projected from 3-space to
 * 2-space.
 */
	umin=0.;
	umax=1.;
	vmin=0.;
	vmax=1.;
	wmin=0.;
	wmax=1.;
/*
 * Define the position of the eye.
 */
	peye[0] = 6.;
	peye[1] = 4.;
	peye[2] = 5.;
/*
 * Initialize Threed.
 */
	c_set3 (.1,.9,.1,.9,umin,umax,vmin,vmax,wmin,wmax,peye);
/*
 * Draw perimeters in each of the three coordinate planes.
 */
	gset_line_colr_ind(RED);
	c_perim3 (10,2,10,2,1,0.);
	c_perim3 (10,2,10,2,2,0.);
	c_perim3 (10,4,10,4,3,0.);
/*
 * Put some labels on the plot.  First, the U axis.
 */
	gset_line_colr_ind(MAGENTA);
	c_pwrzt (.5,0.,1.1,"U",1,3,-1,+3,0);

	for( ilab = 1; ilab <=10; ilab++ ) {
		upos=(float)(ilab)/10.;
		sprintf( chrs, "%8.1f", upos );
		ibeg=-1;
		for( ichr = 0; ichr < 8; ichr++ ) {
            if (chrs[ichr] != ' ') {
				if (ibeg == -1) {
					ibeg=ichr;
				}
				iend=ichr;
			}
		}
		if (chrs[ibeg] == '0') ibeg = min(ibeg+1,iend);
		c_pwrzt (upos,0.,1.05,&chrs[ibeg],iend-ibeg+1, 3,-1,3,0);
	}
/*
 * next, the v axis.
 */
   c_pwrzt (0.,.5,1.1,"V",1,3,2,3,0);

	for( ilab = 1; ilab <=10; ilab++ ) {
		vpos=(float)(ilab)/10.;
		sprintf( chrs, "%8.1f", vpos );
		ibeg=-1;
		for( ichr = 0; ichr < 8; ichr++ ) {
            if (chrs[ichr] != ' ') {
               if (ibeg == -1) {
				   ibeg=ichr;
               }
               iend=ichr;
		   }
		}
		if (chrs[ibeg] == '0') ibeg = min(ibeg+1,iend);
		c_pwrzt (0.,vpos,1.05,&chrs[ibeg],iend-ibeg+1, 3,2,3,0);
	}
/*
 * Finally, the w axis.
 */
	c_pwrzt (1.2,0.,.5,"W",1,3,-1,3,1);

	for( ilab = 0; ilab <=10; ilab++ ) {
		wpos=(float)(ilab)/10.;
		sprintf( chrs, "%8.1f", wpos );
		ibeg=0;
		for( ichr = 0; ichr < 8; ichr++ ) {
            if (chrs[ichr] != ' ') {
				if (ibeg == -1) {
					ibeg=ichr;
				}
				iend=ichr;
            }
		}
		if (chrs[ibeg] == '0') ibeg = min(ibeg+1,iend);
		c_pwrzt (1.05,0.,wpos,&chrs[ibeg],iend-ibeg+1,3,-1,3,1);
	}
/*
 * Using c_point3, draw grids inside the perimeters drawn by c_perim3.
 */
	gset_line_colr_ind(RED);
	for( i = 1; i <=11; i++ ) {
		ptmp=(float)(i-1)/10.;
		for( j = 1; j <=101; j++ ) {
            qtmp=(float)(j-1)/100.;
			c_point3 (ptmp,qtmp,0.);
            c_point3 (qtmp,ptmp,0.);
            c_point3 (ptmp,0.,qtmp);
            c_point3 (qtmp,0.,ptmp);
            c_point3 (0.,ptmp,qtmp);
            c_point3 (0.,qtmp,ptmp);
		}
	}
/*
 * Double the line width and draw a wire-frame representation of the
 * surface defined by the function WFUN, using the routines c_frst3 and
 * c_vect3.
 */
	gset_line_colr_ind(GREEN);
	c_plotif (0.,0.,2);
	gset_linewidth (2.);

	for( i = 1; i <=11; i++ ) {
		utmp=(float)(i-1)/10.;
		c_frst3 (utmp,0.,wfun(utmp,0.));
		for( j = 2; j <= 11; j++ ) {
            vtmp=(float)(j-1)/10.;
            c_vect3 (utmp,vtmp,wfun(utmp,vtmp));
		}
	}
	for( j = 1; j <= 11; j++ ) {
		vtmp=(float)(j-1)/10.;
		c_frst3 (0.,vtmp,wfun(0.,vtmp));
		for( i = 2; i <= 11; i++ ) {
            utmp=(float)(i-1)/10.;
            c_vect3 (utmp,vtmp,wfun(utmp,vtmp));
		}
	}
/*
 * Advance the frame.
 */
	c_frame();
/*
 * Deactivate and close workstation, close GKS.
 */
	gdeactivate_ws(WKID);
	gclose_ws(WKID);
	gclose_gks();
}

/*
 * Declare a function wfun(u,v) to be used in the example.
 */
	
float wfun
#ifdef NeedFuncProto
(float u, float v)
#else
(u,v)
float u, v;
#endif
{
	return((.5+.25*sin(5.*u)+.25*cos(5.*v)));
}


void color()
{
	Gcolr_rep rgb;

/*
 * Black
 */
	rgb.rgb.red = 0.00; rgb.rgb.green = 0.00; rgb.rgb.blue = 0.00;
	gset_colr_rep(WKID,BLACK,&rgb);
/*
 * White
 */
	rgb.rgb.red = 1.00; rgb.rgb.green = 1.00; rgb.rgb.blue = 1.00;
	gset_colr_rep(WKID,WHITE,&rgb);
/*
 * Magenta
 */
	rgb.rgb.red = 1.00; rgb.rgb.green = 0.0; rgb.rgb.blue = 1.00;
	gset_colr_rep(WKID,MAGENTA,&rgb);
/*
 * Blue
 */
	rgb.rgb.red = 0.00; rgb.rgb.green = 0.0; rgb.rgb.blue = 1.00;
	gset_colr_rep(WKID,BLUE,&rgb);
/*
 * Green
 */
	rgb.rgb.red = 0.00; rgb.rgb.green = 1.00; rgb.rgb.blue = 0.00;
	gset_colr_rep(WKID,GREEN,&rgb);
/*
 * Red
 */
	rgb.rgb.red = 1.00; rgb.rgb.green = 0.00; rgb.rgb.blue = 0.00;
	gset_colr_rep(WKID,RED,&rgb);

	return;
}
