/*
 *	$Id: c_fthex03.c,v 1.1 1994-05-24 22:42:25 haley Exp $
 */
#include <stdio.h>
#include <math.h>

#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define NCRV  361

/*
 * Declare a function W(U,V) to be used in the example.
 */
#define wfun(u,v)   (.5+.25*sin(5.*(u))+.25*cos(5.*(v)))

#define min(x,y)     ((x) < (y) ? (x) : (y))
#define max(x,y)     ((x) > (y) ? (x) : (y))

#define WHITE   0
#define BLACK   1
#define MAGENTA 2
#define BLUE 3
#define GREEN  4
#define RED  5

main()
{
	int i, j, ilab, ichr, ibeg, iend;
	float temp, utmp, vtmp, wtmp, ptmp, qtmp;
	float upos, vpos, wpos;
	float umin, umax, vmin, vmax, wmin, wmax;
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
 * Declare arrays in which to put coordinates for a curve to be drawn.
 */
	float ucrv[NCRV],vcrv[NCRV],wcrv[NCRV];
/*
 * Open GKS.
 */
	c_opngks();
/*
 * Define color table
 */
	color();
/*
 * Make the tick marks drawn by PERIM3 twice as long as the default.
 */
	c_tick43 (24,16,24,16,24,16);
/*
 * Define the boundaries of the box to be projected from 3-space to
 * 2-space.
 */
	umin = 0.;
	umax = 1.;
	vmin = 0.;
	vmax = 1.;
	wmin = 0.;
	wmax = 1.;
/*
 * Define the position of the eye.
 */
	peye[0] = 6.;
	peye[1] = 4.;
	peye[2] = 5.;
/*
 * Initialize THREED.
 */
	c_set3 (.1,.9,.1,.9,umin,umax,vmin,vmax,wmin,wmax,peye);
/*
 * Draw perimeters in each of the three coordinate planes.
 */
	gset_line_colr_ind(GREEN);
	c_perim3 (10,2,10,2,1,0.);
	c_perim3 (10,2,10,2,2,0.);
	c_perim3 (10,2,10,2,3,0.);
/*
 * Put some labels on the plot.  First, the U axis.
 */
	gset_line_colr_ind(BLACK);
	c_pwrzt (.5,0.,1.1,"U",1,3,-1,3,0);

	for( ilab = 1; ilab <= 10; ilab++ ) {
		upos = (float)ilab/10.;
		sprintf(chrs,"%8.1f", upos );
		ibeg = -1;
		for( ichr = 0; ichr < 8; ichr++ ) {
            if (chrs[ichr] != ' ') {
				if (ibeg == -1) {
					ibeg = ichr;
				}
				iend = ichr;
			}
		}
		if (chrs[ibeg] == '0') ibeg = min(ibeg+1,iend);
		c_pwrzt (upos,0.,1.05,&chrs[ibeg],iend-ibeg+1,3,-1,3,0);
	}
/*
 * Next, the V axis.
 */
	c_pwrzt (0.,.5,1.1,"V",1,3,2,3,0);

	for( ilab = 1; ilab <= 10; ilab++ ) {
		vpos = (float)ilab/10.;
		sprintf( chrs, "%8.1f", vpos );
		ibeg = -1;
		for( ichr = 0; ichr < 8; ichr++ ) {
            if (chrs[ichr] != ' ') {
				if (ibeg == -1) {
					ibeg = ichr;
				}
				iend = ichr;
			}
		}
		if (chrs[ibeg] == '0') ibeg = min(ibeg+1,iend);
		c_pwrzt (0.,vpos,1.05,&chrs[ibeg],iend-ibeg+1,3,2,3,0);
	}
/*
 * Finally, the W axis.
 */
	c_pwrzt (1.2,0.,.5,"W",1,3,-1,3,1);

	for( ilab = 0; ilab <= 10; ilab++ ) {
        wpos = (float)ilab/10.;
		sprintf(chrs,"%8.1f", wpos );
        ibeg = -1;
		for( ichr = 0; ichr < 8; ichr++ ) {
            if (chrs[ichr] != ' ') {
				if (ibeg == -1) {
					ibeg = ichr;
				}
				iend = ichr;
			}
		}
		if (chrs[ibeg] == '0') ibeg = min(ibeg+1,iend);
		c_pwrzt (1.05,0.,wpos,&chrs[ibeg],iend-ibeg+1, 3,-1,3,1);
	}
/*
 * Using POINT3, draw grids inside the perimeters drawn by PERIM3.
 */
	gset_marker_colr_ind(GREEN);
	for( i = 1; i <= 11; i++ ) {
		ptmp = (float)(i-1)/10.;
		for( j = 1; j <= 101; j++ ) {
            qtmp = (float)(j-1)/100.;
            c_point3 (ptmp,qtmp,0.);
            c_point3 (qtmp,ptmp,0.);
            c_point3 (ptmp,0.,qtmp);
            c_point3 (qtmp,0.,ptmp);
            c_point3 (0.,ptmp,qtmp);
            c_point3 (0.,qtmp,ptmp);
		}
	}
/*
 * Draw a wire-frame representation of the surface defined by the
 * function WFUN, using the routines FRST3 and VECT3.
 */
	gset_line_colr_ind(RED);
	for( i = 1; i <= 11; i++ ) {
		utmp = (float)(i-1)/10.;
		c_frst3 (utmp,0.,wfun(utmp,0.));
		for( j = 2; j <= 11; j++ ) {
            vtmp = (float)(j-1)/10.;
            c_vect3 (utmp,vtmp,wfun(utmp,vtmp));
		}
	}

	for( j = 1; j <= 11; j++ ) {
		vtmp = (float)(j-1)/10.;
		c_frst3 (0.,vtmp,wfun(0.,vtmp));
		for( i = 2; i <= 11; i++ ) {
            utmp = (float)(i-1)/10.;
            c_vect3 (utmp,vtmp,wfun(utmp,vtmp));
		}
	}
/*
 * Double the line width and draw a curve using CURVE3.
 */
	gset_line_colr_ind(BLUE);
	c_plotif (0.,0.,2);
	gset_linewidth (2.);

	for( i = 0; i < NCRV; i++ ) {
		temp = 5.*(float)i;
		ucrv[i] = .5+temp/3600.*cos(.017453292519943*temp);
		vcrv[i] = .5+temp/3600.*sin(.017453292519943*temp);
		wcrv[i] = wfun(ucrv[i],vcrv[i]);
	}
    c_curve3 (ucrv,vcrv,wcrv,NCRV);
/*
 * Advance the frame.
 */
	c_frame();
/*
 * Close GKS.
 */
	c_clsgks();
/*
 * Done.
 */
}

void color()
{
	Gcolr_rep rgb;

/*
 * Black
 */
	rgb.rgb.red = 0.00; rgb.rgb.green = 0.00; rgb.rgb.blue = 0.00;
	gset_colr_rep(1,BLACK,&rgb);
/*
 * White
 */
	rgb.rgb.red = 1.00; rgb.rgb.green = 1.00; rgb.rgb.blue = 1.00;
	gset_colr_rep(1,WHITE,&rgb);
/*
 * Blue
 */
	rgb.rgb.red = 0.00; rgb.rgb.green = 0.0; rgb.rgb.blue = 1.00;
	gset_colr_rep(1,BLUE,&rgb);
/*
 * Green
 */
	rgb.rgb.red = 0.00; rgb.rgb.green = 1.00; rgb.rgb.blue = 0.00;
	gset_colr_rep(1,GREEN,&rgb);
/*
 * Red
 */
	rgb.rgb.red = 1.00; rgb.rgb.green = 0.00; rgb.rgb.blue = 0.00;
	gset_colr_rep(1,RED,&rgb);

	return;
}
