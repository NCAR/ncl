/*
 *	$Id: c_fthex05.c,v 1.1 1994-10-31 04:15:18 haley Exp $
 */
#include <stdio.h>
#include <math.h>

#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define BLACK   0
#define WHITE   1
#define MAGENTA 2
#define BLUE    3
#define GREEN   4
#define RED     5

#define NCRV  361

/*
 * Declare a function W(U,V) to be used in the example.
 */
#define wfun(u,v)   (.5+.25*sin(5.*(u))+.25*cos(5.*(v)))
#define min(x,y)     ((x) < (y) ? (x) : (y))

#define WSTYPE SED_WSTYPE
#define WKID   1

/*
 * Declare the common block in which the viewing distance will be
 * communicated to THREED.
 */
#if !defined (cray)
extern struct common1 {
#else
struct common1 {
#endif
	float rzro;
} NGCALLF(temprt,TEMPRT);

/*
 * Declare the common block in which the internal parameters of
 * SRFACE are stored.
 */
#if !defined (cray)
extern struct common2 {
#else
struct common2 {
#endif
    int ifr,istp,irots,idrx,idry,idrz,iupper,iskirt,ncla;
    float theta,hskirt,chi,clo,cinc;
    int ispval;
} NGCALLF(srfip1,SRFIP1);

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
	float peye[6];
/*
 * Declare arrays in which to define the surface to be drawn by
 * SRFACE and to provide the workspace it needs.
 */
	float u[21],v[21],w[21][21];
    int rwrk[882];
/*
 * Define a character variable in which to form numeric labels.
 */
	char chrs[9];
/*
 * Turn off frame advances by SRFACE.
 */
	NGCALLF(srfip1,SRFIP1).ifr=0;
/*
 * Turn on the drawing of skirts by SRFACE and set the base value
 * for the skirts.
 */
	NGCALLF(srfip1,SRFIP1).iskirt=1;
	NGCALLF(srfip1,SRFIP1).hskirt=0.;
/*
 * Define the surface to be drawn.
 */
	for( i = 0; i < 21; i++ ) {
		u[i] = (float)i/20.;
	}
	for( j = 0; j < 21; j++ ) {
		v[j]=(float)j/20.;
	}

	for( i = 0; i < 21; i++ ) {
		utmp=(float)i/20.;
		for( j = 0; j < 21; j++ ) {
            vtmp=(float)j/20.;
            w[j][i]=wfun(utmp,vtmp);
		}
	}
/*
 * Open GKS.
 */
	gopen_gks("stdout",0);
	gopen_ws(WKID, NULL, WSTYPE);
	gactivate_ws(WKID);
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
 * Define the distance from which the box, when viewed from the direction
 * that makes it biggest, should just fill the screen.
 */
	NGCALLF(temprt,TEMPRT).rzro=8.;
/*
 * Define the position of the eye.
 */
	peye[0] = 7.;
	peye[1] = 5.;
	peye[2] = 3.;
/*
 * Define the position of the point looked at to be the point at the
 * center of the box.  These three values in the array are seen only
 * by SRFACE; THREED automatically assumes that you're looking at the
 * center of the box.
 */
	peye[3]=.5*(umin+umax);
	peye[4]=.5*(vmin+vmax);
	peye[5]=.5*(wmin+wmax);
/*
 * Communicate to SRFACE the dimensions of the box and the distance from
 * which it is to be viewed.
 */
	c_setr   (umin,umax,vmin,vmax,wmin,wmax,NGCALLF(temprt,TEMPRT).rzro);
/*
 * Initialize THREED, using arguments that will make it use exactly the
 * same projection as SRFACE.
 */
	c_set3(.0087976,.9902248,.0087976,.9902248,umin,umax,vmin,vmax,wmin,wmax,peye);
/*
 * Draw perimeters in each of the three coordinate planes.
 */
	gset_line_colr_ind(RED);
	c_perim3 (10,2,10,2,1,0.);
	c_perim3 (10,2,10,2,2,0.);
	c_perim3 (10,2,10,2,3,0.);
/*
 * Put some labels on the plot.  First, the U axis.
 */
	gset_line_colr_ind(WHITE);
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
 *  Double the line width and draw the surface.
 */ 
	c_plotif (0.,0.,2);
	gset_linewidth (2.);

	c_srface (u,v,(float *)w,&rwrk[0],21,21,21,peye,0.);
/*
 * Advance the frame.
 */
	c_frame();
/*
 * Close GKS.
 */
	gdeactivate_ws(WKID);
	gclose_ws(WKID);
	gclose_gks();
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
