/*
 *  $Id: c_vvex03.c,v 1.3 2004-08-01 17:12:46 haley Exp $
 */
#include <stdio.h>
#include <math.h>

#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define IWTYPE 1
#define WKID   1

/*
 * This example illustrates use of the user-modifiable routine
 * VVUMXY to create user-defined mappings of vector data.
 * The first frame demonstrates a method of plotting data contained
 *  in an irregularly spaced rectangular grid. 
 * The second frame plots scattered (non-gridded) data.
 * The third frame maps scattered data through an EZMAP projection.
 */

#define PI   3.14159
#define MAXDIM  100

float xcoord[MAXDIM],ycoord[MAXDIM];

#if !defined (cray)
extern struct common {
#else
struct common {
#endif
	int imap;
	float xvpl,xvpr,yvpb,yvpt,wxmn,wxmx,wymn,wymx;
	float xlov,xhiv,ylov,yhiv,sxdc,sydc;
	int nxct,nyct;
	float rlen;
	int lnlg,invx,invy,itrt,iwct;
	float fw2w,fh2h,dvmn,dvmx,rbig;
	int ibig;
} NGCALLC(vvmap,VVMAP);

main()
{
	extern void irrex(), sctrex(), scezex();
/*
 * open gks, open workstation of type 1, activate workstation
 */
    gopen_gks("stdout",0);
    gopen_ws(WKID, NULL, IWTYPE);
    gactivate_ws(WKID);
/*
 * draw an irregularly gridded vector plot.
 */
	irrex();
/*
 * draw a scattered vector plot.
 */
	sctrex();
/*
 * draw a plot of scattered vectors mapped through an ezmap projection.
 */
	scezex();
/*
 * deactivate and close workstation, close gks.
 */
    gdeactivate_ws (WKID);
    gclose_ws(WKID);
    gclose_gks();
}


#define M   10
#define N   10

/*
 * =============================================================
 */
void irrex()
{
	float vmn, vmx, dmx, vl, vr, vb, vt, ul, ur, ub, ut;
/*
 * arrays to hold the coordinate data.
 */
	float xcloc[M] = {0.0,7.5,20.0,40.0,45.0,65.0,70.0,77.0,90.0,95.0};
	float ycloc[N] = {12.5,22.5,30.0,35.0,52.0,58.0,70.0,75.0,85.0,90.0};

	int ll, idm, icn;
	float u[N][M], v[N][M], xdm, ydm, x, y;
	float xgrid = 100., ygrid = 100.;
	int i, j, ierr;
	float gxsize, gysize, vszadj, vrl;
/*
 * specify the ndc coordinates for a plot title.
 */
	float fx = 0.5, fy = 0.975;
/*
 * set up a functionally defined vector field
 */
	gxsize = PI/xgrid;
	gysize = PI/ygrid;
	for( i = 0; i < M; i++ ) {
		for( j = 0; j < N; j++ ) {
            u[j][i] = cos(gxsize*(xcloc[i]));
            v[j][i] = sin(gysize*(ycloc[j]));
		}
	}
/*
 * copy coordinate values into the vvuser common block arrays, in
 * order to make the data accessible to vvumxy.
 */
	for( i = 0; i < M; i++ ) {
		xcoord[i] = xcloc[i];
	}

	for( i = 0; i < N; i++ ) {
		ycoord[i] = ycloc[i];
	}
/*
 * set the user space (arguments 5,6,7, and 8 of the set call) to contain 
 * the extremes of the values assigned to the coordinate arrays; then
 * tell vectors not to do a set call.
 */ 
	c_set(0.05,0.95,0.05,0.95,0.0,100.0,0.0,100.0,1);
	c_vvseti("set - do-set-call flag", 0);
/*
 * set the data coordinate boundaries equal to the grid coordinate
 * boundaries (i.e. the range of the array indexes). this is actually
 * the default condition for vectors, but is included for emphasis here.
 */
	c_vvsetr("xc1 -- lower x bound", 1.0);
	c_vvsetr("xcm -- upper x bound", (float)M);
	c_vvsetr("yc1 -- lower y bound", 1.0);
	c_vvsetr("ycn -- upper y bound", (float)N);
/*
 * set the map parameter to a value indicating a user-defined mapping
 * (anything other than 0, 1, or 2). the routine vvumxy must be modified
 * to recognize this value for map.
 */
	c_vvseti("map - user-defined mapping", 3) ;
	idm=0;
	xdm = ydm = 0.0;
	c_vvinit((float *)u,M,(float *)v,M,&xdm,idm,M,N,&ydm,idm);
/*
 * for an irregular grid the default vector sizes may not be appropriate.
 * the following (admittedly cumbersome) code adjusts the size relative 
 * to the default size chosen by vectors. it must follow vvinit.
 */
	vszadj = 0.75;
	c_vvgetr("dmx - ndc maximum vector size", &dmx);
	c_getset(&vl,&vr,&vb,&vt,&ul,&ur,&ub,&ut,&ll);
	vrl = vszadj * dmx / (vr - vl);
	c_vvsetr("vrl - vector realized length", vrl);

	c_vvectr((float *)u,(float *)v,&xdm,&idm,0,&ydm);
	c_perim(1,0,1,0);
/*
 * save the current normalization transformation then set it to 0.
 */
	ginq_cur_norm_tran_num(&ierr,&icn);
	gsel_norm_tran(0);
	x = c_cfux(fx);
	y = c_cfuy(fy);
/*
 * call plchlq to write the plot title.
 */
	c_plchlq (x,y,"irregularly gridded vector data", 16.,0.,0.);
/*
 * restore the normalization transformation.
 */
	gsel_norm_tran(icn);
	c_frame();
    return;
}

/*
 * ==================================================================
 */
void sctrex()
{
	float vmn, vmx, dmx, vl, vr, vb, vt, ul, ur, ub, ut;
	int idm, ll;
	float xdm, ydm, x, y;
	float vszadj, vrl;
/*
 * in this routine the u and v arrays are treated as single-dimensioned
 * entities. the location of the vector with components u(i),v(i) is
 * specified by xcloc(i),ycloc(i). the coordinate locations are copied
 * to the common block vvuser in order to make them accessible to the
 * the user-defined mapping routine, vvumxy.
 *
 * since vectors expects 2-d arrays for u and v, the input parameters
 * m and n multiplied together should equal the total number of elements 
 * in each of the 1-d data arrays.
 *
 * all data lies within a 100 x 100 square (origin 0.0 at lower left).
 */
    int i, ierr, icn, m = 10, n = 1;
/*
 * specify the ndc coordinates for a plot title.
 */
    float xcloc[10]={40.0,7.5,20.0,35.0,45.0,65.0,75.0,80.0,90.0,95.0};
    float ycloc[10]={50.0,80.0,20.0,60.0,15.0,80.0,20.0,45.0,60.0,50.0};
    float u[10]={.3,.6,.8,.2,.9,.8,-.4,-.3,.1,.2};
    float v[10]={-.2,.1,.7,.6,.3,-.4,-.6,-.8,-.9,-.5};
    float fx = 0.5, fy = 0.975;
/*
 * copy coordinate values into the vvuser common block arrays.
 */
    for( i=1; i <= m*n; i++ ) {
        xcoord[i] = xcloc[i];
        ycoord[i] = ycloc[i];
    }
/*
 * set the user space (arguments 5,6,7, and 8 of the set call) to contain 
 * the extremes of the values assigned to the coordinate arrays; then
 * tell vectors not to do a set call.
 */ 
    c_set(0.075,0.925,0.1,0.95,0.0,100.0,0.0,100.0,1);
    c_vvseti("set - do-set-call flag", 0);
/*
 * set the data coordinate boundaries equal to the grid coordinate
 * boundaries (i.e. the range of the array indexes). this is actually
 * the default condition for vectors, but is included for emphasis here.
 */
    c_vvsetr("xc1 -- lower x bound", 1.0);
    c_vvsetr("xcm -- upper x bound", (float)m);
    c_vvsetr("yc1 -- lower y bound", 1.0);
    c_vvsetr("ycn -- upper y bound", (float)n);
/*
 * set the map parameter to a value indicating a user-defined mapping
 * (anything other than 0, 1, or 2). the routine vvumxy must be modified
 * to recognize this value for map.
 */
    c_vvseti("map - user-defined mapping", 4);
    idm=0;
    c_vvinit((float *)u,m,(float *)v,m,&xdm,idm,m,n,&ydm,idm);
/*
 * for an scattered grid the default vector sizes may not be appropriate.
 * the following (admittedly cumbersome) code adjusts the size relative 
 * to the default size chosen by vectors. it must follow vvinit.
 */
    vszadj = 0.1;
    c_vvgetr("dmx - ndc maximum vector size", &dmx);
	c_getset(&vl,&vr,&vb,&vt,&ul,&ur,&ub,&ut,&ll);
    vrl = vszadj * dmx / (vr - vl);
    c_vvsetr("vrl - vector realized length", vrl);

    c_vvectr((float *)u,(float *)v,&xdm,&idm,0,&ydm);
    c_perim(1,0,1,0);
/*
 * save the current normalization transformation then set it to 0.
 */
	ginq_cur_norm_tran_num(&ierr,&icn);
	gsel_norm_tran(0);
    x = c_cfux(fx);
    y = c_cfuy(fy);
/*
 * call plchlq to write the plot title.
 */
    c_plchlq (x,y,"scattered vector data", 16.,0.,0.);
/*
 * restore the normalization transformation.
 */
    gsel_norm_tran(icn);
    c_frame();
    return;
}
/*
 * =================================================================
 */
void scezex()
{
    float p1[2],p2[2],p3[2],p4[2];
	float vmn, vmx, dmx, vl, vr, vb, vt, ul, ur, ub, ut;
	float xdm, ydm;
	int idm;
/*
 * this routine maps geographically scattered vector data
 * through an ezmap projection. in order to make the effect of the 
 * projection more obvious the vectors are given uniform direction 
 * and magnitude.
 */  
    int m=20, n=1;
/*
 * all data lies within the region latitude 0.0,90.0 and 
 * and longitude 0.0,100.0.
 */
    float xcloc[20]={40.0,7.5,20.0,35.0,40.0,65.0,75.0,80.0,90.0,95.0,50.0,80.0,20.0,60.0,15.0,80.0,20.0,45.0,60.0,50.0};
    float ycloc[20]={50.0,80.0,20.0,60.0,15.0,80.0,20.0,45.0,60.0,50.0,40.0,7.5,20.0,35.0,45.0,65.0,75.0,13.0,24.0,32.0};
	float u[20], v[20];
/*
 * specify the ndc coordinates for a plot title.
 */
    float fx = 0.5, fy = 0.975, vszadj, vrl, x, y;
/*
 * copy coordinate values into the vvuser common block arrays.
 */
    int i, j, ll, ierr, icn;
    for( i = 0; i < m*n; i++ ) {
        u[i] = v[i] = 1.0;
        xcoord[i] = xcloc[i];
        ycoord[i] = ycloc[i];
    }
/*
 * set up a satellite view ezmap projection. draw the map grid only.
 */
    p1[0] = p2[0] = p3[0] = p4[0] = 0.;
	c_mapset("MA",p1,p2,p3,p4);
	c_maproj("sv",45.0,50.0,0.0);
	c_mapint();
	c_mapgrd();
/*
 * don't let vectors do a set call
 */
	c_vvseti("set - do-set-call flag", 0);
/*
 * set the data coordinate boundaries equal to the grid coordinate
 * boundaries (i.e. the range of the array indexes). this is actually
 * the default condition for vectors, but is included for emphasis here.
 */
	c_vvsetr("xc1 -- lower x bound", 1.0);
	c_vvsetr("xcm -- upper x bound", (float)m);
	c_vvsetr("yc1 -- lower y bound", 1.0);
	c_vvsetr("ycn -- upper y bound", (float)n);
/*
 * set the map parameter to a value indicating a user-defined mapping
 * (anything other than 0, 1, or 2). the routine vvumxy must be modified
 * to recognize this value for map.
 */
	c_vvseti("map - user-defined mapping", 5) ;
	c_vvsetr("lwd - vector linewidth", 2.0) ;
	idm=0;
	c_vvinit((float *)u,m,(float *)v,m,&xdm,idm,m,n,&ydm,idm);
/*
 * adjust vector sizes.
 */
	vszadj = 0.1;
	c_vvgetr("dmx - ndc maximum vector size", &dmx);
	c_getset(&vl,&vr,&vb,&vt,&ul,&ur,&ub,&ut,&ll);
	vrl = vszadj * dmx / (vr - vl);
	c_vvsetr("vrl - vector realized length", vrl);

	c_vvectr((float *)u,(float *)v,&xdm,&idm,0,&ydm);
	c_perim(1,0,1,0);
/*
 * save the current normalization transformation then set it to 0.
 */
	ginq_cur_norm_tran_num(&ierr,&icn);
	gsel_norm_tran(0);
    x = c_cfux(fx);
    y = c_cfuy(fy);
/*
 * call plchlq to write the plot title.
 */
	c_plchlq (x,y,"scattered vector data mapped through an ezmap projection",16.,0.,0.);
/*
 * restore the normalization transformation.
 */
	gsel_norm_tran(icn);
	c_frame();

	return;
}
/*
 * =================================================================
 *
 * modified version of vvumxy that implements mapping for the 
 * following values of the vectors 'map' internal parameter:
 * 
 * 3 - irregular rectangular grid mapping
 * 4 - scattered data mapping
 * 5 - scattered data mapped through an ezmap projection.
 */

#define PDTOR   0.017453292519943
#define PRCFAC  1e5
#define PVFRAC  0.001
#define PFOVFL  1e12
#define IPMXCT  40
#define PDUVML  2.0
#define IPCTST  PRCFAC*90


int NGCALLF(vvumxy,VVUMXY)(x,y,u,v,uvm,xb,yb,xe,ye,ist)
float *x, *y, *u, *v, *uvm, *xb, *yb, *xe, *ye;
int *ist;
{
	int i, j;
	float xt, yt;
	float duv, clt, sgn, dv1, t;
	int ict;
/*
 * this is a user modifiable routine that allows custom projections of
 * the vector space. x and y give the vector position within the domain
 * of the data space. by default, this space is coincident with the
 * grid space (i.e. 1 through dimension lengths of the u and v arrays).
 * the vector endpoints are output in fractional coordinates (ndc space).
 * note that this is different from the old mxf and myf routines, which
 * output in 'plotter coordinate' space. it also differs from the 
 * conpack routine cpmpxy, which returns values in user space. 
 * 
 * vvumxy (velocity vector -- user map x,y) is called whenever 
 * the internal parameter map is set to a value other than 0, 1, or 2.
 *
 * based on the magnitude and direction of the vector the start and 
 * ending points of the vector are returned in ndc space.
 *
 * input parameters:
 *
 * x,y   -- vector position in the user coordinate system
 * u,v   -- vector components from the u,v arrays for this position
 * uvm   -- magnitude of the u,v components (supplied for convenience
 *          and efficiency - but note that many mappings do not need 
 *          this value)
 *
 * output parameters:
 *
 * xb,yb -- starting point of the vector in fractional coordinates
 *          (ndc space)
 * xe,ye -- ending point of the vector in fractional coordinates
 *          (ndc space)
 * ist   -- status results of the mapping: 0 indicates success -- any
 *          non-zero value causes vvectr to discard the vector at this
 *          location
 *
 * the mapping common block: made available to user mapping routines.
 * note: all these variables should be considered read-only by vvumxy.
 *
 * description of vvmap contents:
 *
 * imap                - value of the internal parameter 'map'
 * xvpl,xvpr,yvpb,yvpt - the currently set viewport values. (getset
 *                       arguments 1, 2, 3, and 4)
 * wxmn,wxmx,wymn,wymx - the min and max boundaries of user coordinate
 *                       space, (usually but not always equivalent to
 *                       window coordinates). wxmn and wymn are true
 *                       minimum values even one or both axes is 
 *                       inverted. (i.e. they are equivalent to getset
 *                       arguments 5,6,7, and 8 sorted numerically)
 * xlov,xhiv,ylov,yhiv - min and max boundaries of the data space, by
 *                       default equivalent to the array grid space.
 *                       xlov and ylov are not necessarily less than 
 *                       xhiv and yhiv.
 * sxdc,sydc           - scaling factors for converting vector component
 *                       values into lengths in ndc space.
 * nxct,nyct           - length of each dimension of the u and v 
 *                       component arrays.
 * rlen                - length of the maximum vector in user 
 *                       coordinates.
 * lnlg                - the linear/log mode (getset argument 9)
 * invx,invy           - user coordinates inversion flags: 
 *                       0 - not inverted, 1 - inverted
 * itrt                - value of the internal parameter trt
 * iwct                - not currently used
 * fw2w,fh2h           - scale factors for converting from fraction of
 *                       viewport width/height to ndc width/height 
 * dvmn,dvmx           - min/max vector lengths in ndc
 * rbig,ibig           - machine dependent maximum real/integer values
 *
 * math constants:
 *
 * local parameters (used for the ezmap projection only):
 *
 * PRCFAC - precision factor used to resolve float equality within
 *            the precision of a 4 byte real
 * PVFRAC - initial fraction of the vector magnitude used to
 *            determine the differential increment
 * PFOVFL - floating point overflow value
 * IPMXCT - number of times to allow the differential to increase
 * PDUVML - multiplier when the differential is increased
 * pcstst - test value for closeness to 90 degree latitude
 *
 * ---------------------------------------------------------------------
 */
    if (NGCALLC(vvmap,VVMAP).imap == 3) {
/*
 * mapping for irregular rectangular gridded vector data.
 * 
 * since the array grid and input data space are coincident in this
 * case, x and y converted to integers serve as the index into the 
 * coordinate arrays that define the vector location in the user
 * coordinate system. 
 * this code includes more tests that may be necessary in
 * production environments: it does so partly to illustrate how to use
 * some of the contents of the vvmap common block.
 */
        i = (int)(*x+.5);
        j = (int)(*y+.5);
/*
 * nxct and nyct contain the number of elements along each coordinate
 * axis. therefore the following test ensures that i and j are within
 * the domain of the array dimensions.
 */
        if (i < 1 || i > NGCALLC(vvmap,VVMAP).nxct || j < 1 || j > NGCALLC(vvmap,VVMAP).nyct) {
            *ist = -1;
            return(0);
        }
        xt = xcoord[i-1];
        yt = ycoord[j-1];
/*
 * wxmn, wxmx, wymn, and wymx contain the minimum and maximum values of
 * the user coordinate space. the following test ensures that the 
 * coordinate values in the array are within the current boundaries
 * of the user space.
 */
        if (xt < NGCALLC(vvmap,VVMAP).wxmn || xt > NGCALLC(vvmap,VVMAP).wxmx || yt < NGCALLC(vvmap,VVMAP).wymn || yt > NGCALLC(vvmap,VVMAP).wymx) {
            *ist = -1;
            return(0);
        }
        *xb = c_cufx(xt);
        *yb = c_cufy(yt);
        *xe = *xb + *u * NGCALLC(vvmap,VVMAP).sxdc;
        *ye = *yb + *v * NGCALLC(vvmap,VVMAP).sydc;
/*
 * ---------------------------------------------------------------------
 */
    }
    else if (NGCALLC(vvmap,VVMAP).imap == 4) {
/*
 * mapping for scattered vector data.
 */ 
        i = (int)(*x+.5);
        j = (int)(*y+.5);
        
        if (i < 1 || i > NGCALLC(vvmap,VVMAP).nxct || j < 1 || j > NGCALLC(vvmap,VVMAP).nyct) {
            *ist = -1;
            return(0);
        }
/*
 * since xcoord and ycoord are actually single dimensional arrays,
 * convert the 2-d indexes supplied to vvumxy into their 1-d equivalent 
 * to index into the coordinate arrays.
 */
        xt = xcoord[NGCALLC(vvmap,VVMAP).nxct*(j-1)+i-1];
        yt = ycoord[NGCALLC(vvmap,VVMAP).nxct*(j-1)+i-1];

        if (xt < NGCALLC(vvmap,VVMAP).wxmn || xt > NGCALLC(vvmap,VVMAP).wxmx ||  yt < NGCALLC(vvmap,VVMAP).wymn || yt > NGCALLC(vvmap,VVMAP).wymx) {
            *ist = -1;
            return(0);
        }
        *xb = c_cufx(xt);
        *yb = c_cufy(yt);
        *xe = *xb + *u * NGCALLC(vvmap,VVMAP).sxdc;
        *ye = *yb + *v * NGCALLC(vvmap,VVMAP).sydc;
    }
/*
 * ---------------------------------------------------------------------
 */
    else if (NGCALLC(vvmap,VVMAP).imap == 5) {
/*
 * mapping for scattered vector data projected through ezmap. xcoord and
 * ycoord contain the longitude and latitude respectively of each vector
 * datum. 
 */
        i = (int)(*x+.5);
        j = (int)(*y+.5);

        if (i < 1 || i > NGCALLC(vvmap,VVMAP).nxct || j < 1 || j > NGCALLC(vvmap,VVMAP).nyct) {
            *ist = -1;
            return(0);
        }
/*
 * since xcoord and ycoord are actually single dimensional arrays,
 * convert the 2-d indexes supplied to vvumxy into their 1-d equivalent 
 * to index into the coordinate arrays.
 */
        xt = xcoord[NGCALLC(vvmap,VVMAP).nxct*(j-1)+i-1];
        yt = ycoord[NGCALLC(vvmap,VVMAP).nxct*(j-1)+i-1];
/*
 * the following code is adapted from the ezmap projection code in 
 * vvmpxy. an iterative technique is used that handles most vectors 
 * arbitrarily close to the projection limb.
 * xt is longitude, yt is latitude.
 *
 * test for 90 degree latitude.
 */
        if ((int)(fabs(yt)*PRCFAC+0.5)==IPCTST) {
            *ist=-1;
            return(0);
        }
/*
 * project the starting value: bail out if outside the window
 */
        c_maptrn (yt,xt,xb,yb);
        if (*xb < NGCALLC(vvmap,VVMAP).wxmn || *xb > NGCALLC(vvmap,VVMAP).wxmx || *yb < NGCALLC(vvmap,VVMAP).wymn || *yb > NGCALLC(vvmap,VVMAP).wymx) {
            *ist=-5;
            return(0);
        }
/*
 * check the vector magnitude
 */
        if ((int)(*uvm*PRCFAC+0.5) == 0) {
            *ist = -2;
            return(0);
        }
/*
 * the incremental distance is proportional to a small fraction
 * of the vector magnitude
 */
        duv=PVFRAC/(*uvm);
        clt=cos(*y*PDTOR);
/*
 * project the incremental distance. if the positive difference doesn't
 * work, try the negative difference. if the difference results in a
 * zero length vector, try a number of progressively larger increments.
 */
        ict=0;
        sgn=1.0;
twenty:

        c_maptrn(yt + sgn * *v * duv, xt + sgn * *u * duv/clt,&xt,&yt);

        dv1=sqrt((xt - *xb)*(xt - *xb)+(yt - *yb)*(yt - *yb));
        if (dv1 > NGCALLC(vvmap,VVMAP).rlen) {
            if (sgn == -1.0) {
                *ist = -4;
                return(0);
            }
            else {
                sgn=-1.0;
                goto twenty;
            }
        }

        if ((int)(dv1*PRCFAC) == 0) {
            if (ict < IPMXCT) {
                ict = ict + 1;
                duv=duv*PDUVML;
                goto twenty;
            }
            else {
                *ist = -3;
                return(0);
            }
        }

        if (fabs(xt) >= PFOVFL || fabs(yt) >= PFOVFL) {
            *ist = -6;
            return(0);
        }

        t   = sgn*((xt - *xb)/dv1)* (*uvm);
        *xb = c_cufx(*xb);
        *xe = *xb + t * NGCALLC(vvmap,VVMAP).sxdc;
        t   = sgn*((yt - *yb)/dv1)* (*uvm);
        *yb = c_cufy(*yb);
        *ye = *yb+t*NGCALLC(vvmap,VVMAP).sydc;
    }
/*
 * ---------------------------------------------------------------------
 */
    else {
/*
 * default mapping:
 *
 * wxmn, wxmx, wymn, and wymx contain the minimum and maximum values of
 * the user coordinate space. somewhat inaccurately, the mmenomic 'w'
 * implies window coordinate space, which is usually (but not always)
 * the same as user coordinate space. but note that even when 
 * the coordinates are reversed, you are guaranteed that wxmn < wxmx
 * and wymn < wymx. this eliminates the need to invoke min and max.
 */
        if (*x < NGCALLC(vvmap,VVMAP).wxmn || *x > NGCALLC(vvmap,VVMAP).wxmx || *y < NGCALLC(vvmap,VVMAP).wymn || *y > NGCALLC(vvmap,VVMAP).wymx) {
            *ist = -1;
            return(0);
        }
        *xb = c_cufx(*x);
        *yb = c_cufy(*y);
        *xe = *xb + *u * NGCALLC(vvmap,VVMAP).sxdc;
        *ye = *yb + *v * NGCALLC(vvmap,VVMAP).sydc;
    }
/*
 * done.
 */
    return(0);

}
