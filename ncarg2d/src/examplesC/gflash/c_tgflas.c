/*
 *	$Id: c_tgflas.c,v 1.4 1995-06-14 13:59:24 haley Exp $
 */
#include <stdio.h>
#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define IWTYPE 1
#define WKID   1

main()
{
    int idum,ierr;
/*
 * OPEN GKS, OPEN WORKSTATION OF TYPE 1, ACTIVATE WORKSTATION
 */
    gopen_gks ("stdout",0);
    gopen_ws( WKID, NULL, IWTYPE);
    gactivate_ws( WKID );
/*
 * INVOKE DEMO DRIVER
 */
    tgflas(&ierr);
/*
 *     DEACTIVATE AND CLOSE WORKSTATION, CLOSE GKS.
 */
    gdeactivate_ws(WKID);
    gclose_ws(WKID);
    gclose_gks();
}

tgflas(ierror)
int *ierror;
{
    float twopi,x0c,y0c,rc;
    int nptc, nptp,i;
    float x0p,y0p,rp,xc,yc;
    float x0s,y0s,rs;
    float dtheta,ang;
    extern double sin(double), cos(double);
/*
 *  Data for the graphical objects.
 */
    twopi= 6.283185;
    x0c= .325; y0c = .5; rc=.1; nptc =210;
    x0p= .500; y0p = .5; rp=.35; nptp =16;
    x0s= .675; y0s = .5; rs=.1;
/*
 * Establish the viewport and window.
 */
    c_set(0.,1.,0.,1.,0.,1.,0.,1.,1);
/*
 *  Initialize the GFLASH package.  If using a non-NCAR GKS package
 *  the final argument in the following call should be replaced with
 *  the workstation type for WISS.
 */
    gopen_ws(9,"",3);
/*
 *  Put a circle in a buffer with identifier 1.
 */
    c_gflas1(1);
    dtheta = twopi/nptc;
    c_frstpt(x0c+rc,y0c);
    for(i=1;i<=nptc;i++){
        ang = dtheta*(float)(i);
        xc = rc*cos((double)ang);
        yc = rc*sin((double)ang);
        c_vector(x0c+xc,y0c+yc);
    }
    c_gflas2();
/*
 *  Put a polygonal fan in a buffer with identifier 2.
 */
    c_gflas1(2);
    dtheta = twopi/nptp;
    for(i=1;i<=nptp;i++){
        ang = dtheta*(float)i;
        xc = rp*cos((double)ang);
        yc = rp*sin((double)ang);
        c_line(x0p,y0p,x0p+xc,y0p+yc);
    }
    c_gflas2();
/*
 *  Put a square in a buffer with identifier 3.
 */
    c_gflas1(3);
    c_frstpt(x0s+rs,y0s+rs);
    c_vector(x0s-rs,y0s+rs);
    c_vector(x0s-rs,y0s-rs);
    c_vector(x0s+rs,y0s-rs);
    c_vector(x0s+rs,y0s+rs);
    c_gflas2();
/*
 *  Put a background perimeter in a buffer with identifier 4.
 */
    c_gflas1(4);
    c_frstpt( 0.0, 0.0);
    c_vector( 1.0, 0.0);
    c_vector( 1.0, 1.0);
    c_vector( 0.0, 1.0);
    c_vector( 0.0, 0.0);
    c_gflas2();
/*
 *  Create plots.
 *
 *  Frame 1 -- title, circle, and fan.
 *
 */
    c_plchlq(0.5,0.91,"FRAME 1",25.,0.,0.);
    c_gflas3(1);
    c_gflas3(2);
    c_gflas3(4);
    c_frame();
/*
 *  Frame 2 -- fan, title, and square.
 */
    c_gflas3(2);
    c_plchlq(0.5,0.91,"FRAME 2",25.,0.,0.);
    c_gflas3(3);
    c_gflas3(4);
    c_frame();
/*
 *  Frame 3 -- circle, square, fan, and title (note that the change
 *             in window affects the PLCHLQ call, but not the elements
 *             in the buffers -- this illustrates the independent
 *             nature of the FLASH buffers).
 */
    c_set (0.,1.,0.,1.,0.,10.,0.,10.,1);
    c_gflas3(1);
    c_gflas3(3);
    c_gflas3(2);
    c_plchlq(5.0,9.1,"FRAME 3",25.,0.,0.);
    c_gflas3(4);
    c_frame();
/*
 *  Close the GFLASH package.
 */
    gclose_ws(9);
    *ierror = 1;
    return(1);
}
