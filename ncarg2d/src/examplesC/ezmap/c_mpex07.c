/*
 *	$Id: c_mpex07.c,v 1.1 1994-05-13 14:26:37 haley Exp $
 */
#include <stdio.h>
#include <math.h>

/*
 * Include function prototypes
 */
#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

main()
{
    float p1[2],p2[2],p3[2],p4[2];
    int lval;
    extern void bndary();
/*
 * This program produces a stereographic view of the North Pole,
 * with labelled meridians.
 */

/*
 * Define the label for the top of the map.
 */
    char plbl[33];
    strcpy( plbl, "MERIDIONAL LABELS ON A POLAR MAP");
/*
 * Open GKS.
 */
    c_opngks();
/*
 * Turn off the clipping indicator.
 */
    gset_clip_ind (GIND_NO_CLIP);
/*
 * Move the map a little to provide more room for labels.
 */
    c_mappos (.075,.925,.05,.90);
/*
 * Use an elliptical (circular, in this case) perimeter.
 */
    c_mpseti ("EL",1);
/*
 * Show continents and international boundaries.
 */
    c_mapstc ("OU","PO");
/*
 * Use a stereographic projection, centered at the North Pole.
 */
    c_maproj ("ST",90.,0.,-100.);
/*
 * Specify the angular distances to the edges of the map.
 */
    p1[0] = 80.;
    p2[0] = 80.;
    p3[0] = 80.;
    p4[0] = 80.;
    p1[1] = p2[1] = p3[1] = p4[1] = 0.;
    c_mapset ("AN",p1,p2,p3,p4);
/*
 * Draw the map.
 */
    c_mapdrw();
/*
 * a routine to label the meridians.  This routine is not;
 * a part of EZMAP; the code is given below.
 */
    maplbm();
/*
 * Put the label at the top of the plot.
 */
    c_set (0.,1.,0.,1.,0.,1.,0.,1.,1);
    c_pwrit (.5,.975,plbl,32,2,0,0);
/*
 * Draw a boundary around the edge of the plotter frame.
 */
    bndary();
/*
 * Advance the frame.
 */
	c_frame();
/*
 *  Test c_mpgetl, c_mapgtl, c_mpsetl, c_mapstl
 */
    c_mpsetl( "DO", 0 );
    c_mpgetl( "DO", &lval );
    printf( "c_mpsetl, c_mpgetl: lval should be 0, lval is really %d\n", lval );
    c_mapstl( "DO", 0 );
    c_mapgtl( "DO", &lval );
    printf( "c_mapstl, c_mapgtl: lval should be 0, lval is really %d\n", lval );
/*
 * Close GKS.
 */
    c_clsgks();
/*
 * Done.
 */
}

void bndary()
{
/*
 * Routine to draw the plotter-frame edge.
 */
    c_plotit(    0,    0,0);
    c_plotit(32767,    0,1);
    c_plotit(32767,32767,1);
    c_plotit(    0,32767,1);
    c_plotit(    0,    0,1);
}

maplbm()
{
/*
 * this routine labels the meridians if and only if the current
 * projection is azimuthal and centered at one of the poles, a
 * circular boundary is being used, and the grid increment is an
 * integral divisor of 180.  the routine was not thought general
 * enough to include in ezmap itself, but may nevertheless be of
 * interest to users.
 *
 *
 * necessary local declarations.
 */
    char proj[3], chrs[4], chlb[5];
    float plat, plon, flew, frew, fbew, ftew, ulew, urew, vbew, vtew;
    float grid, rota, woch, hoch, holb, wolb, angd, angr, xend, yend;
    float a, b, c, fmul, delx, dely, x, y;
    int i, ielp, lnlg, ilew, irew, ibew, itew, igrd, icsz, nchs;
/*
 * see if the conditions required for maplbm to work are met.
 */
/*
 * the projection must be azimuthal, ...
 */
    c_mpgetc ("PR",proj,2);
    if (strcmp(proj,"ST") && strcmp(proj,"OR") && strcmp(proj,"LE") && 
        strcmp(proj,"GN") && strcmp(proj,"AE")) return(1);
/*
 * the pole latitude must be +90 degrees or -90 degrees, ...
 */
    c_mpgetr ("PT",&plat);
    if (fabs(plat) < 89.9999) return(1);
/*
 * the perimeter must be elliptical, ...
 */
    c_mpgeti ("EL",&ielp);
    if (ielp == 0) return(1);
/*
 * the values used in the set call must define a circle, ...
 */
    c_getset (&flew,&frew,&fbew,&ftew,&ulew,&urew,&vbew,&vtew,&lnlg);
    ilew=c_kfpx(flew);
    irew=c_kfpx(frew);
    ibew=c_kfpy(fbew);
    itew=c_kfpy(ftew);
    if (ulew+urew > 0.0001 || vbew+vtew > 0.0001) return(1);
    if (ulew+vtew > 0.0001 || vbew+urew > 0.0001) return(1);
/*
 * and the grid spacing must be an integral divisor of 180.
 */
    c_mpgetr ("GR",&grid);
    if( (180 % (int)grid) != 0) return(1);
/*
 * all conditions are satisfied.  label the meridians.
 */
/*
 * collect the necessary information.
 */
    igrd=grid;
    c_mapgtr ("PN",&plon);
    c_mapgtr ("RO",&rota);
    c_mpgeti ("LS",&icsz);
    if (icsz == 0) {
        icsz=8;
    }
    else {
        if (icsz == 1) {
            icsz=12;
        }
        else {
            if (icsz == 2) {
                icsz=16;
            }
            else {
                if (icsz == 3) {
                    icsz=24;
                }
            }
        }
    }
    woch=((float)(  icsz)/(float)(irew-ilew))*(urew-ulew);
    hoch=((float)(2*icsz)/(float)(itew-ibew))*(vtew-vbew);
    holb=hoch/1.5;
/*
 * loop on the label values.
 */
    for( i= -180; i <= 179; i+=igrd ) {
/*
 * express the value of the longitude in a nice form.
 */
        sprintf( chrs, "%3d", abs(i) );
        nchs=0;
        if (abs(i) >= 100) {
            nchs=nchs+1;
            chlb[nchs-1]=chrs[0];
        }
        if (abs(i) >= 10) {
            nchs=nchs+1;
            chlb[nchs-1]=chrs[1];
        }
        nchs=nchs+1;
        chlb[nchs-1]=chrs[2];
        if (i > -180 && i < 0) {
            nchs=nchs+1;
            chlb[nchs-1]='W';
        }
        else {
            if (i > 0 && i < 180) {
                nchs=nchs+1;
                chlb[nchs-1]='E';
            }
        }
        chlb[nchs] = '\0';
/*
 * compute the width of the label.
 */
        wolb=(float)(nchs)*woch;
/*
 * find the angle at which the labelled meridian lies on the plot.
 */
        if (plat > 0.) {
            angd=(float)(i-90)-plon-rota;
        }
        else {
            angd=(float)(90-i)+plon-rota;
        }
/*
 * reduce the angle to the range from -180 to +180 and
 * find its equivalent in radians.
 */
        if( angd+180. < 0 ) x = -180.;
        else                x =  180.;
        if( 180.-angd < 0 ) y = -180.;
        else                y =  180.;
        angd=angd-x+y;
        angr=.017453292519943*angd;
/*
 * figure out where the end of the meridian is.
 */
        xend=urew*cos(angr);
        yend=vtew*sin(angr);
/*
 * extend the meridian a little to make a tick mark.
 */
        c_line (xend,yend,1.015*xend,1.015*yend);
/*
 * compute a center position for the label which puts its nearest
 * edge at a fixed distance from the perimeter.  first, compute
 * the components (delx,dely) of the vector from the center of the
 * label box to the edge nearest the perimeter.
 */
        if      (angd < -179.9999) {
            delx= 0.5*wolb;
            dely= 0.;
        }
        else if (angd <  -90.0001) {
            delx= 0.5*wolb;
            dely= 0.5*holb;
        }
        else if (angd <  -89.9999) {
            delx= 0.0;
            dely= 0.5*holb;
        }
        else if (angd <   -0.0001) {
            delx= -0.5*wolb;
            dely= 0.5*holb;
        }
        else if (angd <   0.0001) {
            delx= -0.5*wolb;
            dely= 0.0;
        }
        else if (angd <  89.9999) {
            delx= -0.5*wolb;
            dely= -0.5*holb;
        }
        else if (angd <  90.0001) {
            delx= 0.0;
            dely= -0.5*holb;
        }
        else if (angd < 179.9999) {
            delx= 0.5*wolb;
            dely= -0.5*holb;
        }
        else {
            delx= 0.5*wolb;
            dely= 0.0;
        }
/*
 * then, solve (for fmul) the following equation:
*
 *   sqrt((fmul*xend+delx)**2+(fmul*yend+dely)**2))=1.02*urew
 */
/*
 * which expresses the condition that the corner of the box
 * nearest the circular perimeter should be at a distance of
 * 1.02*(the radius of the perimeter) away from the center of
 * the plot.
 */
        a=xend*xend+yend*yend;
        b=2.*(xend*delx+yend*dely);
        c=delx*delx+dely*dely-1.0404*urew*urew;

        fmul=(-b+sqrt(b*b-4.*a*c))/(2.*a);
/*
 * draw the label.
 */
        c_pwrit (fmul*xend,fmul*yend,chlb,nchs,icsz,0,0);
/*
 * end of loop.
 */
    }
    return(1);
}
