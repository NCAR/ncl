/*
 * $Id: c_cmpitm.c,v 1.1 1994-05-13 14:26:24 haley Exp $
 */

#include <stdio.h>
#include <math.h>

#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

/*
 *
 * This program draws a map, then it draws the projection of a circle, 
 * which is masked over land.
 *
 * The arrays clat and clon will be used to hold lat/lon pairs defining
 * the desired circle.
 */

#define NPTS   361
#define NWRK   5000
#define LMAP   150000
#define NGRPS  2
#define ISIZ   2

main()
{
    extern int mask1();
    extern void circle();
    float clat[NPTS], clon[NPTS], xwrk[NWRK], ywrk[NWRK];
    float plim1[2], plim2[2], plim3[2], plim4[2];
    int map[LMAP], iarea[NGRPS], igrp[NGRPS], icir;

    plim1[0] = 20.;
    plim2[0] = -85.;
    plim3[0] = 30.;
    plim4[0] = -75.;
/*
 * Open GKS.
 */
    c_opngks();
/*
 * Draw the map, and set up area map.
 */
    cmpmsk("ME",0.,-75.,0.,"PO","CO",plim1,plim2,plim3,plim4,2.,map,LMAP);
/*
 * Define a circle centered at rlat,rlon
 */
    circle (25.,-80.,3.5,clat,clon,NPTS);
/*
 * Draw the circle on the map.
 */
    c_mapitm  (clat[0],clon[0],0,map,xwrk,ywrk,NWRK,iarea,igrp,NGRPS,mask1);
    for( icir = 1; icir < NPTS; icir++ ) {
        c_mapitm (clat[icir],clon[icir],1,map,xwrk,ywrk,NWRK,iarea,igrp,NGRPS,mask1);
    }
    c_mapiqm (map,xwrk,ywrk,NWRK,iarea,igrp,NGRPS,mask1);
/*
 * Advance the frame.
 */
    c_frame();
/*
 * Close GKS.
 */
    c_clsgks();
}

void circle (rlat,rlon,radius,clat,clon,npts)
float rlat, rlon, radius, *clat, *clon;
int npts;
{
/*
 * Points on a circle around the North Pole are rotated to lie around
 * the point at latitude RLAT and longitude RLON.
 * RADIUS is the radius of the circle, in degrees.
 * Return the lat/lon coordinates of the desired circle in CLAT & CLON.
 *
 * DTOR and RTOD are the constants used to get from degrees to radians
 * and vice-versa.
 */
    float dtor  = .017453292519943;
    float rtod  =  57.2957795130823;
    float alat, alon, ucrd, vcrd, wcrd;
    int icir;

    for( icir = 0; icir < npts; icir++ ) {
        alat = 90. - radius;
        alon = (float)icir;
        ucrd = cos(dtor*alat)*cos(dtor*alon);
        vcrd = cos(dtor*alat)*sin(dtor*alon);
        wcrd = sin(dtor*alat);
        if (rlat >= 0) {
            rotate (2,90.-rlat,&ucrd,&vcrd,&wcrd);
            rotate (3,rlon,&ucrd,&vcrd,&wcrd);
        }
        else {
            rotate (2,90.+fabs(rlat),&ucrd,&vcrd,&wcrd);
            rotate (3,rlon,&ucrd,&vcrd,&wcrd);
        }
        clat[icir] = rtod*(float)asin((double)wcrd);
        clon[icir] = rtod*(float)atan2((double)vcrd,(double)ucrd);
    }
    return;
}

rotate (iaxs,angl,ucrd,vcrd,wcrd)
int iaxs;
float angl,*ucrd,*vcrd,*wcrd;
{
    float sina, cosa;
    float utmp, vtmp, wtmp;
/*
 * This routine rotates the point with coordinates (UCRD,VCRD,WCRD)
 * by the angle ANGL about the axis specified by IAXS (1 for the
 * U axis, 2 for the V axis, 3 for the W axis).  The coordinate
 * system is assumed to be a right-handed one.
 */
    sina = sin(.017453292519943*angl);
    cosa = cos(.017453292519943*angl);

    utmp = *ucrd;
    vtmp = *vcrd;
    wtmp = *wcrd;

    if ( iaxs == 1) {
        *vcrd = vtmp*cosa-wtmp*sina;
        *wcrd = wtmp*cosa+vtmp*sina;
    }
    else if ( iaxs == 2) {
        *ucrd = utmp*cosa+wtmp*sina;
        *wcrd = wtmp*cosa-utmp*sina;
    }
    else {
        *ucrd = utmp*cosa-vtmp*sina;
        *vcrd = vtmp*cosa+utmp*sina;
    }
    return;
}

cmpmsk(proj, plat, plon, rota, outln, jlim, plim1, plim2, plim3, plim4, 
       grd, map, lmap)
char proj[3], outln[3], jlim[3];
float plat, plon, rota, grd;
int *map, lmap;
float plim1[2], plim2[2], plim3[2], plim4[2];
{
    extern int mask2();
    int iarea[ISIZ], igrp[ISIZ], idash = 65535;
    float xwrk[NWRK], ywrk[NWRK];
/*
 * CMPLOT demonstrates MAPLOT drawing continental and political outlines
 *
 * Use solid lines for grid
 */
    c_dashdb(&idash);
/*
 * Draw Continental, political outlines 
 */
    c_mapstc ("OU - OUTLINE DATASET SELECTOR",outln);
/*
 * Set grid spacing
 */
    c_mapstr ("GR - GRID SPACING",grd);
/*
 * Set up projection
 */
    c_maproj (proj,plat,plon,rota);
/*
 * If it"s a satellite projection, choose a satellite distance
 */
    if( !strcmp( proj, "SV") )c_mapstr ("SA - SATELLITE DISTANCE",5.);
/*
 * Set limits of map
 */
    c_mapset (jlim,plim1,plim2,plim3,plim4);
/*
 * Initialize Maps and Areas
 */
    c_mapint();
    c_arinam (map,LMAP);
    c_mapbla (map);
/*
 * Draw Masked Grid Lines
 */
    c_mapgrm (map, xwrk, ywrk, NWRK, iarea, igrp, ISIZ, mask2);
/*
 * draw Continental Outlines and Perimeter
 */
    c_maplbl();
    c_maplot();
/*
 * Done.
 */
    return;
}


int mask1(xc,yc,mcs,areaid,grpid,idsize)
float *xc, *yc;
int *areaid, *grpid, *mcs, *idsize;
{
    int i, id;
/*
 * Retrieve area id for geographical area
 */
    for( i = 0; i < *idsize; i++ ) {
        if (grpid[i] == 1) id = areaid[i];
    }
/*
 * If the line is over water, and has 2 or more points draw it.
 */
    if ((c_mapaci(id) == 1) && (*mcs >= 2)) {
        c_curved(xc,yc,*mcs);
    }
/*
 * Otherwise, don't draw the line - mask it.
 */
    return(1);
}

int mask2(xc,yc,mcs,areaid,grpid,idsize)
float *xc, *yc;
int *areaid, *grpid, *mcs, *idsize;
{
    int i, id, idash = 29298;
    c_dashdb (&idash);
/*
 * Retrieve area id for geographical area
 */
    for( i = 0; i < *idsize; i++ ) {
        if (grpid[i] == 1) id = areaid[i];
    }
/*
 * If the line is over water, and has 2 or more points draw it.
 */
    if ((c_mapaci(id) == 1) && (*mcs >= 2)) {
        c_curved(xc,yc,*mcs);
    }
/*
 * Otherwise, don't draw the line - mask it.
 */
    return(1);
}

