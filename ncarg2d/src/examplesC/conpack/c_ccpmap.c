/*
 * $Id: c_ccpmap.c,v 1.3 1994-08-19 14:39:39 haley Exp $
 */

#include <stdio.h>
#include <math.h>

#include <ncarg/gks.h>
#include <ncarg/ncargC.h>

#define min(x,y)   ((x) < (y) ? (x) : (y))
#define max(x,y)   ((x) > (y) ? (x) : (y))

/*
** Define error file, Fortran unit number, and workstation type,
** and workstation ID.
*/
#define IERRF  "stdout"
#define LUNIT  "gmeta"
#define IWTYPE 1
#define IWKID  1
#define ISZDM  0

#define M 30        /* rows of data */
#define N 20        /* columns of data */
#define LRWK 3500   /* size of real work array */
#define LIWK 3500   /* size of integer work array */

#define RMNLON (-190.0) /* minimum longitude value */
#define RMXLON 20.0 /* maximum longitude value */
#define RMNLAT (-20.0)  /* minimum latitude value */
#define RMXLAT 50.0 /* maximum latitude value */

float zdat[N][M];       /* contour data */

int main()
{
    int IWRK[LIWK];     /* integer work array */

                /* create contour data */
    extern void mkdat(int); 

    float rwrk[LRWK];   /* real work array */
/*
**  United States corner latitude and longitudes
*/
    float rlat1[2];     /* lower latitude */
    float rlat2[2];     /* upper latitude */
    float rlon1[2];     /* lower longitude */
    float rlon2[2];     /* upper longitude */


    rlat1[0]=RMNLAT; rlat1[1]=0.0;
    rlat2[0]=RMXLAT; rlat2[1]=0.0;
    rlon1[0]=RMNLON; rlon1[1]=0.0;
    rlon2[0]=RMXLON; rlon2[1]=0.0;

    mkdat (1);
/*
** Open GKS
*/
    gopen_gks(IERRF, ISZDM);
    gopen_ws(IWKID, LUNIT, IWTYPE);
    gactivate_ws(IWKID);
/*
** Turn off clipping.
*/
    gset_clip_ind (0);
/*
** Draw Lat/Lon lines at 10 degree intervals.
** Draw political & continental outlines.
*/
    c_mapsti ("GR - GRID",10);
    c_mapstc ("OU - OUTLINE DATASET","PO");
/*
** Draw a Satellite view over the United States
*/
    c_maproj ("SV - SATELLITE-VIEW",35.,-100.,0.);
    c_mapset ("MA",rlat1,rlon1,rlat2,rlon2);
/*
** Don't draw a square around the globe
*/
    c_mapsti ("PE - PERIMETER FLAG", 0);
/*
** Draw map.
*/
    c_mapdrw();
/*
** Tell CONPACK that the SET call has been done, force it to generate X
** coordinates that are longitudes and Y coordinates that are latitudes,
** turn on mapping to an EZMAP background. Define the out-of-range value
** (returned by MAPTRN for an unprojectable point).
*/
    c_cpseti ("SET - DO SET-CALL FLAG",0);
    c_cpsetr ("XC1 - X COORDINATE AT I = 1",RMNLON);
    c_cpsetr ("XCM - X COORDINATE AT I = M",RMXLON);
    c_cpsetr ("YC1 - Y COORDINATE AT J = 1",RMNLAT);
    c_cpsetr ("YCN - Y COORDINATE AT J = N",RMXLAT);
    c_cpseti ("MAP - MAPPING FLAG",1);
    c_cpsetr ("ORV - OUT-OF-RANGE VALUE",1.0e12);
/*
** Initialize the drawing of the first contour plot.
*/
    c_cprect (&zdat[0][0],M,M,N,rwrk,LRWK,IWRK,LIWK);
/*
** Draw Contours
*/
    c_cpcldr(&zdat[0][0],rwrk,IWRK);

/* Close frame and close GKS */
    c_frame();
    gdeactivate_ws(IWKID);
    gclose_ws(IWKID);
    gclose_gks();
    return (0);
}


void mkdat(nctfr)
int nctfr;
{
    int i, j;
    float x, y, z, c, zmin, zmax, rlon, rlat;
    extern float c_ggdpnt();
/*
 * nctfr is used to generate a random number, ZDAT is the data array
 */
    x = 0.;
    y = 1.;
    z = .9;
    c_ggdini(x,y,nctfr,z);
    zmin = 1.e36;
    zmax = -1.e36;
    for( i = 1; i <= M; i++ ) {
        rlon=.017453292519943*(-180.+360.*(float)(i-1)/(float)(M-1));
        for( j = 1; j <= N; j++ ) {
            rlat=.017453292519943*(-90.+180.*(float)(j-1)/(float)(N-1));
            x = (float)c_ggdpnt(rlat,rlon);
            c = cos(rlat*4.);
            zdat[j-1][i-1] = x+.5*c;
            zmin = min(zmin,zdat[j-1][i-1]);
            zmax = max(zmax,zdat[j-1][i-1]);
        }
    }
    for( i = 1; i <= M; i++ ) {
        for( j = 1; j <= N; j++ ) {
            zdat[j-1][i-1] =((zdat[j-1][i-1]-zmin)/(zmax-zmin))*130.-50.;
        }
    }
    return;
}

