/*
 *  $Id: c_fngngdts.c,v 1.3 1994-08-11 19:01:29 haley Exp $
 */
#include <stdio.h>

/*
 * Include function prototypes
 */
#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define WSTYPE SED_WSTYPE
#define WKID   1

main()
{
    float plim1[2], plim2[2], plim3[2], plim4[2];
    char stmp[20];
    int ival;
    float rval;

    plim1[0] = 22.;
    plim1[1] = 0.;
    plim2[0] = -122.;
    plim2[1] = 0.;
    plim3[0] = 47.;
    plim3[1] = 0.;
    plim4[0] = -65.;
    plim4[1] = 0.;
/*
 * Open GKS, Turn Clipping off
 */
    gopen_gks("stdout",0);
    gopen_ws(WKID, NULL, WSTYPE);
    gactivate_ws(WKID);
/*
 * Invoke demo driver
 */
    cmptra("OR",35.,-105.,0.,"PO","CO",plim1,plim2,plim3,plim4);
/*
 * Test c_nggetc
 */
    c_ngsetc("ME","TEST");
    c_nggetc("ME.",stmp,19);
    if( strcmp( stmp, "TEST" ) ) {
        printf( "c_nggetc:  stmp should be 'TEST', stmp is really '%s'\n", stmp );
    }
/*
 *     DEACTIVATE AND CLOSE WORKSTATION, CLOSE GKS.
 */
    gdeactivate_ws(WKID);
    gclose_ws(WKID);
    gclose_gks();
}

char *city[3] = {"Atlanta, GA", "Boulder, CO", "Seattle, WA"};

cmptra(proj, plat, plon, rota, outln, jlim, plim1, plim2, plim3, plim4)
float plat, plon, rota;
char *proj, *outln, *jlim;
float plim1[2], plim2[2], plim3[2], plim4[2];
{
    int i, indice, numsts = 3;
    float lat[3], lon[3], xcoor[3], ycoor[3], x, y;
        extern void dfclrs();
/*
 * lat, lon coordinates for Atlanta, Boulder, and Seattle
 */
    lat[0] = 33.5;
    lat[1] = 40.0;
    lat[2] = 47.5;
    lon[0] = -84.5;
    lon[1] = -105.;
    lon[2] = -122.0;
    xcoor[0] = xcoor[1] = xcoor[2] = 0.;
    ycoor[0] = ycoor[1] = ycoor[2] = 0.;
/*
 * cmptra demonstrates marking points on a map
 *
 * Set up a color table
 */
    dfclrs();
/* 
 * Draw Continental, political outlines 
 */
    c_mapstc ("OU - OUTLINE DATASET SELECTOR",outln);
/*
 * Set up projection
 */
    c_maproj (proj,plat,plon,rota);
/*
 * If it's a satellite projection, choose a satellite distance
 */
    if (!strcmp(proj,"SV")) c_mapstr ("SA - SATELLITE DISTANCE:",5.);
/*
 * Set limits of map
 */
    c_mapset (jlim,plim1,plim2,plim3,plim4);
/*
 * Turn off Grid lines
 */
    c_mapstr ("GR",0.);
/*
 * Draw map
 */
    c_mapdrw();

    for( i = 0; i < numsts; i++ ) {
/*
 * Transform the Coordinates
 */
        c_maptra(lat[i],lon[i],&x,&y);
        xcoor[i] = x;
        ycoor[i] = y;
/*
 * Draw a label
 */
        c_plchhq (x+.015,y,city[i], .015, 0., -1.);
/*
 * Save last indice
 */
        indice = i;
    }

/*
 * Draw filled circles at selected sites.
 */
    if (xcoor[indice] != 1.e12) {
        c_ngdots (xcoor,ycoor,numsts,.02,15);
    }
/*
 * Draw stars over the selected sites and connect them with lines.
 */
    c_points (xcoor, ycoor, numsts, -3, 1);
/*
 * Advance the frame.
 */
    c_frame();
/*
 * Done.
 */
}

void dfclrs()
{
/*
 * Define a set of RGB color triples for colors 1 through 15.
 */
    Gcolr_rep rgbv[16];
    int i;
/*
 * Define the RGB color triples needed below.
 */
    rgbv[0].rgb.red = 1.00;
    rgbv[0].rgb.green = 1.00;
    rgbv[0].rgb.blue = 1.00;
    rgbv[1].rgb.red = 0.00;
    rgbv[1].rgb.green = 0.00;
    rgbv[1].rgb.blue = 0.00;
    rgbv[2].rgb.red = 0.70;
    rgbv[2].rgb.green = 0.70;
    rgbv[2].rgb.blue = 0.70;
    rgbv[3].rgb.red = 0.75;
    rgbv[3].rgb.green = 0.50;
    rgbv[3].rgb.blue = 1.00;
    rgbv[4].rgb.red = 0.50;
    rgbv[4].rgb.green = 0.00;
    rgbv[4].rgb.blue = 1.00;
    rgbv[5].rgb.red = 0.00;
    rgbv[5].rgb.green = 0.00;
    rgbv[5].rgb.blue = 1.00;
    rgbv[6].rgb.red = 0.00;
    rgbv[6].rgb.green = 0.50;
    rgbv[6].rgb.blue = 1.00;
    rgbv[7].rgb.red = 0.00;
    rgbv[7].rgb.green = 1.00;
    rgbv[7].rgb.blue = 1.00;
    rgbv[8].rgb.red = 0.00;
    rgbv[8].rgb.green = 1.00;
    rgbv[8].rgb.blue = 0.60;
    rgbv[9].rgb.red = 0.00;
    rgbv[9].rgb.green = 1.00;
    rgbv[9].rgb.blue = 0.00;
    rgbv[10].rgb.red = 0.70;
    rgbv[10].rgb.green = 1.00;
    rgbv[10].rgb.blue = 0.00;
    rgbv[11].rgb.red = 1.00;
    rgbv[11].rgb.green = 1.00;
    rgbv[11].rgb.blue = 0.00;
    rgbv[12].rgb.red = 1.00;
    rgbv[12].rgb.green = 0.75;
    rgbv[12].rgb.blue = 0.00;
    rgbv[13].rgb.red = 1.00;
    rgbv[13].rgb.green = 0.38;
    rgbv[13].rgb.blue = 0.38;
    rgbv[14].rgb.red = 1.00;
    rgbv[14].rgb.green = 0.00;
    rgbv[14].rgb.blue = 0.38;
    rgbv[15].rgb.red = 1.00;
    rgbv[15].rgb.green = 0.00;
    rgbv[15].rgb.blue = 0.00;
/*
 * Define 16 different color indices, for indices 0 through 15.  The
 * color corresponding to index 0 is black and the color corresponding
 * to index 1 is white.
 */
    for( i = 0; i <= 15; i++ ) {
        gset_colr_rep(WKID,i,&rgbv[i]);
    }
}
