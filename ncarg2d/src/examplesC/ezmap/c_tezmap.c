/*
 *	$Id: c_tezmap.c,v 1.1 1994-10-31 03:29:15 haley Exp $
 */
#include <stdio.h>
#include <math.h>
#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

struct common {
    int iframe, ierrr;
} NGCALLF(error,ERROR);

#define WSTYPE  SED_WSTYPE
#define WKID    1

main()
{
    int idum, ierr;
/*
 * OPEN GKS, OPEN WORKSTATION OF TYPE 1, ACTIVATE WORKSTATION
 */
    gopen_gks ("stdout",0);
    gopen_ws (WKID, NULL, WSTYPE);
    gactivate_ws (WKID);
/*
 * INVOKE DEMO DRIVER
 */
    tezmap(&ierr);
/*
 *     DEACTIVATE AND CLOSE WORKSTATION, CLOSE GKS.
 */
    gdeactivate_ws(WKID);
    gclose_ws(WKID);
    gclose_gks();
}

tezmap (ierror)
int *ierror;
{
    float plm1[2],plm2[2],plm3[2],plm4[2];
    float tx,ty;
    int ierr, idum;

    plm1[0] = plm1[1] = plm2[0] = plm2[1] = 0.0;
    plm3[0] = plm3[1] = plm4[0] = plm4[1] = 0.0;
/*
 * Define the center of a plot title string on a square grid of size
 * 0. to 1.
 */
    tx = 0.5;
    ty = 0.9765;
/*
 * Initialize the error parameter.
 */
    NGCALLF(error,ERROR).ierrr = 0;
/*
 * Turn on the error recovery mode.
 */
    c_entsr(&idum,1);
/*
 *     Frame 1 -- The stereographic projection.
 */
    NGCALLF(error,ERROR).iframe = 1;
/*
 * Set the projection-type parameter.
 */
    c_maproj("ST",80.0,-160.0,0.0);
/*
 * Set the limit parameters.
 */
    c_mapset("MA",plm1,plm2,plm3,plm4);
/*
 * Set the outline-dataset parameter.
 */
    c_mapstc("OU","PS");
/*
 * Draw the map.
 */
    c_mapdrw();
/*
 * Report any errors encountered.
 */
    if( c_nerro(&ierr) ) rpterr();
/*
 * Select normalization transformation 0.
 */
    gsel_norm_tran(0);
/*
 * Call PLCHLQ to write the plot title.
 */
    c_plchlq(tx,ty,"EZMAP DEMONSTRATION:  STEREOGRAPHIC PROJECTION",16.,0.,0.);
    c_frame();

/*
 *     Frame 2 -- The orthographic projection.
 */
    NGCALLF(error,ERROR).iframe = 2;
/*
 * Set the projection-type parameter.
 */
    c_maproj("OR",60.0,-120.0,0.0);
/*
 * Draw the map.
 */
    c_mapdrw();
/*
 * Report any errors encountered.
 */
    if( c_nerro(&ierr) ) rpterr();
/*
 * Write the title.
 */
/*
 * Select normalization transformation 0.
 */
    gsel_norm_tran(0);
/*
 * Call PLCHLQ to write the plot title.
 */
    c_plchlq(tx,ty,"EZMAP DEMONSTRATION:  ORTHOGRAPHIC PROJECTION",16.,0.,0.);
    c_frame();
/*
 *     Frame 3 -- The Lambert conformal conic projection.
 */
    NGCALLF(error,ERROR).iframe = 3;
/*
 * Set the projection-type, limits, and outline-dataset parameters.
 */
    c_maproj("LC",45.0,-100.0,45.0);
    plm1[0] = 50.0;
    plm2[0] = -130.0;
    plm3[0] = 20.0;
    plm4[0] = -75.0;
    c_mapset("CO",plm1,plm2,plm3,plm4);
    plm1[0] = 0.0;
    plm2[0] = 0.0;
    plm3[0] = 0.0;
    plm4[0] = 0.0;
    c_mapstc("OU","US");
/*
 * Draw the map.
 */
    c_mapdrw();
/*
 * Report any errors encountered.
 */
    if( c_nerro(&ierr)) rpterr();
/*
 * Select normalization transformation 0.
 */
    gsel_norm_tran(0);
/*
 * Call PLCHLQ to write the plot title.
 */
    c_plchlq(tx,ty,"EZMAP DEMONSTRATION: LAMBERT CONFORMAL CONIC PROJECTION",16.,0.,0.);
    c_frame();
/*
 *     Frame 4 -- The Lambert equal area projection.
 */
    NGCALLF(error,ERROR).iframe = 4;
/*
 * Set the projection-type, limits, and outline-dataset parameters.
 */
    c_maproj("LE",20.0,-40.0,0.0);
    c_mapset("MA",plm1,plm2,plm3,plm4);
    c_mapstc("OU","CO");
/*
 * Draw the map.
 */
    c_mapdrw();
/*
 * Report any errors encountered.
 */
    if( c_nerro(&ierr))  rpterr();
/*
 * Select normalization transformation 0.
 */
    gsel_norm_tran(0);
/*
 * Call PLCHLQ to write the plot title.
 */
    c_plchlq(tx,ty,"EZMAP DEMONSTRATION:  LAMBERT EQUAL AREA PROJECTION",16.,0.,0.);
    c_frame();
/*
 *     Frame 5 -- The gnomonic projection.
 */
    NGCALLF(error,ERROR).iframe = 5;
/*
 * Set the projection-type parameter.
 */
    c_maproj("GN",0.0,0.0,0.0);
/*
 * Draw the map.
 */
    c_mapdrw();
/*
 * Report any errors encountered.
 */
    if( c_nerro(&ierr)) rpterr();
/*
 * Select normalization transformation 0.
 */
    gsel_norm_tran(0);
/*
 * Call PLCHLQ to write the plot title.
 */
    c_plchlq(tx,ty,"EZMAP DEMONSTRATION:  GNOMONIC PROJECTION",16.,0.,0.);
    c_frame();
/*
 *     Frame 6 -- The azimuthal equidistant projection.
 */
    NGCALLF(error,ERROR).iframe = 6;
/*
 * Set the projection-type parameter.
 */
    c_maproj("AE",-20.0,40.0,0.0);
/*
 * Set the grid spacing.
 */
    c_mapstr("GR",5.0);
/*
 * Draw the map.
 */
    c_mapdrw();
/*
 * Report any errors encountered.
 */
    if( c_nerro(&ierr)) rpterr();
/*
 * Select normalization transformation 0.
 */
    gsel_norm_tran(0);
/*
 * Call PLCHLQ to write the plot title.
 */
    c_plchlq(tx,ty,"EZMAP DEMONSTRATION:  AZIMUTHAL EQUIDISTANT PROJECTION",16.,0.,0.);
    c_frame();
/*
 *     Frame 7 -- The cylindrical equidistant projection.
 */
    NGCALLF(error,ERROR).iframe = 7;
/*
 * Set the map projection type parameter.
 */
    c_maproj("CE",-40.0,80.0,0.0);
/*
 * Draw the map.
 */
    c_mapdrw();
/*
 * Report any errors encountered.
 */
    if( c_nerro(&ierr))  rpterr();
/*
 * Select normalization transformation 0.
 */
    gsel_norm_tran(0);
/*
 * Call PLCHLQ to write the plot title.
 */
    c_plchlq(tx,ty,"EZMAP DEMONSTRATION:  CYLINDRICAL EQUIDISTANT PROJECTION",16.,0.,0.);
    c_frame();
/*
 *     Frame 8 -- The mercator projection.
 */
    NGCALLF(error,ERROR).iframe = 8;
/*
 * Set the map projection type parameter.
 */
    c_maproj("ME",-60.0,120.0,0.0);
/*
 * Draw the map.
 */
    c_mapdrw();
/*
 * Report any errors encountered.
 */
    if( c_nerro(&ierr)) rpterr();
/*
 * Select normalization transformation 0.
 */
    gsel_norm_tran(0);
/*
 * Call PLCHLQ to write the plot title.
 */
    c_plchlq(tx,ty,"EZMAP DEMONSTRATION: MERCATOR PROJECTION", 16.,0.,0.);
    c_frame();
/*
 *     Frame 9 -- The Mollyweide-type projection.
 */
    NGCALLF(error,ERROR).iframe = 9;
/*
 * Set the map projection type parameter.
 */
    c_maproj("MO",-80.0,160.0,0.0);
/*
 * Draw the map.
 */
    c_mapdrw();
/*
 * Report any errors encountered.
 */
    if( c_nerro(&ierr)) rpterr();
/*
 * Select normalization transformation 0.
 */
    gsel_norm_tran(0);
/*
 * Call PLCHLQ to write the plot title.
 */
    c_plchlq(tx,ty,"EZMAP DEMONSTRATION:  MOLLWEIDE-TYPE PROJECTION",16.,0.,0.);
    c_frame();
/*
 *     Frame 10 -- The satellite view projection.
 */
    NGCALLF(error,ERROR).iframe = 10;
/*
 * Set the map projection type parameter.
 */
    c_maproj("SV",0.0,-135.0,0.0);
/*
 * Set the satellite distance and supress grid lines.
 */
    c_mapstr("SA",6.631);
    c_mapsti("GR",0);
/*
 * Draw the map.
 */
    c_mapdrw();
/*
 * Report any errors encountered.
 */
    if( c_nerro(&ierr)) rpterr();
/*
 * Select normalization transformation 0.
 */
    gsel_norm_tran(0);
/*
 * Call PLCHLQ to write the plot title.
 */
    c_plchlq(tx,ty,"EZMAP DEMONSTATION: SATELLITE VIEW PROJECTION",16.,0.,0.);
    c_frame();

    if(!NGCALLF(error,ERROR).ierrr) printf( "EZMAP TEST EXECUTED--SEE PLOTS TO CERTIFY\n");
    if(NGCALLF(error,ERROR).ierrr==1) printf( "EZMAP TEST UNSUCCESSFUL\n" );
    *ierror = ierr;
    return(1);
}

rpterr()
{
/*
 * ROUTINE TO REPORT ERROR MESSEGES
 */
    printf( "ERROR IN FRAME %2d\nERROR MESSAGE FOLLOWS:", NGCALLF(error,ERROR).iframe );
    c_eprin();
    c_errof();
    NGCALLF(error,ERROR).ierrr = 1;
    return(1);
}
