/*
 *  $Id: c_tezmpa.c,v 1.1 1994-05-13 14:26:47 haley Exp $
 */
#include <stdio.h>
#include <math.h>
#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

main()
{
    int idum, ierr;
/*
 * OPEN GKS, OPEN WORKSTATION OF TYPE 1, ACTIVATE WORKSTATION
 */
    gopen_gks ("stdout",0);
    gopen_ws (1, NULL, 1);
    gactivate_ws (1);
/*
 * invoke demo driver
 */
    tezmpa(&ierr);
/*
 *     deactivate and close workstation, close gks.
 */
    gdeactivate_ws(1);
    gclose_ws(1);
    gclose_gks();
}

tezmpa (ierror)
int *ierror;
{
    int iam[25000],iai[2],iag[2],ioc[14],ie;
    int i,j, isu;
    Gasfs if1;
/*
 * dimension the arrays needed by c_arscam and ardrln for x/y coordinates.
 */
    float xcs[200],ycs[200], plm1[2],plm2[2],plm3[2],plm4[2];
/*
 * define the rgb color triples needed below.
 */
    Gcolr_rep rgb[14];
/*
 * declare the routine which will color the areas.
 */
    extern int colram(
#ifdef NeedFuncProto
        float *xcs,
        float *ycs,
        int *ncs,
        int *iai,
        int *iag,
        int *nai
#endif
    );
    extern int colrln(
#ifdef NeedFuncProto
        float *xcs,
        float *ycs,
        int *ncs,
        int *iai,
        int *iag,
        int *nai
#endif
    );
/*
 * declare the routine which will draw lines of latitude and longitude
 * over water.
 */
    ioc[0] = 6;
    ioc[1] = 2;
    ioc[2] = 5;
    ioc[3] = 12;
    ioc[4] = 10;
    ioc[5] = 11;
    ioc[6] = 1;
    ioc[7] = 3;
    ioc[8] = 4;
    ioc[9] = 8;
    ioc[10] = 9;
    ioc[11] = 7;
    ioc[12] = 13;
    ioc[13] = 14;

    rgb[0].rgb.red = 0.70;
    rgb[0].rgb.green = 0.70;
    rgb[0].rgb.blue = 0.70;
    rgb[1].rgb.red = 0.75;
    rgb[1].rgb.green = 0.50;
    rgb[1].rgb.blue = 1.00;
    rgb[2].rgb.red = 0.50;
    rgb[2].rgb.green = 0.00;
    rgb[2].rgb.blue = 1.00;
    rgb[3].rgb.red = 0.00;
    rgb[3].rgb.green = 0.00;
    rgb[3].rgb.blue = 1.00;
    rgb[4].rgb.red = 0.00;
    rgb[4].rgb.green = 0.50;
    rgb[4].rgb.blue = 1.00;
    rgb[5].rgb.red = 0.00;
    rgb[5].rgb.green = 1.00;
    rgb[5].rgb.blue = 1.00;
    rgb[6].rgb.red = 0.00;
    rgb[6].rgb.green = 1.00;
    rgb[6].rgb.blue = 0.60;
    rgb[7].rgb.red = 0.00;
    rgb[7].rgb.green = 1.00;
    rgb[7].rgb.blue = 0.00;
    rgb[8].rgb.red = 0.70;
    rgb[8].rgb.green = 1.00;
    rgb[8].rgb.blue = 0.00;
    rgb[9].rgb.red = 1.00;
    rgb[9].rgb.green = 1.00;
    rgb[9].rgb.blue = 0.00;
    rgb[10].rgb.red = 1.00;
    rgb[10].rgb.green = 0.75;
    rgb[10].rgb.blue = 0.00;
    rgb[11].rgb.red = 1.00;
    rgb[11].rgb.green = 0.38;
    rgb[11].rgb.blue = 0.38;
    rgb[12].rgb.red = 1.00;
    rgb[12].rgb.green = 0.00;
    rgb[12].rgb.blue = 0.38;
    rgb[13].rgb.red = 1.00;
    rgb[13].rgb.green = 0.00;
    rgb[13].rgb.blue = 0.00;
/*
 * set the aspect source flags for fill area interior style and for
 * fill area style index to "individual".
 */
    ginq_asfs (&ie,&if1);
    if1.fill_int_style=1;
    if1.fill_style_ind=1; 
    gset_asfs (&if1);
/*
 * force solid fill.
 */
    gset_fill_int_style (GSTYLE_SOLID);
/*
 * define 15 different color indices.  the first 14 are spaced through
 * the color spectrum and the final one is black.
 */
    for( j = 1; j <= 14; j++ ) {
        i=ioc[j-1];
        gset_colr_rep(1,j,&rgb[i-1]);
    }
/*
 * set color index 15 to black.
 */
    rgb[0].rgb.red = rgb[0].rgb.green = rgb[0].rgb.blue = 0.00;
    gset_colr_rep(1,15,&rgb[0]);
/*
 * set up ezmap.
 */
    c_mapsti ("MV",1);
    c_mapstc ("OU","PO");
    c_maproj ("ME",0.,0.,0.);
    plm1[0] = 30.;
    plm2[0] = -15.;
    plm3[0] = 60.;
    plm4[0] = 30.;
    plm1[1] = plm2[1] = plm3[1] = plm4[1] = 0.0;
    c_mapset ("CO",plm1,plm2,plm3,plm4);
/*
 * make c_mapbla use 1 and 2 as the group identifiers.
 */
    c_mapsti ("G1",1);
    c_mapsti ("G2",2);
/*
 * use 5 vertical strips to reduce the number of points defining the
 * sub-areas.
 */
    c_mapsti ("VS",5);
/*
 * initialize ezmap.
 */
    c_mapint();
/*
 * initialize the area map.
 */
    c_arinam (iam,25000);
/*
 * add edges to the area map.
 */
    c_mapbla (iam);
/*
 * pre-process the area map.
 */
    c_arpram (iam,0,0,0);
/*
 * compute and print the amount of space used in the area map.
 */
    isu=25000-(iam[5]-iam[4]-1);
    printf( "SPACE USED IN AREA MAP IS %d\n", isu);
/*
 * color the map.
 */
    c_arscam (iam,xcs,ycs,200,iai,iag,2,colram);
/*
 * flush c_plotit's buffers and set polyline color index to black.
 */
    c_plotit(0,0,0);
    gset_line_colr_ind(15);
/*
 * in black, draw a perimeter and outline all the countries.
 */
    c_mapsti ("LA",0);
    c_mapsti ("MV",1);
    c_maplbl();
    c_maplot();
/*
 * draw lines of latitude and longitude over water.
 */
    c_mapgrm (iam,xcs,ycs,200,iai,iag,2,colrln);
/*
 * advance the frame.
 */
    c_frame();
/*
 * done.
 */
    *ierror=0;
    printf("  EZMAPA TEST EXECUTED--SEE PLOTS TO CERTIFY\n");
    return(1);
}

#ifdef __STDC__
int colram(
    float *xcs,
    float *ycs,
    int *ncs,
    int *iai,
    int *iag,
    int *nai
)
#else 
int colram(xcs,ycs,ncs,iai,iag,nai)
    float *xcs;
    float *ycs;
    int *ncs;
    int *iai;
    int *iag;
    int *nai;
#endif
{
    int i, itm;
    Gpoint_list fill_area;

    itm=1;
    for( i = 0; i < *nai; i++ ) {
        if (iai[i] < 0) itm=0;
    }
    if (itm != 0) {
        itm=0;
        for( i = 0; i < *nai; i++ ) {
            if (iag[i] == 1) itm=iai[i];
        }
          if (itm > 0) {
/*
 *  set area fill color index. 
 */
              gset_fill_colr_ind(c_mapaci(itm));
/*
 * Create structure to pass to gfill_area
 */
              fill_area.num_points = *ncs-1;
              fill_area.points = (Gpoint *) malloc(2*(*ncs-1)*sizeof(Gfloat));
              if( !fill_area.points ) {
                  fprintf( stderr, "colram: Not enough memory to create fill area structure\n" );
                  gemergency_close_gks();
                  exit(1);
              }
              for( i = 0; i < *ncs-1; i++ ) {
                  fill_area.points[i].x = xcs[i];
                  fill_area.points[i].y = ycs[i];
              }
/*
 * Fill area
 */
              gfill_area (&fill_area);
              free(fill_area.points);
          }
    }
    return(1);
}

#ifdef __STDC__
int colrln(
    float *xcs,
    float *ycs,
    int *ncs,
    int *iai,
    int *iag,
    int *nai
)
#else 
int colrln(xcs,ycs,ncs,iai,iag,nai)
    float *xcs;
    float *ycs;
    int *ncs;
    int *iai;
    int *iag;
    int *nai;
#endif
{
    int i, itm;
    Gpoint_list line;

    itm=1;
    for( i = 0; i < *nai; i++ ) {
        if (iai[i] < 0) itm=0;
    }
    if (itm != 0) {
        itm=0;
        for( i = 0; i < *nai; i++ ) {
            if (iag[i] == 1) itm=iai[i];
        }
        if (c_mapaci(itm) == 1) {
/*
 * flush plotit's buffers and set polyline color index to black.
 */
            c_plotit(0,0,0);
            gset_line_colr_ind(15);
/*
 * Create structure to pass to gpolyline
 */
            line.num_points = *ncs;
            line.points = (Gpoint *) malloc(2*(*ncs)*sizeof(Gfloat));
            if( !line.points ) {
                fprintf( stderr, "colrln: Not enough memory to create fill area structure\n" );
                gemergency_close_gks();
                exit(1);
            }
            for( i = 0; i < *ncs; i++ ) {
                line.points[i].x = xcs[i];
                line.points[i].y = ycs[i];
            }
/*
 * Draw line
 */
            gpolyline (&line);
            free(line.points);
        }
    }
    return(1);
}
