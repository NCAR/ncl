/*
 *	$Id: c_eezmpa.c,v 1.2 1992-11-04 15:50:24 haley Exp $
 */
#include <stdio.h>
#include <math.h>
#include <ncarg/ncargC.h>
#include <ncarg/ncarg_gksC.h>

#define max(x,y)   ((x) > (y) ? (x) : (y) )

int iam[250000];

main()
{
    float xcs[10000],ycs[10000];
    int iai[10],iag[10];
    float rgb[4][15],p1[2],p2[2],p3[3],p4[2];
    int ioc[14];
    int if1[13];
    int i,j,isu,ie;
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
        int *igi,
        int *nai
#endif
    );
/*
 * define the required rgb triples and indices.
 */
    rgb[1][1] = 0.70;
    rgb[2][1] = 0.70;
    rgb[3][1] = 0.70;
    rgb[1][2] = 0.75;
    rgb[2][2] = 0.50;
    rgb[3][2] = 1.00;
    rgb[1][3] = 0.50;
    rgb[2][3] = 0.00;
    rgb[3][3] = 1.00;
    rgb[1][4] = 0.00;
    rgb[2][4] = 0.00;
    rgb[3][4] = 1.00;
    rgb[1][5] = 0.00;
    rgb[2][5] = 0.50;
    rgb[3][5] = 1.00;
    rgb[1][6] = 0.00;
    rgb[2][6] = 1.00;
    rgb[3][6] = 1.00;
    rgb[1][7] = 0.00;
    rgb[2][7] = 1.00;
    rgb[3][7] = 0.60;
    rgb[1][8] = 0.00;
    rgb[2][8] = 1.00;
    rgb[3][8] = 0.00;
    rgb[1][9] = 0.70;
    rgb[2][9] = 1.00;
    rgb[3][9] = 0.00;
    rgb[1][10] = 1.00;
    rgb[2][10] = 1.00;
    rgb[3][10] = 0.00;
    rgb[1][11] = 1.00;
    rgb[2][11] = 0.75;
    rgb[3][11] = 0.00;
    rgb[1][12] = 1.00;
    rgb[2][12] = 0.38;
    rgb[3][12] = 0.38;
    rgb[1][13] = 1.00;
    rgb[2][13] = 0.00;
    rgb[3][13] = 0.38;
    rgb[1][14] = 1.00;
    rgb[2][14] = 0.00;
    rgb[3][14] = 0.00;
    
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
/*
 * open gks.
 */
    c_opngks();
/*
 * re-set certain aspect source flags to "individual".
 */
    c_gqasf(ie,if1);
    if1[10]=1;
    if1[11]=1;
    c_gsasf(if1);
/*
 * force solid fill.
 */
    c_gsfais(1);
/*
 * define 15 different color indices.  the first 14 are spaced through
 * the color spectrum and the final one is black.
 */
    for( j = 1; j <= 14; j++ ) {
        i=ioc[j-1];
        c_gscr(1,j,rgb[1][i],rgb[2][i],rgb[3][i]);
    }
    c_gscr(1,15,0.,0.,0.);
/*
 * set up ezmap, but don't draw anything.
 */
    p1[0] = p2[0] = p3[0] = p4[0] = 0.;
    p1[1] = p2[1] = p3[1] = p4[1] = 0.;
    c_mapstc("OU","PO");
    c_maproj("ME",0.,0.,0.);
    c_mapset("MA",p1,p2,p3,p4);
/*
 * set the number of vertical strips and the group identifiers to
 * be used by c_mapbla.
 */
    c_mapsti("VS",150);
    c_mapsti("G1",1);
    c_mapsti("G2",2);
/*
 * initialize ezmap.
 */
    c_mapint();
/*
 * initialize the area map.
 */
    c_arinam(iam,250000);
/*
 * add edges to the area map.
 */
    c_mapbla(iam);
/*
 * pre-process the area map.
 */
    c_arpram(iam,0,0,0);
/*
 * compute and print the amount of space used in the area map.
 */
    isu=250000-(iam[6]-iam[5]-1);
    printf( "SPACE USED IN AREA MAP IS %d\n",isu);
/*
 * set the background color.
 */
    c_gscr(1,0,1.,1.,1.);
/*
 * color the map.
 */
    c_arscam(iam,xcs,ycs,10000,iai,iag,10,colram);
/*
 * in black, draw a perimeter and outline all the countries.  we turn
 * off the labels (since they seem to detract from the appearance of
 * the plot) and we reduce the minimum vector length so as to include
 * all of the points in the boundaries.
 *
 * flush c_plotit's buffers and set polyline color index to black.
 */
    c_plotit(0,0,0);
    c_gsplci(15);

    c_mapsti("LA",0);
    c_mapsti("MV",1);
    c_maplbl();
    c_maplot();
/*
 * draw lines of latitude and longitude over water.  they will be in
 * black because of the c_gsplci call above.
 */
    c_mapgrm(iam,xcs,ycs,10000,iai,iag,10,colrln);
/*
 * advance the frame.
 */
    c_frame();
/*
 * close gks.
 */
    c_clsgks();
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
colram (xcs,ycs,ncs,iai,iag,nai)
    float *xcs;
    float *ycs;
    int *ncs;
    int *iai;
    int *iag;
    int *nai;
#endif
{
/*
 * for each area, one gets a set of points (using normalized device
 * coordinates), two group identifiers and their associated area
 * identifiers.  if both of the area identifiers are zero or negative,
 * the area need not be color-filled; otherwise, it is filled with
 * a color obtained from mapaci.  if the area is defined by more than
 * 150 points, we'd like to know about it.  (i'm assuming that the
 * device being used won't handle polygons defined by more points than
 * that.)
 */
    int itm;

    if (iai[9] >= 0 && iai[1] >= 0) {
        itm=max(iai[0],iai[1]);
        if (itm > 0) {
            if (*ncs > 150) printf( "colram - ncs too big - %d\n",*ncs);
/*
 * set area fill color index.
 */
            c_gsfaci(c_mapaci(itm));

            c_gfa (*ncs-1,xcs,ycs);
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
/*
 * for each line segment, one gets a set of points (using normalized
 * device coordinates), two group identifiers and their associated
 * area identifiers.  if both of the area identifiers are zero or
 * negative, the segment is not drawn; otherwise, we use mapaci to
 * see if the segment is over water and, if so, we draw the segment.
 * if the segment is defined by more than 150 points, we'd like to
 * know about it.
 */
    int itm;

    if (iai[0] >= 0 && iai[1] >= 0) {
        itm=max(iai[0],iai[1]);
        if (c_mapaci(itm) == 1) {
            if (*ncs > 150) printf( "colrln - ncs too big - %d\n",*ncs);
            c_gpl (*ncs,xcs,ycs);
        }
    }
    return(1);
}
