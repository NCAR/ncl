/*
 *	$Id: c_eezmpa.c,v 1.1 1994-05-13 14:26:27 haley Exp $
 */
#include <stdio.h>
#include <math.h>

/*
 * Include function prototypes
 */
#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define max(x,y)   ((x) > (y) ? (x) : (y) )

int iam[250000];

main()
{
    float xcs[10000],ycs[10000];
    int iai[10],iag[10];
    float p1[2],p2[2],p3[3],p4[2];
    int ioc[16];
    Gasfs if1;
    int i,j,isu,ie;
    Gcolr_rep rgb[16];
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
    rgb[ 0].rgb.red = 1.00; rgb[ 0].rgb.green = 1.00; rgb[ 0].rgb.blue = 1.00;
    rgb[ 1].rgb.red = 0.70; rgb[ 1].rgb.green = 0.70; rgb[ 1].rgb.blue = 0.70;
    rgb[ 2].rgb.red = 0.75; rgb[ 2].rgb.green = 0.50; rgb[ 2].rgb.blue = 1.00;
    rgb[ 3].rgb.red = 0.50; rgb[ 3].rgb.green = 0.00; rgb[ 3].rgb.blue = 1.00;
    rgb[ 4].rgb.red = 0.00; rgb[ 4].rgb.green = 0.00; rgb[ 4].rgb.blue = 1.00;
    rgb[ 5].rgb.red = 0.00; rgb[ 5].rgb.green = 0.50; rgb[ 5].rgb.blue = 1.00;
    rgb[ 6].rgb.red = 0.00; rgb[ 6].rgb.green = 1.00; rgb[ 6].rgb.blue = 1.00;
    rgb[ 7].rgb.red = 0.00; rgb[ 7].rgb.green = 1.00; rgb[ 7].rgb.blue = 0.60;
    rgb[ 8].rgb.red = 0.00; rgb[ 8].rgb.green = 1.00; rgb[ 8].rgb.blue = 0.00;
    rgb[ 9].rgb.red = 0.70; rgb[ 9].rgb.green = 1.00; rgb[ 9].rgb.blue = 0.00;
    rgb[10].rgb.red = 1.00; rgb[10].rgb.green = 1.00; rgb[10].rgb.blue = 0.00;
    rgb[11].rgb.red = 1.00; rgb[11].rgb.green = 0.75; rgb[11].rgb.blue = 0.00;
    rgb[12].rgb.red = 1.00; rgb[12].rgb.green = 0.38; rgb[12].rgb.blue = 0.38;
    rgb[13].rgb.red = 1.00; rgb[13].rgb.green = 0.00; rgb[13].rgb.blue = 0.38;
    rgb[14].rgb.red = 1.00; rgb[14].rgb.green = 0.00; rgb[14].rgb.blue = 0.00;
    rgb[15].rgb.red = 0.00; rgb[15].rgb.green = 0.00; rgb[15].rgb.blue = 0.00;
    
    ioc[0] = 0;
    ioc[1] = 6;
    ioc[2] = 2;
    ioc[3] = 5;
    ioc[4] = 12;
    ioc[5] = 10;
    ioc[6] = 11;
    ioc[7] = 1;
    ioc[8] = 3;
    ioc[9] = 4;
    ioc[10] = 8;
    ioc[11] = 9;
    ioc[12] = 7;
    ioc[13] = 13;
    ioc[14] = 14;
    ioc[15] = 15;
/*
 * open gks.
 */
    c_opngks();
/*
 * re-set certain aspect source flags to "individual".
 */
    ginq_asfs(&ie,&if1);
    if1.fill_int_style = GASF_INDIV;
    if1.fill_style_ind = GASF_INDIV;
    gset_asfs(&if1);
/*
 * force solid fill.
 */
    gset_fill_int_style(GSTYLE_SOLID);
/*
 * define 15 different color indices.  the first 14 are spaced through
 * the color spectrum and the final one is black.
 */
    for( j = 0; j < 16; j++ ) {
        i=ioc[j];
        gset_colr_rep(1,j,&rgb[i]);
    }
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
    gset_line_colr_ind(15);

    c_mapsti("LA",0);
    c_mapsti("MV",1);
    c_maplbl();
    c_maplot();
/*
 * draw lines of latitude and longitude over water.  they will be in
 * black because of the gset_line_colr_ind call above.
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
    int i, itm;
    Gpoint_list area;

    if (iai[9] >= 0 && iai[1] >= 0) {
        itm=max(iai[0],iai[1]);
        if (itm > 0) {
            if (*ncs > 150) printf( "colram - ncs too big - %d\n",*ncs);
/*
 * set area fill color index.
 */
            gset_fill_colr_ind(c_mapaci(itm));
/*
 * Create structure to pass to gfill_area
 */
            area.num_points = *ncs-1;
            area.points = (Gpoint *) malloc(area.num_points*sizeof(Gpoint));
            if( !area.points ) {
                fprintf( stderr, "colram: Not enough memory to create fill area structure\n" );
                gemergency_close_gks();
                exit(1);
            }
            for( i = 0; i < *ncs-1; i++ ) {
                area.points[i].x = xcs[i];
                area.points[i].y = ycs[i];
            }
/*
 * Fill area
 */
            gfill_area(&area);
            free((Gpoint *)area.points);
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
    int i, itm;
    Gpoint_list line;

    if (iai[0] >= 0 && iai[1] >= 0) {
        itm=max(iai[0],iai[1]);
        if (c_mapaci(itm) == 1) {
            if (*ncs > 150) printf( "colrln - ncs too big - %d\n",*ncs);
/*
 * Create structure to pass to gfill_area
 */
            line.num_points = *ncs;
            line.points = (Gpoint *) malloc(line.num_points*sizeof(Gpoint));
            if( !line.points ) {
                fprintf( stderr, "colrln: Not enough memory to create fill area structure\n" );
                gemergency_close_gks();
                exit(1);
            }
            for( i = 0; i < *ncs; i++ ) {
                line.points[i].x = xcs[i];
                line.points[i].y = ycs[i];
            }
            gpolyline(&line);
            free(line.points);
        }
    }
    return(1);
}
