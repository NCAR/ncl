/*
 *  $Id: c_eezmpa.c,v 1.7 2004-08-12 15:14:38 haley Exp $
 */

#include <stdio.h>
#include <math.h>
#include <stdlib.h>

/*
 * Include function prototypes.
 */

#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define max(x,y)   ((x) > (y) ? (x) : (y) )

int iam[25000];

#define IWTYPE 1
#define WKID   1

main()
{
    float xcs[10000],ycs[10000];
    int iai[10],iag[10];
    float p1[2],p2[2],p3[3],p4[2];
    int ioc[16];
    int i,j,isu;
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
 * Define the required RGB triples and indices.
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
 * Open GKS.
 */
    gopen_gks("stdout",0);
    gopen_ws(WKID, NULL, IWTYPE);
    gactivate_ws(WKID);
/*
 * Force solid fill.
 */
    gset_fill_int_style(GSTYLE_SOLID);
/*
 * Define 15 different color indices.  The first 14 are spaced through
 * the color spectrum and the final one is black.
 */
    for( j = 0; j < 16; j++ ) {
        i=ioc[j];
        gset_colr_rep(WKID,j,&rgb[i]);
    }
/*
 * Set up ezmap, but don't draw anything.
 */
    p1[0]= 30.;
    p2[0]=-15.;
    p3[0]= 60.;
    p4[0]= 30.;
    p1[1]=p2[1]=p3[1]=p4[1]=0.;

    c_mapstc("OU","PO");
    c_maproj("ME",0.,0.,0.);
    c_mapset("CO",p1,p2,p3,p4);
/*
 * Make MPLNAM use 1 and 2 as the group identifiers.
 */
    c_mapsti("G1",1);
    c_mapsti("G2",2);
/*
 * Make EZMAP use all boundary points.
 */
    c_mapsti("MV",1);
/*
 * Use 5 vertical strips to reduce the number of points defining the
 * sub-areas.
 */
    c_mapsti("VS",5);
/*
 * Initialize EZMAP.
 */
    c_mapint();
/*
 * Initialize the area map.
 */
    c_arinam(iam,25000);
/*
 * Add edges to the area map.
 */
    c_mapbla(iam);
/*
 * Pre-process the area map.
 */
    c_arpram(iam,0,0,0);
/*
 * Compute and print the amount of space used in the area map.
 */
    isu=iam[0]-(iam[5]-iam[4]-1);
    printf( "SPACE USED IN AREA MAP IS %d\n",isu);
/*
 * Color the map.
 */
    c_arscam(iam,xcs,ycs,10000,iai,iag,10,colram);
/*
 * In black, draw a perimeter and outline all the countries.  We turn
 * off the labels (since they seem to detract from the appearance of
 * the plot) and we reduce the minimum vector length so as to include
 * all of the points in the boundaries.
 *
 * Flush c_plotit's buffers and set polyline color index to black.
 */
    c_plotit(0,0,0);
    gset_line_colr_ind(15);

    c_mapsti("LA",0);
    c_maplbl();

    c_maplot();
/*
 * Draw lines of latitude and longitude over water.  They will be in
 * black because of the gset_line_colr_ind call above.
 */
    c_mapgrm(iam,xcs,ycs,10000,iai,iag,10,colrln);
/*
 * Advance the frame.
 */
    c_frame();
/*
 * Close GKS.
 */
    gdeactivate_ws(WKID);
    gclose_ws(WKID);
    gclose_gks();
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
 * For each area, one gets a set of points (using normalized device
 * coordinates), two group identifiers and their associated area
 * identifiers.  If both of the area identifiers are zero or negative,
 * the area need not be color-filled; otherwise, it is filled with
 * a color obtained from MAPACI.
 */
    int i, itm;
    Gpoint_list area;

    if (iai[0] >= 0 && iai[1] >= 0) {
        itm=max(iai[0],iai[1]);
        if (itm > 0) {
/*
 * Set area fill color index.
 */
            gset_fill_colr_ind(c_mapaci(itm));
/*
 * Create structure to pass to gfill_area.
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
 * Fill area.
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
 * For each line segment, one gets a set of points (using normalized
 * device coordinates), two group identifiers and their associated
 * area identifiers.  If both of the area identifiers are zero or
 * negative, the segment is not drawn; otherwise, we use mapaci to
 * see if the segment is over water and, if so, we draw the segment.
 */
    int i, itm;
    Gpoint_list line;

    if (iai[0] >= 0 && iai[1] >= 0) {
        itm=max(iai[0],iai[1]);
        if (c_mapaci(itm) == 1) {
/*
 * Create structure to pass to gfill_area.
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
