/*
 *	$Id: c_cpex08.c,v 1.3 1994-06-21 14:59:45 haley Exp $
 */
#include <stdio.h>
#include <math.h>
#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

/*
 * Define the list of labels required by the label-bar routine.
 */
char *llbs[14] = {
    "OCEAN","LAND","< 0","0-10","10-20","20-30","30-40","40-50","50-60",
    "60-70", "70-80","80-90","90-100", "> 100"
};

#define WSTYPE SED_WSTYPE
#define WKID   1

main()
{
/*
 * Declare required data arrays and workspace arrays.
 */
    float zdat[40][40],rwrk[1000],xcra[5000],ycra[5000];
    int iwrk[1000],iama[100000],iaia[10],igia[10];
    int i,lind[14];
    float time,p1[2],p2[2],p3[2],p4[2];
    Gasfs iasf;
/*
 * Declare a routine to color the areas represented by the area map.
 */
    extern int colrcl(
#ifdef NeedFuncProto
        float *xcra,
        float *ycra,
        int *ncra,
        int *iaia,
        int *igia,
        int *nagi
#endif
);
    extern int colram(
#ifdef NeedFuncProto
    float *xcra,
    float *ycra,
    int *ncra,
    int *iaia,
    int *igia,
    int *nagi
#endif
    );
    extern int colrll(
#ifdef NeedFuncProto
        float *xcra,
        float *ycra,
        int *ncra,
        int *iaia,
        int *igia,
        int *nagi
#endif
    );
/*
 * Define GKS aspect source flags.
 */
    iasf.linetype = 1;
    iasf.linewidth = 1;
    iasf.line_colr_ind = 1;
    iasf.marker_type = 1;
    iasf.marker_size = 1;
    iasf.marker_colr_ind = 1;
    iasf.text_font_prec = 1;
    iasf.char_expan = 1;
    iasf.char_space = 1;
    iasf.text_colr_ind = 1;
    iasf.fill_int_style = 1;
    iasf.fill_style_ind = 1;
    iasf.fill_colr_ind = 1;
/*
 * Define the list of indices required by the label-bar routine.
 */
    lind[0] = 7;
    for( i = 1; i < 6; i++ ) lind[i] = i+1 ;
    for( i = 6; i < 14; i++ ) lind[i] = i+2 ;
/*
 * Open GKS.
 */
	gopen_gks ("stdout",0);
	gopen_ws (WKID, NULL, WSTYPE);
	gactivate_ws(WKID);
/*
 * Turn off the clipping indicator.
 */
    gset_clip_ind (GIND_NO_CLIP);
/*
 * Set all aspect source flags to "individual".
 */
    gset_asfs(&iasf);
/*
 * Force solid fill.
 */
    gset_fill_int_style (GSTYLE_SOLID);
/*
 * Define color indices.
 */
    dfclrs();
/*
 * Generate an array of test data.
 */
    gendat (zdat,40,40,40,15,15,-10.,110.);
/*
 * Get the current elapsed time, in seconds.
 */
    time = 0;
/*
 * Initialize the area map.
 */
    c_arinam (iama,100000);
/*
 * Use EZMAP and EZMAPA to create a background.
 */
    p1[0] = p2[0] = p3[0] = p4[0] = 0.;
    p1[1] = p2[1] = p3[1] = p4[1] = 0.;
    c_mappos (.01,.74,.01,.99);
    c_maproj ("OR - ORTHOGRAPHIC PROJECTION",15.,15.,0.);
    c_mapset ("MA - MAXIMAL AREA",p1,p2,p3,p4);
    c_mapsti ("EL - ELLIPTICAL BOUNDARY",1);
    c_mapstc ("OU - OUTLINE DATASET","CO");
    c_mapsti ("VS - VERTICAL STRIPPING",0);
    c_mapint();
    c_mapbla (iama);
/*
 * Add a line segment to the area map to separate Africa from Eurasia.
 * Because this line segment is added last, it will determine the area
 * identifier for all of Africa (223).  Also, add a line cutting
 * Madagascar in two, with the same area identifier on both sides, so
 * that it will be treated as a part of Africa.
 */
    c_mapita (26.0,35.0,0,iama,1,223,0);
    c_mapita (28.8,33.0,1,iama,1,223,0);
    c_mapita (33.0,30.0,1,iama,1,223,0);
    c_mapiqa (iama,1,223,0);

    c_mapita (-20.0,42.5,0,iama,1,223,223);
    c_mapita (-20.0,50.0,1,iama,1,223,223);
    c_mapiqa (iama,1,223,223);
/*
 * Tell CONPACK not to do the SET call (since it"s already been done),
 * to use mapping function 1 (EZMAP background), and what range of X and
 * Y coordinates to send into the mapping function.  The X coordinates
 * will be treated as latitudes and will range from 40 degrees west of
 * Greenwich to 55 degrees east of Greenwich, and the Y coordinates will
 * be treated as latitudes and will range from 45 degrees south of the
 * equator to 45 degrees north of the equator.
 */
    c_cpseti ("SET - DO-SET-CALL FLAG",0);
    c_cpseti ("MAP - MAPPING FLAG",1);
    c_cpsetr ("XC1 - X COORDINATE AT I=1",-18.);
    c_cpsetr ("XCM - X COORDINATE AT I=M", 52.);
    c_cpsetr ("YC1 - Y COORDINATE AT J=1",-35.);
    c_cpsetr ("YCN - Y COORDINATE AT J=N", 38.);
/*
 * Tell CONPACK exactly what contour levels to use.
 */
    c_cpseti ("CLS - CONTOUR LEVEL SELECTOR",1);
    c_cpsetr ("CMN - CONTOUR LEVEL MINIMUM",0.);
    c_cpsetr ("CMX - CONTOUR LEVEL MAXIMUM",100.);
    c_cpsetr ("CIS - CONTOUR INTERVAL SPECIFIER",10.);
/*
 * Tell CONPACK what value EZMAP uses to signal that a projected point
 * has disappeared around the limb.  Strictly speaking, this call is
 * not necessary here; it has been inserted for the benefit of users
 * who modify the example to use global data.
 */
    c_cpsetr ("ORV - OUT-OF-RANGE VALUE",1.E12);
/*
 * Initialize the drawing of the contour plot.
 */
    c_cprect (&zdat[0][0],40,40,40,rwrk,1000,iwrk,1000);
/*
 * Add contour lines to the area map.
 */
    c_cpclam (&zdat[0][0],rwrk,iwrk,iama);
/*
 * Color the map.
 */
    c_arscam (iama,xcra,ycra,5000,iaia,igia,10,colram);
/*
 * Outline the continents in black, put black contour lines over the
 * color map, and put gray lines of latitude and longitude over the
 * ocean.
 */
    gset_line_colr_ind (0);
    c_maplot();
    c_cpcldm (&zdat[0][0],rwrk,iwrk,iama,colrcl);
    gset_line_colr_ind (2);
    c_mapgrm (iama,xcra,ycra,5000,iaia,igia,10,colrll);
    gset_line_colr_ind (1);
/*
 * Draw a label bar for the plot, relating colors to values.
 */
    c_lbseti ("CBL - COLOR OF BOX LINES",0);
    c_lblbar (1,.76,.99,.13,.87,14,.5,1.,lind,0,llbs,14,1);
/*
 * Compute and print statistics for the plot, label it, and put a
 * boundary line at the edge of the plotter frame.
 */
    capsap ("EXAMPLE 8",time,iama,100000);
    labtop ("EXAMPLE 8",.017);
    bndary();
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

int ioci[] = {3,4,5,6,8,9,10,11,12,13,14,15};

#ifdef __STDC__
int colram(
    float *xcra,
    float *ycra,
    int *ncra,
    int *iaia,
    int *igia,
    int *nagi
)
#else
colram (xcra,ycra,ncra,iaia,igia,nagi)
    float *xcra;
    float *ycra;
    int *ncra;
    int *iaia;
    int *igia;
    int *nagi;
#endif
{
/*
 * this routine is called to color an area from an area map.  its
 * coordinates are given by the ncra coordinates in the arrays xcra and
 * ycra.  for each i from 1 to nagi, iaia(i) is the area identifier of
 * the area relative to the group whose group identifier is igia(i).
 */
    int iai1, iai3, i;
    Gpoint_list fill_area;
/*
 * find the area identifier for the area relative to groups 1 and 3.
 * the first of these tells us whether the area is over land or water,
 * and the second tells us what contour band the area is in.
 */
    iai1 = -1;
    iai3 = -1;

    for( i = 0; i < *nagi; i++ ) {
        if (igia[i] == 1) iai1=iaia[i];
        if (igia[i] == 3) iai3=iaia[i];
    }
/*
 * color-fill the area, using blue for any area over water, gray for any
 * area over land which is not over africa or is outside the contour
 * plot, and a color depending on the contour level elsewhere.
 */
    if (iai1 > 0) {
        if (c_mapaci(iai1) == 1) {
            gset_fill_colr_ind (7);
        }
        else {
            if (iai1 != 223 || iai3 <= 0)  gset_fill_colr_ind (2);
            else                           gset_fill_colr_ind (ioci[iai3-1]);
        }
/*
 * Create structure to pass to gfill_area
 */
        fill_area.num_points = *ncra-1;
        fill_area.points = (Gpoint *) malloc(fill_area.num_points*sizeof(Gpoint));
        if( !fill_area.points ) {
            fprintf( stderr, "colram: Not enough memory to create fill area structure\n" );
            gemergency_close_gks();
            exit(1);
        }
        for( i = 0; i < *ncra-1; i++ ) {
            fill_area.points[i].x = xcra[i];
            fill_area.points[i].y = ycra[i];
        }
        gfill_area(&fill_area);
        free((Gpoint *)fill_area.points);
    }
    return(1);
}

#ifdef __STDC__
int colrcl(
    float *xcra,
    float *ycra,
    int *ncra,
    int *iaia,
    int *igia,
    int *nagi
)
#else
int colrcl(xcra,ycra,ncra,iaia,igia,nagi)
    float *xcra;
    float *ycra;
    int *ncra;
    int *iaia;
    int *igia;
    int *nagi;
#endif
{
/*
 * this routine is called to draw a portion of a contour line which is
 * wholly contained in some area of an area map.  its coordinates are
 * given by the ncra coordinates in the arrays xcra and ycra.  for each
 * i from 1 to nagi, iaia(i) is the area identifier of the area relative
 * to the group whose group identifier is igia(i).
 */
    int i, iai1, iai3;
    Gpoint_list line;
/*
 * find the area identifier for the area relative to groups 1 and 3.
 * the first of these tells us whether the area is over land or water,
 * and the second tells us what contour band the area is in.
 */
    iai1 = -1;
    iai3 = -1;

    for( i = 0; i < *nagi; i++ ) {
        if (igia[i] == 1) iai1=iaia[i];
        if (igia[i] == 3) iai3=iaia[i];
    }
/*
 * draw the line only if the area it is in is over africa and within
 * the boundary of the contour plot.
 */
    if (iai1 == 223 && iai3 > 0) {
/*
 * Create structure to pass to gpolyline
 */
        line.num_points = *ncra;
        line.points = (Gpoint *) malloc(line.num_points*sizeof(Gpoint));
        if( !line.points ) {
            fprintf( stderr, "colrcl: Not enough memory to create fill area structure\n" );
            gemergency_close_gks();
            exit(1);
        }
        for( i = 0; i < *ncra; i++ ) {
            line.points[i].x = xcra[i];
            line.points[i].y = ycra[i];
        }
        gpolyline(&line);
        free((Gpoint *)line.points);
    }
    return(1);
}

#ifdef __STDC__
int colrll(
    float *xcra,
    float *ycra,
    int *ncra,
    int *iaia,
    int *igia,
    int *nagi
)
#else
int colrll (xcra,ycra,ncra,iaia,igia,nagi)
    float *xcra;
    float *ycra;
    int *ncra;
    int *iaia;
    int *igia;
    int *nagi;
#endif
{
/*
 * this routine is called to draw a portion of a line of latitude or
 * longitude which is wholly contained in some area of an area map.  its
 * coordinates are given by the ncra coordinates in the arrays xcra and
 * ycra.  for each i from 1 to nagi, iaia(i) is the area identifier of
 * the area relative to the group whose group identifier is igia(i).
 */
    int iai1, i;
    Gpoint_list line;
/*
 * find the area identifier for the area relative to group 1, which will
 * tell us whether the area is over land or water.
 */
    iai1 = -1;

    for( i = 0; i < *nagi; i++ ) {
        if (igia[i] == 1 && iaia[i] > 0) iai1=iaia[i];
    }
/*
 * draw the line only if it is over water.
 */
    if (iai1 > 0 && c_mapaci(iai1) == 1) {
/*
 * Create structure to pass to gpolyline
 */
        line.num_points = *ncra;
        line.points = (Gpoint *) malloc(line.num_points*sizeof(Gpoint));
        if( !line.points ) {
            fprintf( stderr, "colrcl: Not enough memory to create fill area structure\n" );
            gemergency_close_gks();
            exit(1);
        }
        for( i = 0; i < *ncra; i++ ) {
            line.points[i].x = xcra[i];
            line.points[i].y = ycra[i];
        }
        gpolyline(&line);
        free((Gpoint *)line.points);
    }
    return(1);
}
