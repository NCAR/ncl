/*
 * $Id: c_ccppole.c,v 1.3 1994-05-26 21:48:46 haley Exp $
 */

#include <stdio.h>
#include <math.h>

#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define min(x,y)    ((x) < (y) ? (x) : (y))
#define max(x,y)    ((x) > (y) ? (x) : (y))

/*
 * Parameterize the number of latitudes, the number of longitudes, the
 * sizes of the real and integer workspaces, the size of the area map,
 * and the size of the arrays used by ARSCAM for X and Y coordinates.
 */
#define NLON  361
#define NLAT  181
#define LRWK  5000
#define LIWK  5000
#define LAMA  600000
#define NCRA  20000

/*
 * Declare the data array.
 */
float zdat[NLAT][NLON];
/*
 * Declare the area map array.
 */
int iama[LAMA];
/*
 * Declare the real and integer workspace arrays for CONPACK.
 */
float rwrk[LRWK];
int iwrk[LIWK];
/*
 * Declare arrays for ARSCAM and MAPGRM to use in calls to COLRAM and
 * COLRLL, respectively.  XCRA and YCRA hold X and Y coordinates; IAIA
 * and IGIA hold area identifiers and group identifiers.
 */
float xcra[NCRA],ycra[NCRA];
int iaia[10],igia[10];

main()
{
/*
 * Declare an array in which to retrieve aspect source flags.
 */
	Gasfs iasf;
	Gcolr_rep rgb;
/*
 * Declare a routine to color the areas represented by the area map.
 */
    extern int colram(
    float *xcra,
    float *ycra,
    int *ncra,
    int *iaia,
    int *igia,
    int *nagi
    );
/*
 * Declare a routine to draw contour lines over land only.
 */
	extern int colrcl(
    float *xcra,
    float *ycra,
    int *ncra,
    int *iaia,
    int *igia,
    int *nagi
    );
/*
 * Declare a routine to draw lat/lon lines over ocean only.
 */
    extern int colrll(
        float *xcra,
        float *ycra,
        int *ncra,
        int *iaia,
        int *igia,
        int *nagi
    );
	int i, j;
	float zmin, zmax, rlat, rlon;
    float pm1[2], pm2[2], pm3[2], pm4[2];
/*
 * Define the values to be used for GKS aspect source flags.
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
 * Open GKS.
 */
	c_opngks();
/*
 * Turn off the clipping indicator.
 */
	gset_clip_ind(GIND_NO_CLIP);
/*
 * Set all aspect source flags to "individual".
 */
	gset_asfs(&iasf);
/*
 * Force solid fill.
 */
	gset_fill_int_style(GSTYLE_SOLID);
/*
 * Define 16 different color indices, for indices 0 through 15.  The
 * color corresponding to index 0 is black and the color corresponding
 * to index 1 is white.  Colors 2 and 3 are used for alternate contour
 * bands over ocean.  Colors 4 through 16 are used for contour bands
 * over land.
 */
	rgb.rgb.red = 0.000; rgb.rgb.green = 0.000; rgb.rgb.blue = 0.000;
	gset_colr_rep (1, 0, &rgb );
	rgb.rgb.red = 1.000; rgb.rgb.green = 1.000; rgb.rgb.blue = 1.000;
	gset_colr_rep (1, 1, &rgb );
	rgb.rgb.red = 0.000; rgb.rgb.green = 1.000; rgb.rgb.blue = 1.000;
	gset_colr_rep (1, 2, &rgb );
	rgb.rgb.red = 0.000; rgb.rgb.green = 0.850; rgb.rgb.blue = 0.850;
	gset_colr_rep (1, 3, &rgb );
	rgb.rgb.red = 0.700; rgb.rgb.green = 0.700; rgb.rgb.blue = 0.700;
	gset_colr_rep (1, 4, &rgb );
	rgb.rgb.red = 0.750; rgb.rgb.green = 0.500; rgb.rgb.blue = 1.000;
	gset_colr_rep (1, 5, &rgb );
	rgb.rgb.red = 0.500; rgb.rgb.green = 0.000; rgb.rgb.blue = 1.000;
	gset_colr_rep (1, 6, &rgb );
	rgb.rgb.red = 0.000; rgb.rgb.green = 0.000; rgb.rgb.blue = 1.000;
	gset_colr_rep (1, 7, &rgb );
	rgb.rgb.red = 0.000; rgb.rgb.green = 0.500; rgb.rgb.blue = 1.000;
	gset_colr_rep (1, 8, &rgb );
	rgb.rgb.red = 0.000; rgb.rgb.green = 1.000; rgb.rgb.blue = 0.600;
	gset_colr_rep (1, 9, &rgb );
	rgb.rgb.red = 0.000; rgb.rgb.green = 1.000; rgb.rgb.blue = 0.000;
	gset_colr_rep (1,10, &rgb );
	rgb.rgb.red = 0.700; rgb.rgb.green = 1.000; rgb.rgb.blue = 0.000;
	gset_colr_rep (1,11, &rgb );
	rgb.rgb.red = 1.000; rgb.rgb.green = 1.000; rgb.rgb.blue = 0.000;
	gset_colr_rep (1,12, &rgb );
	rgb.rgb.red = 1.000; rgb.rgb.green = 0.750; rgb.rgb.blue = 0.000;
	gset_colr_rep (1,13, &rgb );
	rgb.rgb.red = 1.000; rgb.rgb.green = 0.380; rgb.rgb.blue = 0.380;
	gset_colr_rep (1,14, &rgb );
	rgb.rgb.red = 1.000; rgb.rgb.green = 0.000; rgb.rgb.blue = 0.380;
	gset_colr_rep (1,15, &rgb );
	rgb.rgb.red = 1.000; rgb.rgb.green = 0.000; rgb.rgb.blue = 0.000;
	gset_colr_rep (1,16, &rgb );
/*
 * Generate an array of test data.  It is important that the data should
 * represent a continous function around the globe.  What is used here
 * is a simple trigonometric function of latitude and longitude.
 */
	zmin = 1.e36;
	zmax = -1.e36;

	for( i = 0; i < NLON; i++ ) {
		rlon=.017453292519943*(-180.+360.*(float)i/(float)(NLON-1));
		for( j = 0; j < NLAT; j++ ) {
            rlat=.017453292519943*(-90.+180.*(float)j/(float)(NLAT-1));
            zdat[j][i] = .5*cos(8.*rlat)+.25*cos(rlat)*sin(4.*rlon);
            zmin = min(zmin,zdat[j][i]);
            zmax = max(zmax,zdat[j][i]);
		}
	}
/*
 * Reduce the test data to the desired range.
 */
	for( i = 0; i < NLON; i++ ) {
		for( j = 0; j < NLAT; j++ ) {
            zdat[j][i] = ((zdat[j][i]-zmin)/(zmax-zmin))*130.-10.;
		}
	}
/*
 * Put a label at the top of the plot.
 */
	c_set    (0.,1.,0.,1.,0.,1.,0.,1.,1);
	c_plchhq (.5,.975,"CONTOUR BANDS ON A STEREOGRAPHIC PROJECTION", .02,0.,0.);
/*
 * Initialize the area map.
 */
	  c_arinam (iama,LAMA);
/*
 * Tell Ezmap where to put the plot on the plotter frame.
 */
	c_mappos (.03,.97,.01,.95);
/*
 * Tell Ezmap to use a stereographic projection.
 */
	c_maproj ("ST",90.,0.,0.);
/*
 * Tell Ezmap to use a 90-degree distance to each of the four edges
 * of the map.
 */
    pm1[0] = pm2[0] = pm3[0] = pm4[0] = 90.;
	c_mapset ("AN - ANGLES",pm1,pm2,pm3,pm4);
/*
 * Initialize Ezmap.
 */
	c_mapint();
/*
 * Put continental outlines in the area map.
 */
	c_mapbla (iama);
/*
 * Tell CONPACK not to do the SET call (since it's already been done)
 * and to use mapping function 1 (EZMAP background).
 */
	c_cpseti ("SET - DO-SET-CALL FLAG",0);
	c_cpseti ("MAP - MAPPING FLAG",1);
/*
 * Tell CONPACK what ranges of X and Y coordinates to send into the
 * mapping function.
 */
	c_cpsetr ("XC1 - X COORDINATE AT I=1",-180.);
	c_cpsetr ("XCM - X COORDINATE AT I=M",+180.);
	c_cpsetr ("YC1 - Y COORDINATE AT J=1", -90.);
	c_cpsetr ("YCN - Y COORDINATE AT J=N", +90.);
/*
 * Tell CONPACK exactly what contour levels to use.
 */
	c_cpseti ("CLS - CONTOUR LEVEL SELECTOR",1);
	c_cpsetr ("CMN - CONTOUR LEVEL MINIMUM",0.);
	c_cpsetr ("CMX - CONTOUR LEVEL MAXIMUM",110.);
	c_cpsetr ("CIS - CONTOUR INTERVAL SPECIFIER",10.);
/*
 * Tell CONPACK what to use as the out-of-range flag.  This is the
 * value returned by the Ezmap routine MAPTRA for off-map points.
 */
	c_cpsetr ("ORV - OUT-OF-RANGE VALUE",1.e12);
/*
 * Initialize the drawing of the contour plot.
 */
	c_cprect ((float *)zdat,NLON,NLON,NLAT,rwrk,LRWK,iwrk,LIWK);
/*
 * Add contour lines to the area map.
 */
	c_cpclam ((float *)zdat,rwrk,iwrk,iama);
/*
 * Color the map.
 */
	c_arscam (iama,xcra,ycra,NCRA,iaia,igia,10,colram);
/*
 * Switch the current polyline color to black.
 */
	gset_line_colr_ind(0);
/*
 * Outline the continents and draw lines of latitude and longitude over
 * the ocean only.
 */
	c_mapgrm (iama,xcra,ycra,NCRA,iaia,igia,10,colrll);
	c_mapstc ("OU - OUTLINE DATASET","CO");
	c_maplot();
/*
 * Draw the contour lines over land only.
 */
	c_cpcldm ((float *)zdat,rwrk,iwrk,iama,colrcl);
/*
 * advance the frame.
 */
	c_frame();
/*
 * Close GKS.
 */
	c_clsgks();
/*
 * Done.
 */
}

int colram(
    float *xwrk,
    float *ywrk,
    int *nwrk,
    int *iaia,
    int *igia,
    int *nagi
)
{
/*
 * This routine is called to color an area from an area map.  Its
 * coordinates are given by the NWRK coordinates in the arrays XWRK and
 * YWRK.  For each I from 1 to NAGI, IAIA(I) is the area identifier of
 * the area relative to the group whose group identifier is IGIA(I).
 *
 * Find the area identifier for the area relative to groups 1 and 3.
 * The first of these tells us whether the area is over land or water,
 * and the second tells us what contour band the area is in.
 */
	int i, iai1, iai3, fill;
	Gpoint_list fill_area;

	iai1 = -1;
	iai3 = -1;

    for( i = 0; i < *nagi; i++ ) {
        if (igia[i] == 1) iai1=iaia[i];
        if (igia[i] == 3) iai3=iaia[i];
    }
/*
 * Color-fill the area, using two slightly-different shades of blue
 * over water (so that the contour bands will be minimally visible)
 * and brighter colors over land.
 */
    fill = 0;
    if (iai1 > 0) {
        if (c_mapaci(iai1) == 1) {
            fill = 1;
			gset_fill_colr_ind(2+(iai3%2));
		}
        else {
            if (iai3 >= 1 && iai3 <= 13)  {
                fill = 1;
                gset_fill_colr_ind (iai3+3);
            }
        }
        if( fill ) {
/*

 * Create structure to pass to gfill_area
 */
            fill_area.num_points = *nwrk-1;
            fill_area.points = (Gpoint *) malloc(fill_area.num_points*sizeof(Gpoint));
            if( !fill_area.points ) {
                fprintf( stderr, "colram: Not enough memory to create fill area structure\n" );
                gemergency_close_gks();
                exit(1);
            }
			for( i = 0; i < *nwrk-1; i++ ) {
				fill_area.points[i].x = xwrk[i];
				fill_area.points[i].y = ywrk[i];
			}
			gfill_area(&fill_area);
			free((Gpoint *)fill_area.points);
        }
    }
/*
 * Done.
 */
    return(0);
}

int colrcl(
    float *xwrk,
    float *ywrk,
    int *nwrk,
    int *iaia,
    int *igia,
    int *nagi
)
{
/*
 * This routine is called to draw a portion of a contour line which is
 * wholly contained in some area of an area map.  Its coordinates are
 * given by the NWRK coordinates in the arrays XWRK and YWRK.  For each
 * I from 1 to NAGI, IAIA(I) is the area identifier of the area relative
 * to the group whose group identifier is IGIA(I).
 *
 * Find the area identifier for the area relative to groups 1 and 3.
 * The first of these tells us whether the area is over land or water,
 * and the second tells us what contour band the area is in.
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
 * Draw the line only if the area it is in is over land.
 */
	if (iai1 > 0 && c_mapaci(iai1)!= 1) {
/*
 * Create structure to pass to gpolyline
 */
        line.num_points = *nwrk;
        line.points = (Gpoint *) malloc(line.num_points*sizeof(Gpoint));
        if( !line.points ) {
            fprintf( stderr, "colrcl: Not enough memory to create fill area structure\n" );
            gemergency_close_gks();
            exit(1);
        }
        for( i = 0; i < *nwrk; i++ ) {
            line.points[i].x = xwrk[i];
            line.points[i].y = ywrk[i];
        }
        gpolyline(&line);
        free((Gpoint *)line.points);
    }
/*
 * Done.
 */
	return(0);
}

int colrll(
    float *xwrk,
    float *ywrk,
    int *nwrk,
    int *iaia,
    int *igia,
    int *nagi
)
{
/*
 * This routine is called to draw a portion of a line of latitude or
 * longitude which is wholly contained in some area of an area map.  Its
 * coordinates are given by the NWRK coordinates in the arrays XWRK and
 * YWRK.  For each I from 1 to NAGI, IAIA(I) is the area identifier of
 * the area relative to the group whose group identifier is IGIA(I).
 *
 * Find the area identifier for the area relative to group 1, which will
 * tell us whether the area is over land or water.
 */
    int iai1, i;
    Gpoint_list line;

    for( i = 0; i < *nagi; i++ ) {
        if (igia[i] == 1 && iaia[i] > 0) iai1=iaia[i];
    }
/*
 * Draw the line only if it is over water.
 */
	if (iai1 > 0 && c_mapaci(iai1) == 1) {
/*
 * Create structure to pass to gpolyline
 */
		line.num_points = *nwrk;
		line.points = (Gpoint *) malloc(line.num_points*sizeof(Gpoint));
		if( !line.points ) {
			fprintf( stderr, "colrcl: Not enough memory to create fill area structure\n" );
            gemergency_close_gks();
            exit(1);
        }
        for( i = 0; i < *nwrk; i++ ) {
            line.points[i].x = xwrk[i];
            line.points[i].y = ywrk[i];
        }
        gpolyline(&line);
        free((Gpoint *)line.points);
	}
/*
 * Done.
 */
	return(0);
}
