/*
 *	$Id: c_cpex07.c,v 1.2 1994-05-24 22:42:05 haley Exp $
 */
#include <stdio.h>
#include <math.h>

#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

main()
{

/*
 * Declare required data arrays and workspace arrays.
 */
    float zdat[14][23],rwrk[1000], xcra[1000],ycra[1000];
    float time,zmin,zmax,zval;
    int iwrk[1000],iama[12000],iaia[10],igia[10],lind[14];
    int i;
    Gasfs iasf;
/*
 * Declare arrays to hold the list of indices and the list of labels
 * required by the label-bar routine.
 */
    char stmp[11], *llbs[15];
/*
 * Declare the routine which will color the areas.
 */
    extern int colram();
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
    for( i = 0; i < 14; i++ ) lind[i] = i+2;
/*
 * Open GKS.
 */
    c_opngks();
/*
 * Turn off the clipping indicator.
 */
    gset_clip_ind (GIND_NO_CLIP);
/*
 * Set all aspect source flags to "individual".
 */
    gset_asfs (&iasf);
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
    gendat(zdat,23,23,14,20,20,-136.148,451.834);
/*
 * Get the current elapsed time, in seconds.
 */
    time = 0.;
/*
 * Force the plot into the upper portion of the frame.
 */
    c_cpsetr ("VPB - VIEWPORT BOTTOM",.25);
/*
 * Disallow the trimming of trailing zeroes.
 */
    c_cpseti ("NOF - NUMERIC OMISSION FLAGS",0);
/*
 * Tell CONPACK to use 13 contour levels, splitting the range into 14
 * equal bands, one for each of the 14 colors available.
 */
    c_cpseti ("CLS - CONTOUR LEVEL SELECTOR",-13);
/*
 * Initialize the drawing of the contour plot.
 */
    c_cprect (&zdat[0][0],23,23,14,rwrk,1000,iwrk,1000);
/*
 * Initialize the area map and put the contour lines into it.
 */
    c_arinam (iama,12000);
    c_cpclam (&zdat[0][0],rwrk,iwrk,iama);
/*
 * Color the map.
 */
    c_arscam (iama,xcra,ycra,1000,iaia,igia,10,colram);
/*
 * Put black contour lines over the colored map.
 */
    gset_line_colr_ind (0);
    c_cpcldr (&zdat[0][0],rwrk,iwrk);
    gset_line_colr_ind (1);
/*
 * Draw a label bar for the plot, relating colors to values.
 */
    c_cpgetr ("ZMN",&zmin);
    c_cpgetr ("ZMX",&zmax);

    for( i = 0; i < 15; i++ ) {
        strcpy( stmp, "          " );
        zval = zmin+(float)(i)*(zmax-zmin)/14.;
        c_cpsetr( "ZDV - Z DATA VALUE", zval );
        c_cpgetc("ZDV - Z DATA VALUE", stmp, 10 );
        llbs[i] = (char *)malloc((strlen(stmp)+1)*sizeof(char));
        strcpy( llbs[i], stmp );
    }

    c_lbseti ("CBL - COLOR OF BOX LINES",0);
    c_lblbar (0,.05,.95,.15,.25,14,1.,.5,lind,0,llbs,15,1);
/*
 * Compute and print statistics for the plot, label it, and put a
 * boundary line at the edge of the plotter frame.
 */
    capsap ("EXAMPLE 7",time,iama,12000);
    labtop ("EXAMPLE 7",.017);
    bndary();
/*
 * Advance the frame.
 */
    c_frame();
/*
 * Close GKS.
 */
    c_clsgks();
}

colram (xcra,ycra,ncra,iaia,igia,naia)
float *xcra, *ycra;
int *iaia, *igia, *naia, *ncra;
{
/*
 * the arrays xcra and ycra, for indices 1 to ncra, contain the x and y
 * coordinates of points defining a polygon.  the area identifiers in
 * the array iaia, each with an associated group identifier in the array
 * igia, tell us whether the polygon is to be color-filled or not.
 */
    int ifll, i;
    Gpoint_list fill_area;
/*
 * assume the polygon will be filled until we find otherwise.
 */
ifll=1;
/*
 * if any of the area identifiers is negative, don't fill the polygon.
 */
    for( i = 0; i < *naia; i++ ) {
        if (iaia[i] < 0) ifll=0;
    }
/*
 * otherwise, fill the polygon in the color implied by its area
 * identifier relative to edge group 3 (the contour-line group).
 */
    if (ifll) {
        ifll=0;
        for( i = 0; i < *naia; i++ ) {
            if (igia[i] == 3) ifll=iaia[i];
        }
        if (ifll > 0 && ifll < 15) {
            gset_fill_colr_ind (ifll+1);
/*
 * Create structure to pass to gfill_area
 */
            fill_area.num_points = *ncra-1;
            fill_area.points = (Gpoint *) malloc((*ncra-1)*sizeof(Gpoint));
            if( !fill_area.points ) {
                fprintf( stderr, "colram: Not enough memory to create fill area structure\n" );
                gemergency_close_gks();
                exit(1);
            }
            for( i = 0; i < *ncra-1; i++ ) {
                fill_area.points[i].x = xcra[i];
                fill_area.points[i].y = ycra[i];
            }
/*
 * Fill area
 */
            gfill_area (&fill_area);
        }
    }
    return(1);
}
