/*
 *	$Id: c_cpex05.c,v 1.2 1994-06-21 14:59:41 haley Exp $
 */
#include <stdio.h>
#include <math.h>
#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

char *llbs[21] = {
 "0"," ","1"," ","2"," ","3"," ","4"," ","5"," ","6"," ","7"," ","8"," ","9",
 " ","10"
};

#define WSTYPE SED_WSTYPE
#define WKID   1

main()
{
/*
 * Declare an array to hold the data to be contoured.
 */
    float zdat[53][37],rwrk[5000],xcra[1000],ycra[1000];
    float time;
    int iwrk[1000],iama[40000],iara[10],igra[10],lind[20];
    int i;
    extern int shadam();
    Gasfs iasf;
/*
 * Define the list of indices and the list of labels required by the
 * label-bar routine.
 */
    for( i = 0; i < 20; i++ ) lind[i] = i;
/*
 * Initialize the values in the aspect-source-flag structure.
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
	gopen_gks ("stdout",0);
	gopen_ws (WKID, NULL, WSTYPE);
	gactivate_ws(WKID);
/*
 *   turn off the clipping indicator.
 */
    gset_clip_ind (GIND_NO_CLIP);
/*
 * Set all the GKS aspect source flags to "individual".
 */
    gset_asfs (&iasf);
/*
 * Generate a relatively smooth array of test data, with values ranging
 * from 0 to 10.
 */
    gendat (zdat,53,53,37,10,10,0.,10.);
/*
 * Initialize the software fill package to do the desired type of fill.
 */
    c_sfseti ("TYPE OF FILL",-4);
    c_sfseti ("ANGLE OF FILL LINES",15);
    c_sfsetr ("SPACING OF FILL LINES",.000625);
/*
 * Get the current elapsed time, in seconds.
 */
        time=0;
/*
 * Turn off line labels.
 */
    c_cpseti ("LLP - LINE LABEL POSITIONING",0);
/*
 * Turn off the drawing of high and low labels.
 */
    c_cpsetc ("HLT - HIGH/LOW LABEL TEXT"," ");
/*
 * Turn on the drawing of the grid edge ("contour line number -1") and
 * thicken it somewhat.
 */
    c_cpseti ("PAI - PARAMETER ARRAY INDEX",-1);
    c_cpseti ("CLU - CONTOUR LEVEL USE FLAG",1);
    c_cpsetr ("CLL - CONTOUR LEVEL LINE WIDTH",2.);
/*
 * Turn off the informational label.
 */
    c_cpsetc ("ILT - INFORMATIONAL LABEL TEXT"," ");
/*
 * Force the use of contour lines at the values 1, 2, 3, ... 9 and
 * values half-way between.  Arrange for the areas between contour lines
 * to be shaded.
 */
    c_cpseti ("CLS - CONTOUR LEVEL SELECTION",0);

    c_cpseti ("NCL - NUMBER OF CONTOUR LEVELS",19);

    for( i = 1; i <= 19; i++ ) {
        c_cpseti ("PAI - PARAMETER ARRAY IDENTIFIER",i);
        c_cpsetr ("CLV - CONTOUR LEVEL VALUE",(float)(i)/2.);
        c_cpseti ("CLU - CONTOUR LEVEL USE",1);
        c_cpsetr ("CLL - CONTOUR LEVEL LINE WIDTH",2.);
        c_cpseti ("AIA - AREA IDENTIFIER ABOVE LINE",i+1);
        c_cpseti ("AIB - AREA IDENTIFIER BELOW LINE",i);
    }
/*
 * Force the plot into the upper portion of the frame.
 */
    c_cpsetr ("VPB - VIEWPORT BOTTOM",.25);
/*
 * Initialize the drawing of the contour plot.
 */
    c_cprect (&zdat[0][0],53,53,37,rwrk,5000,iwrk,1000);
/*
 * Draw the contour plot.
 */
    c_arinam (iama,40000);
    c_cpcldr (&zdat[0][0],rwrk,iwrk);
    c_cpclam (&zdat[0][0],rwrk,iwrk,iama);
    c_arscam (iama,xcra,ycra,1000,iara,igra,10,shadam);
/*
 * Put a horizontal label bar at the bottom of the frame.
 */
    c_lbsetr ("WBL - WIDTH OF BOX LINES",2.);
    c_lblbar (0,.05,.95,.15,.25,20,1.,.5,lind,0,llbs,21,1);
/*
 * Compute and print statistics for the plot and label it.
 */
    capsap ("EXAMPLE 5",time,iama,40000);
    labtop ("EXAMPLE 5",.017);
/*
 * Put a boundary line at the edge of the plotter frame.
 */
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

shadam (xcs,ycs,ncs,iai,iag,nai)
float *xcs, *ycs;
int *ncs, *iai, *iag, *nai;
{
/*
 * this version of shadam shades the area-map polygon whose edge is
 * defined by the points ((xcs(i),ycs(i)),i=1,ncs) if and only, relative
 * to edge group 3, its area identifier is between 1 and 20.  the density
 * of the shading increases with the area identifier.
 */
/*
 * declare the workspaces required by sfsgfa.
 */
    float dst[1100];
    int i, ish, ind[1200];
/*
 * turn off shading.
 */
    ish=0;
/*
 * if the area identifier for group 3 is in the right range, turn on
 * shading.
 */
    for( i = 0;  i < *nai; i++ ) {
        if (iag[i] == 3 && iai[i] >= 1 && iai[i] <= 20) ish=iai[i];
    }
/*
 * if shading is turned on, shade the area.  the last point of the
 * edge is redundant and may be omitted.
 */
    if (ish) c_sfsgfa (xcs,ycs,*ncs,dst,1100,ind,1200,ish-1);
    return(1);
}
