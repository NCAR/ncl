/*
 *	$Id: c_cpex01.c,v 1.1 1994-05-31 22:28:26 haley Exp $
 */
#include <stdio.h>
#include <math.h>

#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

main()
{
/*
 * Declare an array to hold the data to be contoured.
 */
	float zdat[14][23];
/*
 * Declare an array to hold dense data (5152=4x4x23x14).
 */
	float zdns[5152];
/*
 * Declare the required real and integer workspaces.
 */
	float rwrk[5000];
	int iwrk[1000];
/*
 * Declare an array to hold an area map.
 */
	int iama[20000];
/*
 * Declare the arrays needed by ARSCAM for x/y coordinates.
 */
	float xcra[1000],ycra[1000];
/*
 * Declare the arrays needed by ARSCAM for area and group identifiers.
 */
	int iara[10],igra[10];
/*
 * Declare an array to hold the GKS "aspect source flags".
 */
	Gasfs iasf;
/*
 * Declare the routine which will draw contour lines, avoiding labels.
 */
	extern int drawcl();
/*
 * Declare the routine which does the shading.
 */
	extern int shader();
	
    float p1[2],p2[2],p3[2],p4[2];
	int iclv, iclu, nclv, linu;
	float clev, cinu;
/*
 * Initialize the values in the aspect-source-flag array.
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
    gset_clip_ind (GIND_NO_CLIP);
/*
 * Set all the GKS aspect source flags to "individual".
 */
    gset_asfs (&iasf);
/*
 * Generate an array of test data.
 */
	gendat (zdat,23,23,14,20,20,-136.148,451.834);
/*
 * Example 1-1 ---------------------------------------------------------
 *
 * Force PLOTCHAR to use characters of the lowest quality.
 */
	c_pcseti ("QU - QUALITY FLAG",2);
/*
 * Initialize the drawing of the contour plot.
 */
	c_cprect ((float *)zdat,23,23,14,rwrk,5000,iwrk,1000);
/*
 * Draw the default background.
 */
	c_cpback ((float *)zdat,rwrk,iwrk);
/*
 * Draw contour lines and labels.
 */
	c_cpcldr ((float *)zdat,rwrk,iwrk);
/*
 * Add the informational label and the high/low labels.
 */
	c_cplbdr ((float *)zdat,rwrk,iwrk);
/*
 * Label plot, and put a boundary line around the edge of the
 * plotter frame.
 */
	labtop ("EXAMPLE 1-1",.017);
	bndary();
/*
 * Advance the frame.
 */
	c_frame();
/*
 * Example 1-2 ---------------------------------------------------------
 *
 * Make PLOTCHAR use medium-quality characters.
 */
	c_pcseti ("QU - QUALITY FLAG",1);
/*
 * Turn on the positioning of labels by the penalty scheme.
 */
	c_cpseti ("LLP - LINE LABEL POSITIONING",3);
/*
 * Turn on the drawing of the high and low label boxes.
 */
	c_cpseti ("HLB - HIGH/LOW LABEL BOX FLAG",1);
/*
 * Tell CONPACK to delete high/low labels which overlap the informational
 * label, another high/low label, or the edge.
 */
	c_cpseti ("HLO - HIGH/LOW LABEL OVERLAP FLAG",7);
/*
 * Tell CONPACK not to choose contour levels, so that the ones chosen
 * for example 1-1 will be used.
 */
	c_cpseti ("CLS - CONTOUR LEVEL SELECTION FLAG",0);
/*
 * Increase the line width for labelled levels.
 */
	c_cpgeti ("NCL - NUMBER OF CONTOUR LEVELS",&nclv);
	for( iclv=1; iclv <= nclv; iclv++ ) { 
		c_cpseti ("PAI - PARAMETER ARRAY INDEX",iclv);
		c_cpgeti ("CLU - CONTOUR LEVEL USE FLAG",&iclu);
		if (iclu == 3) {
            c_cpseti ("CLL - CONTOUR-LINE LINE WIDTH",2);
		}
	}
/*
 * Initialize the drawing of the contour plot.
 */
	c_cprect ((float *)zdat,23,23,14,rwrk,5000,iwrk,1000);
/*
 * Draw the default background, using a wider line than normal.
 */
	gset_linewidth (2.);
	c_cpback ((float *)zdat,rwrk,iwrk);
	gset_linewidth (1.);
/*
 * Initialize the area map.
 */
	c_arinam (iama,20000);
/*
 * Put label boxes into the area map.
 */
	c_cplbam ((float *)zdat,rwrk,iwrk,iama);
/*
 * Draw contour lines, avoiding drawing them through the label boxes.
 */
	c_cpcldm ((float *)zdat,rwrk,iwrk,iama,drawcl);
/*
 * draw all the labels.
 */
	c_cplbdr ((float *)zdat,rwrk,iwrk);
/*
 * compute and print statistics for the plot, label it, and put a
 * boundary line around the edge of the plotter frame.
 */
	labtop ("EXAMPLE 1-2",.017);
	bndary();
/*
 * advance the frame.
 */
	c_frame();
/*
 * Example 1-3 ---------------------------------------------------------
 *
 * Make PLOTCHAR use high-quality characters.
 */
	c_pcseti ("QU - QUALITY FLAG",0);
/*
 * Tell CONPACK to delete high/low labels which overlap the informational
 * label or another high/low label, but to move those which overlap the
 * edge inward a little.
 */
	c_cpseti ("HLO - HIGH/LOW LABEL OVERLAP FLAG",11);
/*
 * Turn off the area identifiers for all except the zero contour and set
 * its identifiers in such a way that we can shade the areas "below" that
 * contour.
 */
	for( iclv=1; iclv <= nclv; iclv++ ) { 
		c_cpseti ("PAI - PARAMETER ARRAY INDEX",iclv);
		c_cpgetr ("CLV - CONTOUR LEVEL VALUE",&clev);
		if (clev != 0.) {
            c_cpseti ("AIA - AREA IDENTifIER ABOVE LINE",0);
            c_cpseti ("AIB - AREA IDENTifIER BELOW LINE",0);
		}
		else {
            c_cpseti ("AIA - AREA IDENTifIER ABOVE LINE",2);
            c_cpseti ("AIB - AREA IDENTifIER BELOW LINE",1);
		}
	}
/*
 * Draw the contour plot, using the same calls as for example 1-2.
 */
	c_cprect ((float *)zdat,23,23,14,rwrk,5000,iwrk,1000);
	gset_linewidth (2.);
	c_cpback ((float *)zdat,rwrk,iwrk);
	gset_linewidth (1.);
	c_arinam (iama,20000);
	c_cplbam ((float *)zdat,rwrk,iwrk,iama);
	c_cpcldm ((float *)zdat,rwrk,iwrk,iama,drawcl);
	c_cplbdr ((float *)zdat,rwrk,iwrk);
/*
 * Now, add the zero contour line to the area map.
 */
	c_cpclam ((float *)zdat,rwrk,iwrk,iama);
/*
 * Scan the area map.  The routine SHADER will be called to shade the
 * areas below the zero contour line.
 */
	c_arscam (iama,xcra,ycra,1000,iara,igra,10,shader);
/*
 * Label plot, and put a boundary line around the edge of the plotter frame.
 */
	labtop ("EXAMPLE 1-3",.017);
	bndary();
/*
 * Advance the frame.
 */
	c_frame();
/*
 * Example 1-4 ---------------------------------------------------------
 *
 * Turn on the 2D smoother.
 */
	c_cpsetr ("T2D - TENSION ON THE 2D SPLINES",1.);
/*
 * Draw the contour plot, using the same calls as for example 1-3.
 */
	c_cprect ((float *)zdat,23,23,14,rwrk,5000,iwrk,1000);
	gset_linewidth (2.);
	c_cpback ((float *)zdat,rwrk,iwrk);
	gset_linewidth (1.);
	c_arinam (iama,20000);
	c_cplbam ((float *)zdat,rwrk,iwrk,iama);
	c_cpcldm ((float *)zdat,rwrk,iwrk,iama,drawcl);
	c_cplbdr ((float *)zdat,rwrk,iwrk);
	c_cpclam ((float *)zdat,rwrk,iwrk,iama);
	c_arscam (iama,xcra,ycra,1000,iara,igra,10,shader);
/*
 * Label plot, and put a boundary line around the edge of the plotter frame.
 */
	labtop ("EXAMPLE 1-4",.017);
	bndary();
/*
 * Advance the frame.
 */
	c_frame();
/*
 * Example 1-5 ---------------------------------------------------------
 *
 * Make CONPACK set up the contour levels again (the range of the data
 * may be increased by 3D interpolation), but force it to use the same
 * contour interval and label interval that were used for the first four
 * plots.
 */
	c_cpseti ("CLS - CONTOUR LEVEL SELECTION FLAG",16);
	c_cpgetr ("CIU - CONTOUR INTERVAL USED",&cinu);
	c_cpsetr ("CIS - CONTOUR INTERVAL SPECifIER",cinu);
	c_cpgeti ("LIU - LABEL INTERVAL USED",&linu);
	c_cpseti ("LIS - LABEL INTERVAL SPECifIER",linu);
/*
 * Provide more room for storing coordinates used to trace contour
 * lines.  The default is slightly too small to hold a complete line,
 * and this causes some lines to have a couple of labels right next to
 * one another.
 */
	c_cpseti ("RWC - REAL WORKSPACE FOR CONTOUR TRACING",200);
/*
 * Turn off the 2D smoother.
 */
	c_cpsetr ("T2D - TENSION ON THE 2D SPLINES",0.);
/*
 * Initialize the drawing of the contour plot.
 */
	c_cpsprs ((float *)zdat,23,23,14,rwrk,5000,iwrk,1000,zdns,5152);
/*
 * Force the selection of contour levels and tweak associated parameters.
 */
	c_cppkcl (zdns,rwrk,iwrk);

	c_cpgeti ("NCL - NUMBER OF CONTOUR LEVELS",&nclv);

	for( iclv=1; iclv <= nclv; iclv++ ) { 
		c_cpseti ("PAI - PARAMETER ARRAY INDEX",iclv);
		c_cpgeti ("CLU - CONTOUR LEVEL USE FLAG",&iclu);
          if (iclu == 3) {
			  c_cpseti ("CLL - CONTOUR LINE LINE WIDTH",2);
		  }
		c_cpgetr ("CLV - CONTOUR LEVEL VALUE",&clev);
		if( clev != 0.) {
            c_cpseti ("AIA - AREA IDENTifIER ABOVE LINE",0);
            c_cpseti ("AIB - AREA IDENTifIER BELOW LINE",0);
		}
		else {
            c_cpseti ("AIA - AREA IDENTifIER ABOVE LINE",2);
            c_cpseti ("AIB - AREA IDENTifIER BELOW LINE",1);
		}
	}
/*
 * The rest is pretty much the same as for example 1-4, but the array
 * ZDNS is used in place of ZDAT.
 */
	gset_linewidth (2.);
	c_perim (0,0,0,0);
	gset_linewidth (1.);
	c_arinam (iama,20000);
	c_cplbam (zdns,rwrk,iwrk,iama);
	c_cpcldm (zdns,rwrk,iwrk,iama,drawcl);
	c_cplbdr (zdns,rwrk,iwrk);
	c_cpclam (zdns,rwrk,iwrk,iama);
	c_arscam (iama,xcra,ycra,1000,iara,igra,10,shader);
/*
 * Label plot and put a boundary line around the edge of the plotter frame.
 */
	labtop ("EXAMPLE 1-5",.017);
	bndary();
/*
 * Advance the frame.
 */
	c_frame();
/*
 * Example 1-6 ---------------------------------------------------------
 *
 * Turn off the selection of contour levels, so that the set picked for
 * example 1-5 will be used.
 */
	c_cpseti ("CLS - CONTOUR LEVEL SELECTION FLAG",0);
/*
 * Draw an EZMAP background.  The perimeter and the grid are turned off.
 * The "political + U.S. states" dataset is used and it is dotted.  We
 * use a satellite-view projection, centered over the U.S., showing
 * maximal area.
 */
	c_mapsti ("PE - PERIMETER",0);
	c_mapsti ("GR - GRID",0);
	c_mapstc ("OU - OUTLINE DATASET","PS");
	c_mapsti ("DO - DOTTING OF OUTLINES",1);
	c_mapstr ("SA - SATELLITE HEIGHT",1.13);
	c_maproj ("SV - SATELLITE-VIEW",40.,-95.,0.);
    p1[0] = p2[0] = p3[0] = p4[0] = 0.;
    p1[1] = p2[1] = p3[1] = p4[1] = 0.;
    c_mapset ("MA - MAXIMAL AREA",p1,p2,p3,p4);
	c_mapdrw();
/*
 * Tell CONPACK that the SET call has been done, force it to generate X
 * coordinates that are longitudes and Y coordinates that are latitudes,
 * turn on mapping to an EZMAP background, define the out-of-range value
 * (returned by MAPTRN for an unprojectable point), and put the
 * informational label in a different place.
 */
	c_cpseti ("SET - DO SET-CALL FLAG",0);
	c_cpsetr ("XC1 - X COORDINATE AT I = 1",-130.);
	c_cpsetr ("XCM - X COORDINATE AT I = M",-60.);
	c_cpsetr ("YC1 - Y COORDINATE AT J = 1",10.);
	c_cpsetr ("YCN - Y COORDINATE AT J = N",70.);
	c_cpseti ("MAP - MAPPING FLAG",1);
	c_cpsetr ("ORV - OUT-OF-RANGE VALUE",1.e12);
	c_cpseti ("ILP - INFORMATIONAL LABEL POSITIONING",3);
	c_cpsetr ("ILX - INFORMATIONAL LABEL X POSITION",.5);
	c_cpsetr ("ILY - INFORMATIONAL LABEL Y POSITION",-.02);
/*
 * The rest of the calls are just as in example 1-5, except that the
 * perimeter is not drawn.
 */
	c_cpsprs ((float *)zdat,23,23,14,rwrk,5000,iwrk,1000,zdns,5152);
	c_arinam (iama,20000);
	c_cplbam (zdns,rwrk,iwrk,iama);
	c_cpcldm (zdns,rwrk,iwrk,iama,drawcl);
	c_cplbdr (zdns,rwrk,iwrk);
	c_cpclam (zdns,rwrk,iwrk,iama);
	c_arscam (iama,xcra,ycra,1000,iara,igra,10,shader);
/*
 * Label it and put a boundary line around the edge of the plotter frame.
 */
	labtop ("EXAMPLE 1-6",.017);
	bndary();
/*
 * Advance the frame.
 */
	c_frame();
/*
 * ---------------------------------------------------------------------
 *
 * Close GKS.
 */
	c_clsgks();
/*
 * Done.
 */
}

int drawcl(
    float *xcs,
    float *ycs,
    int *ncs,
    int *iai,
    int *iag,
    int *nai
)
{
/*
 * This version of DRAWCL draws the polyline defined by the points
 * ((XCS(I),YCS(I)),I=1,NCS) if and only if none of the area identifiers
 * for the area containing the polyline are negative.  The dash package
 * routine CURVED is called to do the drawing.
 *
 * Turn on drawing.
 */
	int idr, i;

	idr=1;
/*
 * if any area identifier is negative, turn off drawing.
 */
	for( i=0; i < *nai; i++ ) {
		if (iai[i] < 0 ) idr=0;
	}
/*
 * if drawing is turned on, draw the polyline.
 */
	if (idr != 0) c_curved (xcs,ycs,*ncs);
/*
 * Done.
 */
	return(0);
}

int shader(
    float *xcs,
    float *ycs,
    int *ncs,
    int *iai,
    int *iag,
    int *nai
)
{
/*
 * This version of SHADER shades the polygon whose edge is defined by
 * the points ((XCS(I),YCS(I)),I=1,NCS) if and only, relative to edge
 * group 3, its area identifier is a 1.  The package SOFTFILL is used
 * to do the shading.
 *
 * Define workspaces for the shading routine.
 */
	float dst[1100];
	int i, ish, ind[1200];
/*
 * Turn off shading.
 */
	ish=0;
/*
 * if the area identifier for group 3 is a 1, turn on shading.
 */
    for( i = 0; i < *nai; i++ ) {
        if (iag[i] ==3 && iai[i] ==1) ish=1;
    }
/*
 * if shading is turned on, shade the area.  The last point of the
 * edge is redundant and may be omitted.
 */
    if (ish != 0) {
        c_sfseti ("ANGLE",45);
        c_sfsetr ("SPACING",.006);
        c_sfwrld (xcs,ycs,*ncs-1,dst,1100,ind,1200);
        c_sfseti ("ANGLE",135);
        c_sfnorm (xcs,ycs,*ncs-1,dst,1100,ind,1200);
    }
/*
 * Done.
 */
	return(0);
}
