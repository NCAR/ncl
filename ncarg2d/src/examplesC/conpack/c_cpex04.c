/*
 *	$Id: c_cpex04.c,v 1.2 1994-06-21 14:59:40 haley Exp $
 */
#include <stdio.h>
#include <math.h>

#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define WSTYPE SED_WSTYPE
#define WKID   1

main()
{
/*
 * Declare an array to hold the data to be contoured.
 */
    float zdat[37][53],rwrk[5000],xcra[1000],ycra[1000];
/*
 * Declare the required real and integer workspaces.
 */
    int iwrk[1000], iama[30000],iara[10],igra[10];
    int i;
    float time;
    Gasfs iasf;
/*
 * Declare an array to hold line labels.
 */
    char  labl[10][5];
/*
 * Declare the routine which will draw contour lines, avoiding labels.
 */
    extern int drawcl(
#ifdef NeedFuncProto
        float *xcs,
        float *ycs,
        int *ncs,
        int *iai,
        int *iag,
        int *nai
#endif
    );
    extern int shadam(
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
 * Define a set of line labels.
 */
    strcpy( labl[0], "0-1 " );
    strcpy( labl[1], "1-2 " );
    strcpy( labl[2], "2-3 " );
    strcpy( labl[3], "3-4 " );
    strcpy( labl[4], "4-5 " );
    strcpy( labl[5], "5-6 " );
    strcpy( labl[6], "6-7 " );
    strcpy( labl[7], "7-8 " );
    strcpy( labl[8], "8-9 " );
    strcpy( labl[9], "9-10" );

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
 * set all the gks aspect source flags to "individual".
 */
    gset_asfs (&iasf);
/*
 * generate a relatively smooth array of test data, with values ranging
 * from 0 to 10.
 */
    gendat (zdat,53,53,37,10,10,0.,10.);
/*
 * initialize the software fill package to do the desired type of fill.
 */
    c_sfseti ("TYPE OF FILL",-2);
    c_sfseti ("ANGLE OF FILL LINES",45);
    c_sfsetr ("SPACING OF FILL LINES",.000625);
/*
 * get the current elapsed time, in seconds.
 */
       time=0;
/*
 * turn on the positioning of labels by the penalty scheme and provide a
 * little more room for x and y coordinates defining contour lines, so
 * as not to have labels right next to each other on a contour line.
 */
    c_cpseti ("LLP - LINE LABEL POSITIONING",3);
    c_cpseti ("RWC - REAL WORKSPACE FOR CONTOURS",200);
/*
 * turn off the drawing of high and low labels.
 */
    c_cpsetc ("HLT - HIGH/LOW LABEL TEXT"," ");
/*
 * turn on the drawing of the grid edge ("contour line number -1") and
 * thicken it somewhat.
 */
    c_cpseti ("PAI - PARAMETER ARRAY INDEX",-1);
    c_cpseti ("CLU - CONTOUR LEVEL USE FLAG",1);
    c_cpsetr ("CLL - CONTOUR LEVEL LINE WIDTH",2.);
/*
 * turn off the informational label.
 */
    c_cpsetc ("ILT - INFORMATIONAL LABEL TEXT"," ");
/*
 * turn on line-label boxes.
 */
    c_cpseti ("LLB - LINE LABEL BOXES",1);
/*
 * force the use of contour lines at the values 1, 2, 3, ... 9 and
 * provide for labelling, but not drawing, contour lines at levels
 * .5, 1.5, 2.5, ... 9.5, with labels of the form "0-1", "1-2", ...,
 * "9-10".  arrange for the areas between contour lines drawn to be
 * shaded.
 */
    c_cpseti ("CLS - CONTOUR LEVEL SELECTION",0);
    c_cpseti ("NCL - NUMBER OF CONTOUR LEVELS",19);

    for( i=1; i <= 9; i++ ) {
        c_cpseti ("PAI - PARAMETER ARRAY IDENTIFIER",i);
        c_cpsetr ("CLV - CONTOUR LEVEL VALUE",(float)i);
        c_cpseti ("CLU - CONTOUR LEVEL USE",1);
        c_cpsetr ("CLL - CONTOUR LEVEL LINE WIDTH",2.);
        c_cpseti ("AIA - AREA IDENTIFIER ABOVE LINE",i+1);
        c_cpseti ("AIB - AREA IDENTIFIER BELOW LINE",i);
    }
    for( i=10; i <= 19; i++ ) {
        c_cpseti ("PAI - PARAMETER ARRAY IDENTIFIER",i);
        c_cpsetr ("CLV - CONTOUR LEVEL VALUE",(float)(i-10)+.5);
        c_cpseti ("CLU - CONTOUR LEVEL USE",2);
        c_cpsetc ("LLT - LINE LABEL TEXT",labl[i-10]);
        c_cpseti ("AIA - AREA IDENTIFIER ABOVE LINE",0);
        c_cpseti ("AIB - AREA IDENTIFIER BELOW LINE",0);
    }
/*
 * initialize the drawing of the contour plot.
 */
    c_cprect (&zdat[0][0],53,53,37,rwrk,5000,iwrk,1000);
/*
 * draw the contour plot.
 */
    c_arinam (iama,30000);
    c_cplbam (&zdat[0][0],rwrk,iwrk,iama);
    c_cpcldm (&zdat[0][0],rwrk,iwrk,iama,drawcl);
    c_cplbdr (&zdat[0][0],rwrk,iwrk);
    c_cpclam (&zdat[0][0],rwrk,iwrk,iama);
    c_arscam (iama,xcra,ycra,1000,iara,igra,10,shadam);
/*
 * compute and print statistics for the plot and label it.
 */
    capsap ("EXAMPLE 4",time,iama,30000);
    labtop ("EXAMPLE 4",.017);

/*
 * put a boundary line at the edge of the plotter frame.
 */
    bndary();
/*
 * advance the frame.
 */
    c_frame();
/*
 * close gks.
 */
	gdeactivate_ws(WKID);
	gclose_ws(WKID);
	gclose_gks();
}

#ifdef __STDC__
int drawcl(
    float *xcs,
    float *ycs,
    int *ncs,
    int *iai,
    int *iag,
    int *nai
)
#else
int drawcl (xcs,ycs,ncs,iai,iag,nai)
    float *xcs;
    float *ycs;
    int *ncs;
    int *iai;
    int *iag;
    int *nai;
#endif
{
/*
 * this version of drawcl draws the polyline defined by the points
 * ((xcs(i),ycs(i)),i=1,ncs) if and only if none of the area identifiers
 * for the area containing the polyline are negative.  the dash package
 * routine curved is called to do the drawing.
 */
    int idr, i;
/*
 * turn on drawing.
 */
    idr=1;
/*
 * if any area identifier is negative, turn off drawing.
 */
    for( i = 0; i < *nai; i++ ) {
        if (iai[i] < 0) idr=0;
    }
/*
 * if drawing is turned on, draw the polyline.
 */
    if (idr) c_curved (xcs,ycs,*ncs);
    return(1);
}

#ifdef __STDC__
int shadam(
    float *xcs,
    float *ycs,
    int *ncs,
    int *iai,
    int *iag,
    int *nai
)
#else 
int shadam(xcs,ycs,ncs,iai,iag,nai)
    float *xcs;
    float *ycs;
    int *ncs;
    int *iai;
    int *iag;
    int *nai;
#endif
{
/*
 * this version of shadam shades the polygon whose edge is defined by
 * the points ((xcs(i),ycs(i)),i=1,ncs) if and only, relative to edge
 * group 3, its area identifier is between 1 and 10.  the package
 * softfill is used to do the shading; the density of the shading
 * increases with the value of the area identifier.
 */
/*
 * define workspaces for the shading routine.
 */
    int ish, i,ind[1200];
    float dst[1100];
/*
 * turn off shading.
 */
    ish=0;
/*
 * if the area identifier for group 3 is in the right range, turn on
 * shading.
 */
    for( i = 0; i < *nai; i++ ) {
        if (iag[i] == 3 && iai[i]  >= 1 && iai[i]  <= 10) ish=iai[i];
    }
/*
 * if shading is turned on, shade the area.  the last point of the
 * edge is redundant and may be omitted.
 */
    if (ish) c_sfsgfa (xcs,ycs,*ncs,dst,1100,ind,1200,ish-1);
    return(1);
}
