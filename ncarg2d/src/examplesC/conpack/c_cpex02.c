/*
 *	$Id: c_cpex02.c,v 1.1 1994-05-13 14:25:47 haley Exp $
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
    float zdat[33][33],rwrk[5000],xcra[1000],ycra[1000];
    float time;
/*
 * declare the required real and integer workspaces.
 */
    int iwrk[1000], iama[20000], iara[10], igra[10];
    int i, iplt, nclv, iclv, iclu;
    Gasfs iasf;
/*
 * declare the routine which will draw contour lines, avoiding labels.
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
    extern int shader(
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
 * dimension a character variable to hold plot labels.
 */
    char labl[12];
/*
 * initialize the values in the aspect-source-flag structure
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
 * open gks.
 */
    c_opngks();
/*
 * turn off the clipping indicator.
 */
    gset_clip_ind (GIND_NO_CLIP);
/*
 * set all the gks aspect source flags to "individual".
 */
    gset_asfs (&iasf);
/*
 * turn on the drawing of the grid edge ("contour line number -1") and
 * thicken it somewhat.
 */
    c_cpseti("PAI - PARAMETER ARRAY INDEX",-1);
    c_cpseti("CLU - CONTOUR LEVEL USE FLAG",1);
    c_cpsetr("CLL - CONTOUR LEVEL LINE WIDTH",2.);
/*
 * turn on the positioning of labels by the penalty scheme and provide a
 * little more room for x and y coordinates defining contour lines, so
 * as not to have labels right next to each other on a contour line.
 */
    c_cpseti("LLP - LINE LABEL POSITIONING",3);
    c_cpseti("RWC - REAL WORKSPACE FOR CONTOURS",200);
/*
 * turn on the drawing of the high and low label boxes.
 */
    c_cpseti("HLB - HIGH/LOW LABEL BOX FLAG",1);
/*
 * tell conpack to delete high/low labels which overlap the informational
 * label or another high/low label, but to move those which overlap the
 * edge inward a little.
 */
    c_cpseti("HLO - HIGH/LOW LABEL OVERLAP FLAG",11);
/*
 * make all conpack-written characters a little bigger.
 */
    c_cpsetr("CWM - CHARACTER WIDTH MULTIPLIER",1.25);
/*
 * move the informational label into the lower left-hand corner and
 * turn on the box around it, making the box thicker than normal.
 */
    c_cpsetr("ILX - INFORMATIONAL LABEL X POSITION",.02);
    c_cpsetr("ILY - INFORMATIONAL LABEL Y POSITION",.02);
    c_cpseti("ILP - INFORMATIONAL LABEL POSIIONING",-4);
    c_cpseti("ILB - INFORMATIONAL LABEL BOX",1);
    c_cpsetr("ILL - INFORMATIONAL LABEL LINE WIDTH",2.);
/*
 * change the text of the informational label.
 */
    c_cpsetc("ILT - INFORMATIONAL LABEL TEXT","CONTOUR FROM $CMN$ TO $CMX$ BY $CIU$ (X $SFU$)");
/*
 * do four different plots, one in each quadrant.
 */
    for( iplt=1; iplt <= 4; iplt++ ) {
/*
 * generate an array of test data.
 */
        gendat(zdat,33,33,33,20,20,.000025,.000075);
/*
 * get the current elapsed time, in seconds.
 */
        time=0.;
/*
 * move the viewport to the proper quadrant.
 */
        c_cpsetr("VPL - VIEWPORT LEFT EDGE",.0250+.4875*(float)((iplt-1)%2));
        c_cpsetr("VPR - VIEWPORT RIGHT EDGE",.4875+.4875*(float)((iplt-1)%2));
        c_cpsetr("VPB - VIEWPORT BOTTOM EDGE",.0250+.4875*(float)((4-iplt)/2));
        c_cpsetr("VPT - VIEWPORT TOP EDGE",.4875+.4875*(float)((4-iplt)/2));
/*
 * specify how the scale factor is to be selected.
 */
        c_cpseti("SFS - SCALE FACTOR SELECTION",-iplt);
/*
 * initialize the drawing of the contour plot.
 */
        c_cprect(&zdat[0][0],33,33,33,rwrk,5000,iwrk,1000);
/*
 * force the selection of contour levels, so that associated quantities
 * may be tweaked.
 */
        c_cppkcl(&zdat[0][0],rwrk,iwrk);
/*
 * increase the line width for labelled levels and turn off the area
 * identifiers for all levels.
 */
        c_cpgeti("NCL - NUMBER OF CONTOUR LEVELS",&nclv);

        for( iclv =1; iclv < nclv; iclv++ ) {
            c_cpseti("PAI - PARAMETER ARRAY INDEX",iclv);
            c_cpgeti("CLU - CONTOUR LEVEL USE FLAG",&iclu);
            if(iclu == 3) {
                c_cpseti("CLL - CONTOUR-LINE LINE WIDTH",2);
            }
            c_cpseti("AIA - AREA IDENTIFIER ABOVE LEVEL",0);
            c_cpseti("AIB - AREA IDENTIFIER BELOW LEVEL",0);
        }
/*
 * add two new levels for which no contour lines are to be drawn, but
 * between which shading is to be done.
 */
        nclv=nclv+2;
        c_cpseti("NCL - NUMBER OF CONTOUR LEVELS",nclv);

        c_cpseti("PAI - PARAMETER ARRAY INDEX",nclv-1);
        c_cpsetr("CLV - CONTOUR LEVEL VALUE",.000045);
        c_cpseti("CLU - CONTOUR LEVEL USE FLAG",0);
        c_cpseti("AIA - AREA IDENTIFIER ABOVE LEVEL",1);
        c_cpseti("AIB - AREA IDENTIFIER BELOW LEVEL",2);
        c_cpseti("PAI - PARAMETER ARRAY INDEX",nclv);
        c_cpsetr("CLV - CONTOUR LEVEL VALUE",.000055);
        c_cpseti("CLU - CONTOUR LEVEL USE FLAG",0);
        c_cpseti("AIA - AREA IDENTIFIER ABOVE LEVEL",3);
        c_cpseti("AIB - AREA IDENTIFIER BELOW LEVEL",1);
/*
 * draw the contour plot.
 */
        c_arinam (iama,20000);
        c_cplbam(&zdat[0][0],rwrk,iwrk,iama);
        c_cpcldm(&zdat[0][0],rwrk,iwrk,iama,drawcl);
        c_cplbdr(&zdat[0][0],rwrk,iwrk);
        c_cpclam(&zdat[0][0],rwrk,iwrk,iama);
        c_arscam(iama,xcra,ycra,1000,iara,igra,10,shader);
/*
 * compute and print statistics for the plot and label it.
 */
        sprintf( labl, "EXAMPLE 2-%d", iplt );
        capsap (labl,time,iama,20000);
        labtop (labl,.017);

    }
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
    c_clsgks();
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
    int i, idr;
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
    if (idr != 0) c_curved (xcs,ycs,*ncs);
/*
 * done.
 */
    return(1);
}

#ifdef __STDC__
int shader(
    float *xcs,
    float *ycs,
    int *ncs,
    int *iai,
    int *iag,
    int *nai
)
#else 
int shader(xcs,ycs,ncs,iai,iag,nai)
    float *xcs;
    float *ycs;
    int *ncs;
    int *iai;
    int *iag;
    int *nai;
#endif
{
/*
 * this version of shader shades the polygon whose edge is defined by
 * the points ((xcs(i),ycs(i)),i=1,ncs) if and only, relative to edge
 * group 3, its area identifier is a 1.  the package softfill is used
 * to do the shading.
 */
    int i, ish,ind[1200];
/*
 * define workspaces for the shading routine.
 */
    float dst[1100];
/*
 * turn off shading.
 */
    ish=0;
/*
 * if the area identifier for group 3 is a 1, turn on shading.
 */
    for( i = 0; i < *nai; i++ ) {
        if (iag[i] ==3 && iai[i] ==1) ish=1;
    }
/*
 * if shading is turned on, shade the area.  the last point of the
 * edge is redundant and may be omitted.
 */
    if (ish != 0) {
        c_sfseti ("ANGLE",45);
        c_sfsetr ("SPACING",.006);
        c_sfwrld (xcs,ycs,*ncs-1,dst,1100,ind,1200);
        c_sfseti ("ANGLE",135);
        c_sfnorm (xcs,ycs,*ncs-1,dst,1100,ind,1200);
    }
    return(1);
}
