/*
 *	$Id: c_cpex06.c,v 1.3 1994-08-24 16:02:00 haley Exp $
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
 * Declare required data arrays and workspace arrays.
 */
    float zdat[23][27],rwrk[5000];
    float clev;
    int iclu, iclv, i,nclv;
    int iwrk[1000],iama[10000];
    Gasfs iasf;
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
    gset_asfs (&iasf);
/*
 * Define color indices.
 */
    dfclrs();
/*
 * Generate an array of test data.
 */
    gendat (zdat,27,27,23,25,25,-362.362e11,451.834e11);
/*
 * Increase the approximate number of contour levels used.
 */
    c_cpseti ("CLS - CONTOUR LEVEL SELECTION",25);
/*
 * Turn on the positioning of labels by the penalty scheme.
 */
    c_cpseti ("LLP - LINE LABEL POSITIONING",3);
/*
 * Label highs and low with just the number, boxed and colored green.
 */
    c_cpsetc ("HLT - HIGH/LOW TEXT","$ZDV$");
    c_cpseti ("HLB - HIGH/LOW LABEL BOX FLAG",1);
    c_cpseti ("HLC - HIGH/LOW LABEL COLOR INDEX",9);
/*
 * Tell CONPACK to delete high/low labels which overlap the informational
 * label, another high/low label, or the edge.
 */
    c_cpseti ("HLO - HIGH/LOW LABEL OVERLAP FLAG",7);
/*
 * Turn on the drawing of the grid edge ("contour line number -1"),
 * thicken it somewhat, and make it white.
 */
    c_cpseti ("PAI - PARAMETER ARRAY INDEX",-1);
    c_cpseti ("CLU - CONTOUR LEVEL USE FLAG",1);
    c_cpsetr ("CLL - CONTOUR LEVEL LINE WIDTH",2.);
    c_cpseti ("CLC - CONTOUR LINE COLOR",1);
/*
 * Initialize the drawing of the contour plot.
 */
    c_cprect (&zdat[0][0],27,27,23,rwrk,5000,iwrk,1000);
/*
 * Force the selection of contour levels by CONPACK.
 */
    c_cppkcl (&zdat[0][0],rwrk,iwrk);
/*
 * force the color of the negative contours to blue, the color of the
 * positive contours to red, and the color of the zero contour to white.
 * If a positive or negative contour is labelled, use a darker shade and
 * make the color of the label match.
 */
    c_cpgeti ("NCL - NUMBER OF CONTOUR LEVELS",&nclv);

    for( iclv = 1; iclv <= nclv; iclv++ ) {
        c_cpseti ("PAI - PARAMETER ARRAY INDEX",iclv);
        c_cpgetr ("CLV - CONTOUR LEVEL",&clev);
        c_cpgeti ("CLU - CONTOUR LEVEL USE",&iclu);
        if(clev < 0.) {
            if(iclu == 1) {
                c_cpseti ("CLC - CONTOUR LINE COLOR",7);
            }
            else {
                c_cpseti ("CLC - CONTOUR LINE COLOR",6);
                c_cpseti ("LLC - LINE LABEL COLOR",6);
            }
        }
        else if(clev == 0.) {
            c_cpseti ("CLC - CONTOUR LINE COLOR",1);
            c_cpseti ("LLC - LINE LABEL COLOR",1);
        }
        else {
            if(iclu == 1) {
                c_cpseti ("CLC - CONTOUR LINE COLOR",13);
            }
            else {
                c_cpseti ("CLC - CONTOUR LINE COLOR",14);
                c_cpseti ("LLC - LINE LABEL COLOR",14);               
            }
        }
    }
/*
 * Initialize the area map.
 */
    c_arinam (iama,10000);
/*
 * put label boxes in the area map.
 */
    c_cplbam (&zdat[0][0],rwrk,iwrk,iama);
/*
 * Draw contour lines, avoiding drawing through label boxes.
 */
    c_cpcldm (&zdat[0][0],rwrk,iwrk,iama,drawcl);
/*
 * fill in the labels.
 */
    c_cplbdr (&zdat[0][0],rwrk,iwrk);
/*
 * Compute and print statistics for the plot, label it, and put a
 * boundary line at the edge of the plotter frame.
 */
    capsap ("EXAMPLE 6",iama,10000);
    labtop ("EXAMPLE 6",.017);
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
    if (idr) c_curved (xcs,ycs,*ncs);
    return(1);
}
