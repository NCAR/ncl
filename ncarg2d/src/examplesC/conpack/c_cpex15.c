/*
 * $Id: c_cpex15.c,v 1.2 1996-10-10 21:50:28 haley Exp $
 */

#include <stdio.h>
#include <ncarg/ncargC.h>
#include <ncarg/gks.h>
#include <math.h>

/*
 * Define the error file, the Fortran unit number, the workstation type,
 * and the workstation ID.
 */
#define lunit "gmeta"
#define iwtype SED_WSTYPE
#define iwkid 1

#define min(x,y)  ((x) < (y) ? (x) : (y))
#define max(x,y)  ((x) > (y) ? (x) : (y))

/*
 * This program demonstrates how user-supplied versions of the "user
 * callback" routines CPCHHL and CPCHLL may be used to change various
 * aspects of individual high/low labels and contour-line labels on a
 * contour plot.  In particular, it shows how to prevent such labels
 * from appearing in a portion of the plotter frame where contour lines
 * are suppressed (by masking against the contents of an area map).
 *
 * Declare an array to hold the data to be contoured.
 */
/*
 * Declare an array to hold an area map.  Put in a common block so we
 * can get at it from the routines CPCHHL and CPCHLL.
 */
struct common {
    int iama[200000];
} NGCALLF(usrama,USRAMA);

main()
{
    float zdat[70][70];
/*
 * Declare the required real and integer workspaces.
 */
    float rwrk[5000];
    int iwrk[1000];
/*
 *  Color table
 */ 
    Gcolr_rep ctab[5];

    float p1[2],p2[2],p3[2],p4[2];
    int i, iclu, iclv, nclv;
/*
 * Declare the arrays needed by ARSCAM and MAPGRM for x/y coordinates.
 */
      float xcra[10000],ycra[10000];
/*
 * Declare the arrays needed by ARSCAM and MAPGRM for area and group
 * identifiers.
 */
    int iara[10],igra[10];
/*
 * Declare the routine that draws contour lines, avoiding labels.
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
 * Declare the routine that does the shading of a contour band.
 */
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
 * Declare the routine that fills the EZMAP background.
 */
    extern int filleb(
#ifdef NeedFuncProto
        float *xcs,
        float *ycs,
        int *ncs,
        int *iai,
        int *iag,
        int *nai
#endif
    );
    extern void gendat();
/*
 * Open GKS.
 */
    gopen_gks ("stdout",0);
    gopen_ws (iwkid, lunit, iwtype);
    gactivate_ws(iwkid);
/*
 * Turn off the clipping indicator.
 */
    gset_clip_ind (GIND_NO_CLIP);

/*
 * Define colors to use for various purposes.
 */
/*
 * light blue (for labels)
 */
    ctab[0].rgb.red = .5; ctab[0].rgb.green = .5; ctab[0].rgb.blue = 1.;
/*
 *  light yellow (for labels)
 */
    ctab[1].rgb.red = 1.; ctab[1].rgb.green = 1.; ctab[1].rgb.blue = .5;
/*
 *  light red (for labels)
 */
    ctab[2].rgb.red = 1.; ctab[2].rgb.green = .5; ctab[2].rgb.blue = .5;
/*
 *  white (for land areas)
 */
    ctab[3].rgb.red = 1.; ctab[3].rgb.green = 1.; ctab[3].rgb.blue = 1.;
/*
 *  gray (for ocean areas)
 */
    ctab[4].rgb.red = .6; ctab[4].rgb.green = .6; ctab[4].rgb.blue = .6;

    for( i = 0; i < 5; i++ ) {
        gset_colr_rep(iwkid,i+2,&ctab[i]);
    }
/*
 * Generate an array of test data.
 */
    gendat (&zdat[0][0],70,70,70,21,21,-1.,1.);
/*
 * Put some labels at the top of the plot.
 */
    c_plchhq (.5,.982,"CONPACK EXAMPLE 15",.018,0.,0.);

    c_plchhq (.5,.952,"The routines CPCHHL and CPCHLL are used be low to suppress labels over land.",.012,0.,0.);
    
    c_plchhq (.5,.928,"They are also used to modify colors and line widths used for the labels.",.012,0.,0.);
/*
 * Initialize EZMAP.
 */
/*
 *  no labels
 */
    c_mapsti ("LA - LABELS",0);
/*
 *  no perimeter
 */
    c_mapsti ("PE - PERIMETER",0);
/*
 *  positions the map
 */
    c_mappos (.05,.95,.01,.91);
/*
 *  projection
 */
    c_maproj ("OR - ORTHOGRAPHIC",40.,-135.,0.);
/*
 *  map portion
 */
    p1[0] = p2[0] = p3[0] = p4[0] = 0.;
    p1[1] = p2[1] = p3[1] = p4[1] = 0.;
    c_mapset ("MA - MAXIMAL AREA",p1,p2,p3,p4);
/*
 * initialize
 */
    c_mapint();
/*
 * Tell CONPACK to do no SET call (EZMAP has done it).
 */
    c_cpseti ("SET - DO-SET-CALL FLAG",0);
/*
 * Tell CONPACK to use more contour levels.
 */
    c_cpseti ("CLS - CONTOUR LEVEL SELECTOR",32);
/*
 * Tell CONPACK to position labels using the "regular" scheme.
 */
    c_cpseti ("LLP - LINE LABEL POSITIONING",2);
/*
 * Tweak constants so as to get more labels on each labeled contour.
 */
    c_cpsetr ("RC1 - REGULAR SCHEME CONSTANT 1",.05);
    c_cpsetr ("RC2 - REGULAR SCHEME CONSTANT 2",.1);
    c_cpsetr ("RC3 - REGULAR SCHEME CONSTANT 3",0.);
/*
 * Provide a little more workspace for X and Y coordinates defining
 * contour lines, so as not to have labels right next to each other
 * on a contour line.
 */
    c_cpseti ("RWC - REAL WORKSPACE FOR CONTOURS",200);
/*
 * Turn on drawing and filling of the high and low label boxes.
 */
    c_cpseti ("HLB - HIGH/LOW LABEL BOX FLAG",3);
/*
 * Tell CONPACK to delete high/low labels which overlap the informational
 * label or another high/low label, but to move those which overlap the
 * edge inward a little.
 */
    c_cpseti ("HLO - HIGH/LOW LABEL OVERLAP FLAG",11);
/*
 * Turn on drawing and filling of the contour line label boxes.
 */
    c_cpseti ("LLB - LINE LABEL BOX FLAG",3);
/*
 * Make all CONPACK-written characters a little smaller.
 */
    c_cpsetr ("CWM - CHARACTER WIDTH MULTIPLIER",.8);
/*
 * Turn on the drawing of the grid edge ("contour line number -1") and
 * thicken it somewhat.
 */
    c_cpseti ("PAI - PARAMETER ARRAY INDEX",-1);
    c_cpseti ("CLU - CONTOUR LEVEL USE FLAG",1);
    c_cpsetr ("CLL - CONTOUR LEVEL LINE WIDTH",2.);
/*
 * Tell CONPACK to use EZMAP for mapping and what the out-of-range
 * signal is.
 */
    c_cpseti ("MAP - MAPPING FUNCTION",1);
    c_cpsetr ("ORV - OUT-OF-RANGE VALUE",1.E12);
/*
 * Tell CONPACK what range of coordinates to use.
 */
    c_cpsetr ("XC1 - LONGITUDE AT I = 1",-230.);
    c_cpsetr ("XCM - LONGITUDE AT I = M",- 40.);
    c_cpsetr ("YC1 - LATITUDE AT J = 1" ,- 35.);
    c_cpsetr ("YCN - LATITUDE AT J = N" ,  75.);
/*
 * Initialize the drawing of the contour plot.
 */
    c_cprect (&zdat[0][0],70,70,70,rwrk,5000,iwrk,1000);
/*
 * Force the selection of contour levels so that associated quantities
 * may be tweaked.
 */
    c_cppkcl (&zdat[0][0],rwrk,iwrk);
/*
 * Increase the line width for labelled levels and turn off the area
 * identifiers for all levels.
 */
    c_cpgeti ("NCL - NUMBER OF CONTOUR LEVELS",&nclv);

    for( iclv = 1; iclv <= nclv; iclv++ ) {
        c_cpseti ("PAI - PARAMETER ARRAY INDEX",iclv);
        c_cpgeti ("CLU - CONTOUR LEVEL USE FLAG",&iclu);
        if (iclu == 3) {
            c_cpseti ("CLL - CONTOUR-LINE LINE WIDTH",2);
        }
        c_cpseti ("AIA - AREA IDENTIFIER ABOVE LEVEL",0);
        c_cpseti ("AIB - AREA IDENTIFIER BELOW LEVEL",0);
    }
/*
 * Add two new levels for which no contour lines are to be drawn, but
 * between which shading is to be done.
 */
    nclv += 2;
    c_cpseti ("NCL - NUMBER OF CONTOUR LEVELS",nclv);

    c_cpseti ("PAI - PARAMETER ARRAY INDEX",nclv-1);
    c_cpsetr ("CLV - CONTOUR LEVEL VALUE",-.15);
    c_cpseti ("CLU - CONTOUR LEVEL USE FLAG",0);
    c_cpseti ("AIA - AREA IDENTIFIER ABOVE LEVEL",1);
    c_cpseti ("AIB - AREA IDENTIFIER BELOW LEVEL",2);

    c_cpseti ("PAI - PARAMETER ARRAY INDEX",nclv);
    c_cpsetr ("CLV - CONTOUR LEVEL VALUE",+.15);
    c_cpseti ("CLU - CONTOUR LEVEL USE FLAG",0);
    c_cpseti ("AIA - AREA IDENTIFIER ABOVE LEVEL",3);
    c_cpseti ("AIB - AREA IDENTIFIER BELOW LEVEL",1);
/*
 * Initialize the area map.
 */
    c_arinam (NGCALLF(usrama,USRAMA).iama,200000);
/*
 * Put EZMAP boundary lines into the area map (in edge group 1).
 */
    c_mapbla (NGCALLF(usrama,USRAMA).iama);
/*
 * Put label boxes into the area map (in edge group 3).  One of the first
 * things this routine does is generate a list of labels (high/low and
 * contour line labels).  For each such label, one of the routines CPCHHL
 * or CPCHLL is called, giving the user an opportunity to suppress the
 * positioning of a label there.  The versions of these routines supplied
 * later in this file use the contents of the area map array iama to
 * suppress labels that are over land areas.
 */
    c_cplbam (&zdat[0][0],rwrk,iwrk,NGCALLF(usrama,USRAMA).iama);
/*
 * Fill land and ocean areas in different shades, avoiding label boxes.
 */
    c_arscam (NGCALLF(usrama,USRAMA).iama,xcra,ycra,10000,iara,igra,10,filleb);
/*
 * Set the polyline color index to zero, so that lines drawn from this
 * point on will be drawn in black over the filled background.
 */
    gset_line_colr_ind (0);
/*
 * Draw the EZMAP grid lines (lines of constant latitude and longitude)
 * over the oceans.
 */
    c_mapgrm (NGCALLF(usrama,USRAMA).iama,xcra,ycra,10000,iara,igra,10,drawcl);
/*
 * Put the contour lines at contour levels -.15 and +.15 into the area
 * map.
 */
    c_cpclam (&zdat[0][0],rwrk,iwrk,NGCALLF(usrama,USRAMA).iama);
/*
 * Cross-hatch the area between contour levels -.15 and +.15.
 */
    c_arscam (NGCALLF(usrama,USRAMA).iama,xcra,ycra,10000,iara,igra,10,shader);
/*
 * Draw contour lines over the oceans.
 */
    c_cpcldm (&zdat[0][0],rwrk,iwrk,NGCALLF(usrama,USRAMA).iama,drawcl);
/*
 * Draw labels.  Because the versions of the routines CPCHHL and CPCHLL
 * supplied later in this file are used instead of the default ones in
 * CONPACK, the appearance of the labels is changed in various ways.
 * See the commenting in those routines for further information.
 */
    c_cplbdr (&zdat[0][0],rwrk,iwrk);
/*
 * Advance the frame.
 */
    c_frame();
/*
 * Close GKS.
 */
    gdeactivate_ws(iwkid);
    gclose_ws(iwkid);
    gclose_gks();
}


#ifdef NeedFuncProto
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
/*
 * This subroutine is used to draw EZMAP grid lines and contour lines
 * over the ocean only.  It draws the polyline defined by the points
 * ((XCS(I),YCS(I)),I=1,NCS) if and only if none of the area identifiers
 * for the area containing the polyline are negative and it's over ocean.
 *
 * The dash package routine CURVED is called to do the drawing so that
 * the grid lines will be properly dashed as specified by internal
 * parameters of EZMAP.
 *
 *
 * Find the area identifiers of the polyline relative to group 1 (EZMAP
 * background) and group 3 (CONPACK-supplied edges).
 */
{
    int ia1, ia3, i;

    ia1 = -1;
    ia3 = -1;

    for( i = 0; i < *nai; i++ ) {
        if (iag[i] == 1) ia1 = iai[i];
        if (iag[i] == 3) ia3 = iai[i];
    }
/*
 * Draw the polyline if and only if neither area identifier is negative
 * and it's over the ocean.
 */
    if (ia1 >= 0 && ia3 >= 0 && c_mapaci(ia1) == 1) c_curved (xcs,ycs,*ncs);
/*
 * Done.
 */
    return(0);

}

#ifdef NeedFuncProto
int filleb(
    float *xcs,
    float *ycs,
    int *ncs,
    int *iai,
    int *iag,
    int *nai
)
#else
int filleb (xcs,ycs,ncs,iai,iag,nai)
    float *xcs;
    float *ycs;
    int *ncs;
    int *iai;
    int *iag;
    int *nai;
#endif
{
/*
 * This version of FILLEB fills the polygon whose edge is defined by
 * the points ((XCS(I),YCS(I)),I=1,NCS-1) if it's a land area (in which
 * case color index 6 is used) or an ocean area (in which case color
 * index 7 is used), but it avoids filling areas for which, relative
 * to edge group 3, the area identifier is negative (CONPACK label
 * boxes).  The GKS routine GFA is used to do the fills.
 *
 * Find the area identifiers of the polygon relative to group 1 (EZMAP
 * background) and group 3 (CONPACK-supplied edges).
 */
    Gpoint_list area;
    int ia1, ia3, i, dofill = 0;

    ia1 = -1;
    ia3 = -1;

    for( i = 0; i < *nai; i++ ) {
        if (iag[i] == 1) ia1 = iai[i];
        if (iag[i] == 3) ia3 = iai[i];
    }
/*
 * Fill land areas in white, using GFA and color index 6.
 */
    if (ia1 > 0 && c_mapaci(ia1) == 2 && ia3 >= 0) {
        gset_fill_colr_ind (5);
        dofill = 1;
    }
/*
 * Fill ocean areas in gray, using GFA and color index 7.
 */
    if (ia1 > 0 && c_mapaci(ia1) == 1 && ia3 >= 0) {
        gset_fill_colr_ind (6);
        dofill = 1;
    }
/*
 * Create structure to pass to gfill_area
 */
    if( dofill ) {
        area.num_points = *ncs-1;
        area.points = (Gpoint *) malloc(area.num_points*sizeof(Gpoint));
        if( !area.points ) {
            fprintf( stderr, "filleb: Not enough memory to create fill area structure\n" );
            gemergency_close_gks();
            exit(1);
        }
        for( i = 0; i < *ncs-1; i++ ) {
            area.points[i].x = xcs[i];
            area.points[i].y = ycs[i];
        }
        gfill_area (&area);
        free(area.points);
    }
/*
 * done.
 */
    return(0);
}

#ifdef NeedFuncProto
int shader(
    float *xcs,
    float *ycs,
    int *ncs,
    int *iai,
    int *iag,
    int *nai
)
#else
int shader (xcs,ycs,ncs,iai,iag,nai)
    float *xcs;
    float *ycs;
    int *ncs;
    int *iai;
    int *iag;
    int *nai;
#endif
{
/*
 * This version of SHADER shades the polygon whose edge is defined by
 * the points ((XCS(I),YCS(I)),I=1,NCS-1) if and only if it's over ocean
 * and, relative to edge group 3, its area identifier is a 1 (the area
 * between contours at levels -.15 and .15).  The package SOFTFILL is
 * used to do the shading.
 *
 * Define workspaces for the shading routine.
 */
    float dst[2200];
    int ind[2400];
/*
 * Find the area identifiers of the polygon relative to group 1 (EZMAP
 * background) and group 3 (CONPACK-supplied edges).
 */
    int ia1, ia3, i, dofill = 0;

    ia1 = -1;
    ia3 = -1;

    for( i = 0; i < *nai; i++ ) {
        if (iag[i] == 1) ia1 = iai[i];
        if (iag[i] == 3) ia3 = iai[i];
    }
/*
 * If appropriate, crosshatch the area.
 */
    if (ia1 > 0 && c_mapaci(ia1) == 1 && ia3 == 1) {
        c_sfseti ("angle",45);
        c_sfsetr ("spacing",.003);
        c_sfwrld (xcs,ycs,*ncs-1,dst,2200,ind,2400);
        c_sfseti ("angle",135);
        c_sfnorm (xcs,ycs,*ncs-1,dst,2200,ind,2400);
    }
/*
 * Done.
 */
    return(0);
}


void gendat (data,idim,m,n,mlow,mhgh,dlow,dhgh)
float *data;
int idim;
int m; 
int n;
int mlow;
int mhgh;
float dlow;
float dhgh;
{
/*
** This is a routine to generate test data for two-dimensional graphics
** routines.  Given an array "DATA", dimensioned "IDIM x 1", it fills
** the sub-array ((DATA(I,J),I=1,M),J=1,N) with a two-dimensional field
** of data having approximately "MLOW" lows and "MHGH" highs, a minimum
** value of exactly "DLOW" and a maximum value of exactly "DHGH".
**
** "MLOW" and "MHGH" are each forced to be greater than or equal to 1
** and less than or equal to 25.
**
** The function used is a sum of exponentials.
*/
    float ccnt[3][50], fovm, fovn, dmin, dmax, temp;
    extern float fran();
    int nlow, nhgh, ncnt, i, j, k, ii;
    float fm, fn;

    fovm=9./(float)m;
    fovn=9./(float)n;

    nlow=max(1,min(25,mlow));
    nhgh=max(1,min(25,mhgh));
    ncnt=nlow+nhgh;

    for( k=1; k <= ncnt; k++ ) {
        ccnt[0][k-1]=1.+((float)m-1.)*fran();
        ccnt[1][k-1]=1.+((float)n-1.)*fran();
        if (k <= nlow) {
            ccnt[2][k-1]= -1.;
        }
        else {
            ccnt[2][k-1] = 1.;
        }
    }

    dmin =  1.e36;
    dmax = -1.e36;
    ii = 0;
    for( j = 1; j <= n; j++ ) {
        for( i = 1; i <= m; i++ ) {
            data[ii]=.5*(dlow+dhgh);
            for( k = 1; k <= ncnt; k++ ) {
        fm = fovm*((float)(i)-ccnt[0][k-1]);
        fn = fovn*((float)(j)-ccnt[1][k-1]);
                temp = -( (fm*fm) + (fn*fn) );
                if (temp >= -20.){ 
                     data[ii]=data[ii]+.5*(dhgh-dlow)*ccnt[2][k-1]*exp(temp);
        }
            }
            dmin=min(dmin,data[ii]);
            dmax=max(dmax,data[ii]);
            ii++;
        }
    }

    for( j = 0; j < m*n; j++ ) {
        data[j]=(data[j]-dmin)/(dmax-dmin)*(dhgh-dlow)+dlow;
    }
}

float rseq[] = { .749, .973, .666, .804, .081, .483, .919, .903, .951, .960,
   .039, .269, .270, .756, .222, .478, .621, .063, .550, .798, .027, .569,
   .149, .697, .451, .738, .508, .041, .266, .249, .019, .191, .266, .625,
   .492, .940, .508, .406, .972, .311, .757, .378, .299, .536, .619, .844,
   .342, .295, .447, .499, .688, .193, .225, .520, .954, .749, .997, .693,
   .217, .273, .961, .948, .902, .104, .495, .257, .524, .100, .492, .347,
   .981, .019, .225, .806, .678, .710, .235, .600, .994, .758, .682, .373,
   .009, .469, .203, .730, .588, .603, .213, .495, .884, .032, .185, .127,
   .010, .180, .689, .354, .372, .429 };

float fran()
{
    static int iseq = 0;
    iseq = (iseq % 100) + 1;
    return(rseq[iseq-1]);
}

void NGCALLF(cpchhl,CPCHHL)(int *iflg)
{
    float flbx, flby;
    float rlbx, rlby;
    int i, j, nids, iaid;
/*
 * This routine is called just before and just after each action
 * involving a high/low label.  A user version may take action to change
 * the label.  This version also looks to see if the label is in an
 * allowed position and, if not, blanks it out.
 *
 * *iflg is positive if an action is about to be taken, negative if the
 * action has just been completed.  The action in question is implied
 * by the absolute value of *iflg, as follows:
 *
 *   1 - deciding whether to put a high label at a given point
 *   2 - filling the box around the label for a high
 *   3 - drawing the label for a high
 *   4 - outlining the box around the label for a high
 *   5 - deciding whether to put a low label at a given point
 *   6 - filling the box around the label for a low
 *   7 - drawing the label for a low
 *   8 - outlining the box around the label for a low
 *
 * CPCHHL may retrieve the value of the internal parameter 'ZDV', which
 * is the value associated with the high or low being labelled.
 *
 * CPCHHL may retrieve the values of the internal parameters 'LBX' and
 * 'LBY', which are the coordinates of the center point of the label,
 * in the current user coordinate system.
 *
 * When *iflg is 1, 3, 5, or 7, CPCHHL is permitted to change the value
 * of the internal parameter 'CTM' (a character string); if *iflg is 1 or
 * 5 and 'CTM' is made blank, the label is suppressed; otherwise, the
 * new value of 'CTM' will replace whatever CONPACK was about to use.
 * If this is done for either *iflg = 1 or *iflg = 3, it must be done for
 * both, and the same replacement label must be supplied in both cases.
 * Similarly, if it is done for either *iflg = 5 or *iflg = 7, it must be
 * done for both, and the same replacement label must be specified in
 * both cases.
 *
 * When *iflg = 2, 3, 4, 6, 7, or 8, CPCHHL may make GKS calls to change
 * color or line width; during the following call with *iflg = -2, -3,
 * -4, -6, -7, or -8, such changes should be undone.
 *
 */
    int iaai[10],iagi[10];
/*
 * Define quantities that will be used to generate the coordinates of
 * five points to be tested in making the decision whether a label is
 * over land or water.
 */
    float xstp[5],ystp[5];

    xstp[0] = 0.; xstp[1] = -.01; xstp[2] = .01; xstp[3] = 0.; xstp[4] = 0.; 
    ystp[0] = 0.; ystp[1] =  0.; ystp[2] = 0.; ystp[3] = -.01; ystp[4] = .01;
/*
 * If *iflg = 1, we have to decide whether we want a label at the point
 * ('LBX','LBY') or not, and, if not, reset the value of 'CTM' to a
 * single blank to signal that fact to the calling routine.  The decision
 * is made by looking at an area map previously created in the array
 * iama to see if the label point is over land or water.  We actually
 * test the point itself and four other points around it; it any of
 * the five is over land, we suppress the label (by setting 'CTM'=' ').
 */
    if (*iflg == 1 || *iflg == 5) {
        c_cpgetr ("lbx",&rlbx);
        c_cpgetr ("lby",&rlby);
        flbx = c_cufx(rlbx);
        flby = c_cufy(rlby);
        for( i=0; i < 5; i++ ) {
            c_argtai (NGCALLF(usrama,USRAMA).iama,c_cfux(flbx+xstp[i]),c_cfuy(flby+ystp[i]),iaai,iagi,10,&nids,1);
            iaid = -1;
            for( j = 0; j < nids; j++ ) {
                if (iagi[j] == 1) iaid = iaai[j];
            }
            if (c_mapaci(iaid) == 2) {
                c_cpsetc ("ctm - character temporary"," ");
                return;
            }
        }
    }
/*
 * now, if the label box is being filled, make the fill color depend
 * on whether the label is for a high or a low.
 */
    if (abs(*iflg) == 2 || abs(*iflg) == 6) {
        if (*iflg > 0) {
            if (*iflg == 2) {
                gset_fill_colr_ind (4);
            }
            else {
                gset_fill_colr_ind (2);
            }
        }
        else {
            gset_fill_colr_ind (1);
        }
        return;
    }
/*
 * put the text on the filled background in a contrasting color.
 */
    if (abs(*iflg) == 3 || abs(*iflg) == 7) {
        if (*iflg > 0) {
            if (*iflg == 3) {
                c_pcseti ("cc", 0);
            }
            else {
                c_pcseti ("cc", 1);
            }
        }
        else {
            c_pcseti ("cc",-1);
        }
        return;
    }
/*
 * If the box is being outlined, do it in a contrasting color and widen
 * the lines.
 */
    if (abs(*iflg) == 4 || abs(*iflg) == 8) {
        if (*iflg > 0) {
            if (*iflg == 4) {
                gset_line_colr_ind (0);
            }
            else {
                gset_line_colr_ind (0);
            }
            gset_linewidth (2.);
        }
        else {
            gset_line_colr_ind (1);
            gset_linewidth (1.);
        }
        return;
    }
/*
 * in all other cases, just return.
 */
    return;
}

void NGCALLF(cpchll,CPCHLL) (int *iflg)
{
    float clev, rlbx, rlby, flbx, flby;
    int i, j, nids, iaid;
/*
 * this routine is called just before and just after each action
 * involving a contour line label.  A user version may take action to
 * change the label.  This version also looks to see if the label is
 * in an allowed position and, if not, blanks it out.
 *
 * *iflg is positive if an action is about to be taken, negative if an
 * action has just been completed.  The action in question is implied
 * by the absolute value of *iflg, as follows:
 *
 *   1 - deciding whether to put a line label at a given point
 *   2 - filling the box around a line label
 *   3 - drawing a line label
 *   4 - outlining the box around a line label
 *
 * When CPCHLL is called, the internal parameter 'PAI' will have been
 * set to the index of the appropriate contour level.  Thus, parameters
 * associated with that level may easily be retrieved by calls to CPGETx.
 *
 * CPCHLL may retrieve the value of the internal parameter 'ZDV', which
 * is the contour level associated with the contour line being labelled.
 *
 * CPCHLL may retrieve the values of the internal parameters 'LBX' and
 * 'LBY', which are the coordinates of the center point of the label,
 * in the current user coordinate system.
 *
 * When *iflg is 1 or 3, CPCHLL is permitted to change the value of the
 * internal parameter 'CTM' (a character string); if *iflg is 1 and 'CTM'
 * is made blank, the label is suppressed; otherwise, the new value of
 * 'CTM' will replace whatever CONPACK was about to use.  If this is
 * done for either *iflg = 1 or *iflg = 3, it must be done for both, and
 * the same replacement label must be supplied in both cases.
 *
 * When *iflg = 2, 3, or 4, CPCHLL may make GKS calls to change color
 * or line width; during the following call with *iflg = -2, -3, or -4,
 * such changes should be undone.
 */
    int iaai[10],iagi[10];
/*
 * Define quantities that will be used to generate the coordinates of
 * five points to be tested in making the decision whether a label is
 * over land or water.
 */
    float xstp[5],ystp[5];

    xstp[0] = 0.; xstp[1] = -.01; xstp[2] = .01; xstp[3] = 0.; xstp[4] = 0.; 
    ystp[0] = 0.; ystp[1] =  0.; ystp[2] = 0.; ystp[3] = -.01; ystp[4] = .01;
/*
 * If *iflg = 1, we have to decide whether we want a label at the point
 * ('LBX','LBY') or not, and, if not, reset the value of 'CTM' to a
 * single blank to signal that fact to the calling routine.  The decision
 * is made by looking at an area map previously created in the array
 * iama to see if the label point is over land or water.  We actually
 * test the point itself and four other points around it; it any of
 * the five is over land, we suppress the label (by setting the value
 * of 'CTM' to ' ').
 */
    if (*iflg == 1) {
        c_cpgetr ("LBX",&rlbx);
        c_cpgetr ("LBY",&rlby);
        flbx = c_cufx(rlbx);
        flby = c_cufy(rlby);
        for( i = 0; i < 5; i++ ) {
            c_argtai(NGCALLF(usrama,USRAMA).iama,c_cfux(flbx+xstp[i]),c_cfuy(flby+ystp[i]),iaai,iagi,10,&nids,1);
            iaid = -1;
            for( j = 0; j < nids; j++ ) {
                if (iagi[j] == 1) iaid = iaai[j];
            }
            if (c_mapaci(iaid) == 2) {
                c_cpsetc ("ctm - character temporary"," ");
                return;
            }
        }
    }
/*
 * otherwise, see what the contour value is on the line being labelled
 * and, if it's zero, reset the value of 'CTM' to 'Z' so that will be
 * used as the label.
 */
    c_cpgetr ("ZDV - Z DATA VALUE",&clev);
    if (clev == 0.) {
        c_cpsetc ("CTM - CHARACTER TEMPORARY","Z");
    }
/*
 * Now, if the label box is being filled, make the fill color depend
 * on the contour level.
 */
    if (abs(*iflg) == 2) {
        if (*iflg > 0) {
            if (clev < 0.) {
                gset_fill_colr_ind (2);
            }
            else if (clev == 0.) {
                gset_fill_colr_ind (3);
            }
            else {
                gset_fill_colr_ind (4);
            }
        }
        else {
            gset_fill_colr_ind (1);
        }
        return;
    }
/*
 * put the text on the filled background in a contrasting color.
 */
    if (abs(*iflg) == 3) {
        if (*iflg > 0) {
            if (clev < 0.) {
                c_pcseti ("cc", 1);
            }
            else if (clev == 0.) {
                c_pcseti ("cc", 0);
            }
            else {
                c_pcseti ("cc", 0);
            }
        }
        else {
            c_pcseti ("cc",-1);
        }
        return;
    }
/*
 * if the box is being outlined, do it in a contrasting color and widen
 * the lines.
 */
    if (abs(*iflg) == 4) {
        if (*iflg > 0) {
            if (clev < 0.) {
                gset_line_colr_ind (0);
            }
            else if (clev == 0.) {
                gset_line_colr_ind (0);
            }
            else {
                gset_line_colr_ind (0);
            }
            gset_linewidth (2.);
        }
        else {
            gset_line_colr_ind (1);
            gset_linewidth (1.);
        }
        return;
    }
/*
 * in all other cases, just return.
 */
    return;
}
 
