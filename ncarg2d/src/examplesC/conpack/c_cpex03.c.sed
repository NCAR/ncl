/*
 *	$Id: c_cpex03.c.sed,v 1.3 1994-08-24 16:01:56 haley Exp $
 */
#include <stdio.h>
#include <math.h>

#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define max(x,y)   ((x) > (y) ? (x) : (y))
#define min(x,y)   ((x) < (y) ? (x) : (y))
#define pow2(x)    ((x)*(x))

struct common1
{
    float xfoi[33], yfoj[33];
} cpmpc1_;

struct common2
{
    float xfij[33][33], yfij[33][33];
} cpmpc2_;

#define WSTYPE SED_WSTYPE
#define WKID   1

main()
{
    float zdat[33][33], rwrk[5000],p1[2],p2[2],p3[2],p4[2];
    int iwrk[1000],iama[20000];
    int i, j, iplt,iclv,iclu,nclv;
    float xvpl, xvpr, yvpb, yvpt;

/*
 * Declare the arrays needed by ARSCAM for x/y coordinates.
 */
    float xcra[1000],ycra[1000];
    int iara[10],igra[10];
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
 * Dimension a character variable to hold plot labels.
 */
    char labl[12];
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
    p1[0] = p2[0] = p3[0] = p4[0] = 0.;
    p1[1] = p2[1] = p3[1] = p4[1] = 0.;
/*
 * Open GKS.
 */
	gopen_gks("stdout",0);
	gopen_ws(WKID, NULL, WSTYPE);
	gactivate_ws(WKID);
/*
 * Turn off the clipping indicator.
 */
    gset_clip_ind (GIND_NO_CLIP);
/*
 * Set all the GKS aspect source flags to "individual".
 */
    gset_asfs (&iasf);
/*
 * Turn on the positioning of labels by the penalty scheme and provide a
 * little more room for X and Y coordinates defining contour lines, so
 * as not to have labels right next to each other on a contour line.
 */
    c_cpseti ("LLP - LINE LABEL POSITIONING",3);
    c_cpseti ("RWC - REAL WORKSPACE FOR CONTOURS",200);
/*
 * Turn on the drawing of the high and low label boxes.
 */
    c_cpseti ("HLB - HIGH/LOW LABEL BOX FLAG",1);
/*
 * Tell CONPACK to delete high/low labels which overlap the informational
 * label or another high/low label, but to move those which overlap the
 * edge inward a little.
 */
    c_cpseti ("HLO - HIGH/LOW LABEL OVERLAP FLAG",11);
/*
 * Make all CONPACK-written characters a little bigger.
 */
    c_cpsetr ("CWM - CHARACTER WIDTH MULTIPLIER",1.25);
/*
 * Turn on the drawing of the grid edge ("contour line number -1") and
 * thicken it somewhat.
 */
    c_cpseti ("PAI - PARAMETER ARRAY INDEX",-1);
    c_cpseti ("CLU - CONTOUR LEVEL USE FLAG",1);
    c_cpsetr ("CLL - CONTOUR LEVEL LINE WIDTH",2.);
/*
 * Tell CONPACK to do no SET call.
 */
    c_cpseti ("SET - DO-SET-CALL FLAG",0);
/*
 * Turn on the special-value feature and the outlining of special-value
 * areas ("contour line number -2"), using a double-width line.
 */
    c_cpsetr ("SPV - SPECIAL VALUE",1.e36);
    c_cpseti ("PAI - PARAMETER ARRAY INDEX",-2);
    c_cpseti ("CLU - CONTOUR LEVEL USE FLAG",1);
    c_cpsetr ("CLL - CONTOUR LEVEL LINE WIDTH",2.);
/*
 * Generate an array of test data.
 */
    gendat (zdat,33,33,33,20,20,-1.,1.);
/*
 * Put special values in a roughly circular area.
 */
    for( i = 0; i < 33; i++ ) {
        for( j = 0; j < 33; j++ ) {
            if ((pow2(i-19)+pow2(j-9)) < 25. ) zdat[j][i]=1.e36;
        }
    }
/*
 * Do four different plots, one in each quadrant.
 */
    for( iplt = 1; iplt <= 4; iplt++ ) {
/*
 * Compute viewport parameters.
 */
        xvpl=.0250+.5000*(float)((iplt-1)%2);
        xvpr=.4750+.5000*(float)((iplt-1)%2);
        yvpb=.0250+.5000*(float)((4-iplt)/2);
        yvpt=.4750+.5000*(float)((4-iplt)/2);
/*
 * For each of the four plots, use a different mapping function and
 * create a different background.
 */
        c_cpseti ("MAP - MAPPING FUNCTION",iplt);
/*
 * EZMAP.
 */
        if (iplt == 1) {
            c_mapsti ("GR - GRID INTERVAL",30);
            c_mapstc ("OU - OUTLINE DATASET","CO");
            c_mapsti ("DO - DOTTING OF OUTLINES",1);
            c_mappos (xvpl,xvpr,yvpb,yvpt);
            c_maproj ("OR - ORTHOGRAPHIC",40.,-95.,0.);
            c_mapset ("MA - MAXIMAL AREA",p1,p2,p3,p4);
            c_mapdrw();
            c_cpsetr ("XC1 - LONGITUDE AT I = 1",-160.);
            c_cpsetr ("XCM - LONGITUDE AT I = M",-30.);
            c_cpsetr ("YC1 - LATITUDE AT J = 1",-10.);
            c_cpsetr ("YCN - LATITUDE AT J = N",70.);
/*
 * Polar coordinates.
 */
        }
        else if (iplt == 2) {
            c_cpsetr ("XC1 - RHO AT I = 1",.1);
            c_cpsetr ("XCM - RHO AT I = M",1.);
            c_cpsetr ("YC1 - THETA AT J = 1",0.);
            c_cpsetr ("YCN - THETA AT J = N",90.);
            c_set    (xvpl,xvpr,yvpb,yvpt,0.,1.,0.,1.,1);
/*
 * Rectangular, irregularly-spaced.
 */
        }
        else if (iplt == 3) {
            c_cpsetr ("XC1 - X COORDINATE AT I = 1",1.);
            c_cpsetr ("XCM - X COORDINATE AT I = M",33.);
            c_cpsetr ("YC1 - Y COORDINATE AT J = 1",1.);
            c_cpsetr ("YCN - Y COORDINATE AT J = N",33.);
            c_set    (xvpl,xvpr,yvpb,yvpt,0.,1.,0.,1.,1);
            for( i = 0; i < 33; i++ ) {
                cpmpc1_.xfoi[i]=(float)log10((double)(1.+9.*(float)i/32.));
            }
            for( j = 0; j < 33; j++ ) {
                cpmpc1_.yfoj[j]=(float)log10((double)(1.+9.*(float)j/32.));
            }
/*
 * Parameterized distortion.
 */
        }
        else if (iplt == 4) {
            c_cpsetr ("XC1 - X COORDINATE AT I = 1",1.);
            c_cpsetr ("XCM - X COORDINATE AT I = M",33.);
            c_cpsetr ("YC1 - Y COORDINATE AT J = 1",1.);
            c_cpsetr ("YCN - Y COORDINATE AT J = N",33.);
            c_set    (xvpl,xvpr,yvpb,yvpt,0.,1.,0.,1.,1);
            for( i = 0; i < 33; i++ ) {
                for( j = 0; j < 33; j++ ) {
                    cpmpc2_.xfij[i][j]=(float)i/32.+((float)(16-i)/64.)*
                      ((float)(16-abs(j-16))/16.);
                    cpmpc2_.yfij[i][j]=(float)j/32.+((float)(16-j)/64.)*
                      ((float)(16-abs(i-16))/16.);
                }
            }
        }
/*
 * Initialize the drawing of the contour plot.
 */
        c_cprect (&zdat[0][0],33,33,33,rwrk,5000,iwrk,1000);
/*
 * Force the selection of contour levels, so that associated quantities
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
        nclv=nclv+2;
        c_cpseti ("NCL - NUMBER OF CONTOUR LEVELS",nclv);
        c_cpseti ("PAI - PARAMETER ARRAY INDEX",nclv-1);
        c_cpsetr ("CLV - CONTOUR LEVEL VALUE",-.15);
        c_cpseti ("CLU - CONTOUR LEVEL USE FLAG",0);
        c_cpseti ("AIA - AREA IDENTIFIER ABOVE LEVEL",1);
        c_cpseti ("AIB - AREA IDENTIFIER BELOW LEVEL",2);
        c_cpseti ("PAI - PARAMETER ARRAY INDEX",nclv);
        c_cpsetr ("CLV - CONTOUR LEVEL VALUE",.15);
        c_cpseti ("CLU - CONTOUR LEVEL USE FLAG",0);
        c_cpseti ("AIA - AREA IDENTIFIER ABOVE LEVEL",3);
        c_cpseti ("AIB - AREA IDENTIFIER BELOW LEVEL",1);
/*
 * Draw the contour plot.
 */
        c_arinam (iama,20000);
        c_cplbam (&zdat[0][0],rwrk,iwrk,iama);
        c_cpcldm (&zdat[0][0],rwrk,iwrk,iama,drawcl);
        c_cplbdr (&zdat[0][0],rwrk,iwrk);
        c_cpclam (&zdat[0][0],rwrk,iwrk,iama);
        c_arscam (iama,xcra,ycra,1000,iara,igra,10,shader);
/*
 * Compute and print statistics for the plot and label it.
 */
        sprintf( labl, "EXAMPLE 3-%d", iplt );
        capsap (labl,iama,20000);
        labtop (labl,.017);
    }
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

cpmpxy_(imap,xinp,yinp,xotp,yotp)
int *imap;
float *xinp, *yinp, *xotp, *yotp;
{
/*
 * this version of cpmpxy implements four different mappings:
 *
 *   imap = 1 implies an ezmap mapping.  xinp and yinp are assumed to be
 *   the longitude and latitude, in degrees, of a point on the globe.
 *
 *   imap = 2 implies a polar coordinate mapping.  xinp and yinp are
 *   assumed to be values of rho and theta (in degrees).
 *
 *   imap = 3 implies an orthogonal, but unequally-spaced mapping.  xinp
 *   is assumed to lie in the range from 1 to m, yinp in the range from
 *   1 to n, where m and n are the dimensions of the grid.  the common
 *   block cpmpc1 contains arrays xfoi and yfoj giving the x coordinates
 *   associated with i = 1 to m and the y coordinates associated with
 *   j = 1 to n.
 *
 *   imap = 4 implies a generalized distortion.  xinp is assumed to lie
 *   in the range from 1 to m, yinp in the range from 1 to n, where m
 *   and n are the dimensions of the grid.  the common block cpmpc2
 *   contains arrays xfij and yfij, giving the x and y coordinates
 *   associated with index pairs (i,j).
 *
 */
    int i, j;
/*
 * do the mapping.
 */
    if ( *imap == 1) {
        c_maptrn ( *yinp, *xinp, xotp, yotp);
    }
    else {
        if ( *imap == 2) {
            *xotp= *xinp*cos(.017453292519943 * *yinp);
            *yotp= *xinp*sin(.017453292519943 * *yinp);
        }
        else {
            if ( *imap == 3) {
                i=max(1,min(32,(int)(*xinp)));
                j=max(1,min(32,(int)(*yinp)));
                *xotp = ((float)(i+1) - *xinp)*cpmpc1_.xfoi[i-1]+
                        (*xinp-(float)i)*cpmpc1_.xfoi[i];
                *yotp = ((float)(j+1) - *yinp)*cpmpc1_.yfoj[j-1]+
                        (*yinp-(float)(j))*cpmpc1_.yfoj[j];
            }
            else {
                if ( *imap == 4) {
                    i=max(1,min(32,(int)(*xinp)));
                    j=max(1,min(32,(int)(*yinp)));
                    *xotp=((float)(j+1) - *yinp)*(((float)(i+1) - *xinp) *
                          cpmpc2_.xfij[i-1][j-1]+(*xinp-(float)(i)) *
                          cpmpc2_.xfij[  i][j-1])+(*yinp-(float)(j)) *
                          (((float)(i+1) - *xinp)*cpmpc2_.xfij[i-1][j]+
                          (*xinp-(float)(i))*cpmpc2_.xfij[i][j]);
                    *yotp=((float)(j+1) - *yinp)*(((float)(i+1) - *xinp) *
                          cpmpc2_.yfij[i-1][j-1]+(*xinp-(float)(i)) *
                          cpmpc2_.yfij[  i][j-1])+(*yinp-(float)(j))*
                          (((float)(i+1) - *xinp)*cpmpc2_.yfij[i-1][j]+
                          (*xinp-(float)(i))*cpmpc2_.yfij[i][j]);
                }
                else {
                    *xotp= *xinp;
                    *yotp= *yinp;
                }
            }
        }
    }
    return(1);
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
    int i, idr;
/*
 * this version of drawcl draws the polyline defined by the points
 * ((xcs(i),ycs(i)),i=1,ncs) if and only if none of the area identifiers
 * for the area containing the polyline are negative.  the dash package
 * routine curved is called to do the drawing.
 */
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
/*
 * define workspaces for the shading routine.
 */
    float dst[1100];
    int ind[1200], i, ish;
/*
 * turn off shading.
 */
    ish=0;
/*
 * if the area identifier for group 3 is a 1, turn on shading.
 */
    for( i=0; i < *nai; i++ ) {
          if (iag[i] == 3 && iai[i] == 1) ish=1;
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
