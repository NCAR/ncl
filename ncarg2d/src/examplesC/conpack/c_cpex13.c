/*
 * $Id: c_cpex13.c,v 1.1 1996-10-10 21:50:26 haley Exp $
 */
#include <stdio.h>
#include <ncarg/ncargC.h>
#include <ncarg/gks.h>
#include <math.h>

/*
 * This program produces a detailed picture of the contents of a typical
 * CONPACK-produced area map.  It then shows three different situations
 * that tend to cause problems.  This example is intended to be viewed
 * while reading the text from the programmer document for CONPACK that
 * describes each of the four frames.
 *
 * Define error file, Fortran unit number, workstation type, and
 * workstation ID.
 */
#define lunit "gmeta"
#define iwtype SED_WSTYPE
#define iwkid 1

/*
 * Define various local parameters.
 */
#define LRWK 5000
#define LIWK 5000
#define LAMA 20000
#define NCLV 4

main()
{
    int i, j, iclv;
    float xcrd, ycrd, rho, theta;
    extern void ardbpx();
    extern void circle();
/*
 * Declare the basic data array, a real workspace array, and an integer
 * workspace array.
 */
    float zdat[7][7],rwrk[LRWK];
    int iwrk[LIWK];
/*
 * Declare a bigger data array to use for rho/theta data.
 */
    float rhth[25][10];
/*
 * Declare an area-map array.
 */
    int iama[LAMA];
/*
 * Declare an array to hold contour levels.
 */
    float clev[NCLV];
/*
 * Define the contour levels at which contours are to be generated.
 */
    clev[0] = .25; clev[1] = .52; clev[2] = .90; clev[3] = 1.40;
/*
 * Open GKS.
 */
    gopen_gks ("stdout",0);
    gopen_ws (iwkid, lunit, iwtype);
    gactivate_ws(iwkid);
/*
 * Turn off clipping by GKS.
 */
    gset_clip_ind (GIND_NO_CLIP);
/*
 * Set internal parameters of AREAS that affect the appearance of the
 * debug plots produced by ARDBPX.
 */
    c_arsetr ("ID - IDENTIFIER DISTANCE",.008);
    c_arsetr ("IS - IDENTIFIER SIZE",.008);
    c_arsetr ("AL - ARROWHEAD LENGTH",0.);
    c_arsetr ("AW - ARROWHEAD WIDTH",0.);
/*
 * Tell the dash package to use alternating solids and gaps.  This
 * pattern will be used for the circles on frame 3.
 */
    c_dpsetc ("DPT - DASH PATTERN","$_");
/*
 * Tell PLOTCHAR to use font number 25 (a filled font) and to outline
 * each character.
 */
    c_pcseti ("FN - FONT NUMBER",25);
    c_pcseti ("OF - OUTLINE FLAG",1);
/*
 * Tell PLOTCHAR to tell the Bezier package to reproduce the curves
 * outlining the characters with a little less fidelity.  This cuts
 * down on the size of the metafile.
 */
    c_pcsetr ("FB - FIDELITY OF BEZIER CURVES",.00015);
/*
/*
 * ***** FIRST FRAME BEGINS ********************************************
 *
 * Put a label at the top of the first frame.
 */
  c_plchhq (c_cfux(.5),c_cfuy(.98),"A DETAILED VIEW OF A CONPACK AREA MAP (EDGE GROUP 3)",.015,0.,0.);
/*
 * Put informative labels at the top and bottom of the frame.
 */
    c_plchhq (c_cfux(.5),c_cfuy(.95),"This is a simple case that demonstrates all the essential features of a CONPACK area map.",.012,0.,0.);
    c_plchhq (c_cfux(.5),c_cfuy(.926),"All the edge segments in group 3 are shown, each with its own left and right area identifiers.",.012,0.,0.);
    c_plchhq (c_cfux(.5),c_cfuy(.062),"See the CONPACK programmer document for a complete description of this area map.",.012,0.,0.);
    c_plchhq (c_cfux(.5),c_cfuy(.038),"Also see the frames that follow for examples of situations in which problems can arise.",.012,0.,0.);
/*
 * Define the mapping from the "user" system to the fractional system.
 */
    c_set(.05,.95,.05,.95,-.1,1.1,-.1,1.1,1);
/*
 * Tell CONPACK not to call SET.
 */
    c_cpseti ("SET - DO-SET-CALL FLAG",0);
/*
 * Tell CONPACK to map output coordinates.  Included below is a special
 * version of CPMPXY that, when 'MAP' = 1, does an identity mapping, but
 * generates some out-of-range values.
 */
    c_cpseti ("MAP - MAPPING FLAG",1);
/*
 * Tell CONPACK what to expect as an out-of-range value in the output
 * from CPMPXY.
 */
    c_cpsetr ("ORV - OUT-OF-RANGE VALUE",1.e+12);
/*
 * Tell CONPACK not to select contour levels.  We"ll do it.
 */
    c_cpseti ("CLS - CONTOUR LEVEL SELECTION METHOD",0);
/*
 * Tell CONPACK how many contour levels to use and exactly what those
 * levels are.
 */
    c_cpseti ("NCL - NUMBER OF CONTOUR LEVELS",NCLV);

    for( iclv = 1; iclv <= NCLV; iclv++ ) {
        c_cpseti ("PAI - PARAMETER ARRAY INDEX",iclv);
        c_cpsetr ("CLV - CONTOUR LEVEL",clev[iclv-1]);
        c_cpseti ("CLU - CONTOUR LEVEL USE FLAG",3);
    }
/*
 * Tell CONPACK to position line labels using the regular scheme and
 * modify a few parameters so as to get labels in particular places.
 */
    c_cpseti ("LLP - LINE LABEL POSITIONING",2);

    c_cpsetr ("RC1 - REGULAR SCHEME CONSTANT 1",.55);
    c_cpsetr ("RC2 - REGULAR SCHEME CONSTANT 2",.85);
    c_cpsetr ("RC3 - REGULAR SCHEME CONSTANT 3",0.);

    c_cpsetr ("CWM - CHARACTER WIDTH MULTIPLIER",3.);
/*
 * Get rid of the informational label.
 */
    c_cpsetc ("ILT - INFORMATIONAL LABEL TEXT"," ");
/*
 * Tell CONPACK how to map the data grid into coordinates to be
 * delivered to CPMPXY.
 */
    c_cpsetr ("XC1 - X COORDINATE FOR I = 1",0.);
    c_cpsetr ("XCM - X COORDINATE FOR I = M",1.);
    c_cpsetr ("YC1 - Y COORDINATE FOR J = 1",0.);
    c_cpsetr ("YCN - Y COORDINATE FOR J = N",1.);
/*
 * Tell CONPACK what value is used in the data as a special value.
 */
    c_cpsetr ("SPV - SPECIAL VALUE",1.e+36);
/*
 * Generate a simple two-dimensional data field.
 */
    for( i = 1; i <= 7; i++ ) {
        xcrd=(float)(i-1)/6.;
        for( j = 1; j <= 7; j++ ) {
            ycrd = (float)(j-1)/6.;
            zdat[j-1][i-1] = pow(xcrd,2.)+pow(ycrd,2.);
        }
    }
/*
 * Put some special values in the lower left corner of the data field.
 */
    zdat[0][0]=1.e+36;
    zdat[1][0]=1.e+36;
    zdat[0][1]=1.e+36;
    zdat[1][1]=1.e+36;
/*
 * Tell CONPACK the dimensions of its data array, the real workspace
 * array, and the integer workspace array, so that it can initialize
 * itself to work with those arrays.
 */
    c_cprect (&zdat[0][0],7,7,7,rwrk,LRWK,iwrk,LIWK);
/*
 * Initialize the area map.
 */
    c_arinam (iama,LAMA);
/*
 * Put into the area map the viewport perimeter, the boundary of the
 * "invisible" area (the area in which CPMPXY returns the value "ORV"),
 * the edge of the grid, the edges of the special-value areas, and the
 * contour lines.
 */
    c_cpclam (&zdat[0][0],rwrk,iwrk,iama);
/*
 * Put the label boxes into the area map.
 */
    c_cplbam (&zdat[0][0],rwrk,iwrk,iama);
/*
 * Call a modified version of the AREAS routine ARDBPA to draw the
 * contents of the area map.
 */
    ardbpx (iama,3);
/*
 * Label features of interest on the plot.
 */
    c_plchhq (c_cfux(.90),c_cfuy(.11),"EDGE OF PLOTTER FRAME  ",.008,0.,1.);

    c_plchhq (c_cfux(.86),c_cfuy(.15),"EDGE OF VIEWPORT  ",.008,0.,1.);

    c_plchhq (c_cfux(.80),c_cfuy(.32),"EDGE",.008,0.,0.);
    c_plchhq (c_cfux(.80),c_cfuy(.30),"OF",.008,0.,0.);
    c_plchhq (c_cfux(.80),c_cfuy(.28),"GRID",.008,0.,0.);

    c_plchhq (c_cfux(.30),c_cfuy(.34),"THIS IS A",.008,0.,0.);
    c_plchhq (c_cfux(.30),c_cfuy(.32),"SPECIAL-VALUE",.008,0.,0.);
    c_plchhq (c_cfux(.30),c_cfuy(.30),"AREA, IN WHICH ALL",.008,0.,0.);
    c_plchhq (c_cfux(.30),c_cfuy(.28),"DATA VALUES ARE ",.008,0.,0.);
    c_plchhq (c_cfux(.30),c_cfuy(.26),"EQUAL TO 'SPV'",.008,0.,0.);

    c_plchhq (c_cfux(.77),c_cfuy(.83),"IN THIS AREA,",.008,0.,0.);
    c_plchhq (c_cfux(.77),c_cfuy(.81),"CPMPXY RETURNS",.008,0.,0.);
    c_plchhq (c_cfux(.77),c_cfuy(.79),"COORDINATE VALUES",.008,0.,0.);
    c_plchhq (c_cfux(.77),c_cfuy(.77),"EQUAL TO 'ORV'; AREA",.008,0.,0.);
    c_plchhq (c_cfux(.77),c_cfuy(.75),"IS INVISIBLE UNDER",.008,0.,0.);
    c_plchhq (c_cfux(.77),c_cfuy(.73),"MAPPING",.008,0.,0.);

    c_plchhq (c_cfux(.505),c_cfuy(.61),"HERE ARE",.008,0.,0.);
    c_plchhq (c_cfux(.505),c_cfuy(.59),"TWO",.008,0.,0.);
    c_plchhq (c_cfux(.505),c_cfuy(.57),"LABEL BOXES",.008,0.,0.);

    c_plchhq (c_cfux(.28),c_cfuy(.43),"CONTOUR BAND 1",.008,0.,0.);

    c_plchhq (c_cfux(.30),c_cfuy(.53),"CONTOUR BAND 2",.008,0.,0.);

    c_plchhq (c_cfux(.36),c_cfuy(.67),"CONTOUR BAND 3",.008,0.,0.);

    c_plchhq (c_cfux(.52),c_cfuy(.75),"CONTOUR BAND 4",.008,0.,0.);

    c_plchhq (c_cfux(.48),c_cfuy(.30),"CONTOUR",.008,0.,0.);
    c_plchhq (c_cfux(.48),c_cfuy(.28),"(LEVEL 1)",.008,0.,0.);

    c_plchhq (c_cfux(.62),c_cfuy(.31),"CONTOUR",.008,0.,0.);
    c_plchhq (c_cfux(.62),c_cfuy(.29),"(LEVEL 2)",.008,0.,0.);

    c_plchhq (c_cfux(.73),c_cfuy(.40),"CONTOUR",.008,0.,0.);
    c_plchhq (c_cfux(.73),c_cfuy(.38),"(LEVEL 3)",.008,0.,0.);

    c_plchhq (c_cfux(.745),c_cfuy(.615),"CONTOUR",.008,0.,0.);
    c_plchhq (c_cfux(.745),c_cfuy(.596),"(LEVEL 4)",.008,0.,0.);
/*
 * Advance the frame.
 */
    c_frame();
/*
 *
 * ***** SECOND FRAME BEGINS *******************************************
 *
 * Put a label at the top of the second frame.
 */
    c_plchhq (c_cfux(.5),c_cfuy(.98),"CONPACK AREA MAPS - FRAME 2",.015,0.,0.);
/*
 * Put informative labels at the top and bottom of the frame.
 */
    c_plchhq (c_cfux(.5),c_cfuy(.95),"This frame shows what happens when CPMPXY can't do inverses (or incorrectly says it can't).",.012,0.,0.);
    c_plchhq (c_cfux(.5),c_cfuy(.926),"The algorithm that generates the edge of the invisible area doesn't work so well then.",.012,0.,0.);
    c_plchhq (c_cfux(.5),c_cfuy(.062),"In this case, the effects aren't too bad; more serious effects are sometimes seen.",.012,0.,0.);
    c_plchhq (c_cfux(.5),c_cfuy(.038),"See the CONPACK programmer document for a complete discussion of this problem.",.012,0.,0.);
/*
 * Change to mapping function 2 to illustrate the problem with a CPMPXY
 * that doesn't do inverses.  "MAP" = 2 is just like "MAP" = 1 except
 * that it doesn't do inverses.
 */
    c_cpseti ("MAP - MAPPING FLAG",2);
/*
 * Tell CONPACK the dimensions of its data array, the real workspace
 * array, and the integer workspace array, so that it can initialize
 * itself to work with those arrays.
 */
    c_cprect (&zdat[0][0],7,7,7,rwrk,LRWK,iwrk,LIWK);
/*
 * Initialize the area map.
 */
    c_arinam (iama,LAMA);
/*
 * Put into the area map the viewport perimeter, the boundary of the
 * "invisible" area (the area in which CPMPXY returns the value "ORV"),
 * the edge of the grid, the edges of the special-value areas, and the
 * contour lines.
 */
    c_cpclam (&zdat[0][0],rwrk,iwrk,iama);
/*
 * Put the label boxes into the area map.
 */
    c_cplbam (&zdat[0][0],rwrk,iwrk,iama);
/*
 * Call a modified version of the AREAS routine ARDBPA to draw the
 * contents of the area map.
 */
    ardbpx (iama,3);
/*
 * Label features of interest on the plot.
 */
    c_plchhq (c_cfux(.90),c_cfuy(.11),"EDGE OF PLOTTER FRAME  ",.008,0.,1.);

    c_plchhq (c_cfux(.86),c_cfuy(.15),"EDGE OF VIEWPORT  ",.008,0.,1.);

    c_plchhq (c_cfux(.80),c_cfuy(.32),"EDGE",.008,0.,0.);
    c_plchhq (c_cfux(.80),c_cfuy(.30),"OF",.008,0.,0.);
    c_plchhq (c_cfux(.80),c_cfuy(.28),"GRID",.008,0.,0.);

    c_plchhq (c_cfux(.30),c_cfuy(.31),"SPECIAL-VALUE",.008,0.,0.);
    c_plchhq (c_cfux(.30),c_cfuy(.29),"AREA",.008,0.,0.);

    c_plchhq (c_cfux(.77),c_cfuy(.78),"INVISIBLE AREA",.008,0.,0.);

    c_plchhq (c_cfux(.505),c_cfuy(.59),"LABEL BOXES",.008,0.,0.);

    c_plchhq (c_cfux(.28),c_cfuy(.43),"CONTOUR BAND 1",.008,0.,0.);

    c_plchhq (c_cfux(.30),c_cfuy(.53),"CONTOUR BAND 2",.008,0.,0.);

    c_plchhq (c_cfux(.36),c_cfuy(.67),"CONTOUR BAND 3",.008,0.,0.);

    c_plchhq (c_cfux(.52),c_cfuy(.75),"CONTOUR BAND 4",.008,0.,0.);

    c_plchhq (c_cfux(.48),c_cfuy(.30),"CONTOUR",.008,0.,0.);
    c_plchhq (c_cfux(.48),c_cfuy(.28),"AT LEVEL 1",.008,0.,0.);

    c_plchhq (c_cfux(.62),c_cfuy(.31),"CONTOUR",.008,0.,0.);
    c_plchhq (c_cfux(.62),c_cfuy(.29),"AT LEVEL 2",.008,0.,0.);

    c_plchhq (c_cfux(.73),c_cfuy(.40),"CONTOUR",.008,0.,0.);
    c_plchhq (c_cfux(.73),c_cfuy(.38),"AT LEVEL 3",.008,0.,0.);

    c_plchhq (c_cfux(.745),c_cfuy(.615),"CONTOUR",.008,0.,0.);
    c_plchhq (c_cfux(.745),c_cfuy(.596),"AT LEVEL 4",.008,0.,0.);
/*
 * Advance the frame.
 */
    c_frame();
/*
 *
 * ***** THIRD FRAME BEGINS ********************************************
 *
 * Put a label at the top of the third frame.
 */
    c_plchhq (c_cfux(.5),c_cfuy(.98),"CONPACK AREA MAPS - FRAME 3",.015,0.,0.);
/*
 * Put informative labels at the top and bottom of the frame.
 */
    c_plchhq (c_cfux(.5),c_cfuy(.95),"Sometimes a segment of a contour line is parallel to and just barely inside the edge of the grid.",.012,0.,0.);
    c_plchhq (c_cfux(.5),c_cfuy(.926),"The dashed circles in the area map below show the locations of two such contour-line segments.",.012,0.,0.);
    c_plchhq (c_cfux(.5),c_cfuy(.074), "Prior to version 4, the outer area identifier for such a segment could 'leak' outside the grid.",.012,0.,0.);
    c_plchhq (c_cfux(.5),c_cfuy(.050),"Now, conflicting information from near-coincident segments is resolved in such a way as to avoid problems.",.012,0.,0.);
    c_plchhq (c_cfux(.5),c_cfuy(.026),"See the CONPACK programmer document for a complete discussion of this problem.",.012,0.,0.);
/*
 * Change the mapping back to what it was on the first frame.
 */
    c_cpseti ("MAP - MAPPING FLAG",1);
/*
 * Generate data. The value "4.984615" has been carefully chosen to
 * create the problem.
 */
    for( i = 1; i <= 7; i++ ) {
        xcrd=(float)(i-1)/6.;
        for( j = 1; j <= 7; j++ ) {
            ycrd=(float)(j-1)/6.;
            zdat[j-1][i-1]=4.984615*(pow((xcrd-.58333333),2.)+pow((ycrd-.58333333),2.));
        }
    }
/*
 * Salt in some special values.
 */
    zdat[0][0]=1.e+36;
    zdat[1][0]=1.e+36;
    zdat[0][1]=1.e+36;
    zdat[1][1]=1.e+36;
/*
 * Initialize the area map.
 */
    c_arinam (iama,LAMA);
/*
 * Tell CONPACK the dimensions of its data array, the real workspace
 * array, and the integer workspace array, so that it can initialize
 * itself to work with those arrays.
 */
    c_cprect (&zdat[0][0],7,7,7,rwrk,LRWK,iwrk,LIWK);
/*
 * Put into the area map the viewport perimeter, the boundary of the
 * "invisible" area (the area in which CPMPXY returns the value "ORV"),
 * the edge of the grid, the edges of the special-value areas, and the
 * contour lines.
 */
    c_cpclam (&zdat[0][0],rwrk,iwrk,iama);
/*
 * Call a modified version of the AREAS routine ARDBPA to draw the
 * contents of the area map.
 */
    ardbpx (iama,3);
/*
 * Draw a couple of circles around the problem areas.
 */
    circle (.1+.8*c_cufx(.58333333),.1+.8*c_cufy(1.),.065);
    circle (.1+.8*c_cufx(1.),.1+.8*c_cufy(.58333333),.065);
/*
 * Label features of interest on the plot.
 */
    c_plchhq (c_cfux(.90),c_cfuy(.11),"EDGE OF PLOTTER FRAME  ",.008,0.,1.);

    c_plchhq (c_cfux(.86),c_cfuy(.15),"EDGE OF VIEWPORT  ",.008,0.,1.);

    c_plchhq (c_cfux(.80),c_cfuy(.31),"EDGE",.008,0.,0.);
    c_plchhq (c_cfux(.80),c_cfuy(.29),"OF",.008,0.,0.);
    c_plchhq (c_cfux(.80),c_cfuy(.27),"GRID",.008,0.,0.);

    c_plchhq (c_cfux(.30),c_cfuy(.31),"SPECIAL-VALUE",.008,0.,0.);
    c_plchhq (c_cfux(.30),c_cfuy(.29),"AREA",.008,0.,0.);

    c_plchhq (c_cfux(.77),c_cfuy(.78),"INVISIBLE AREA",.008,0.,0.);
/*
 * Advance the frame.
 */
    c_frame();
/*
 *
 * ***** FOURTH FRAME BEGINS *******************************************
 *
 * Put a label at the top of the fourth frame.
 */
    c_plchhq (c_cfux(.5),c_cfuy(.98),"CONPACK AREA MAPS - FRAME 4",.015,0.,0.);
/*
 * Put informative labels at the top and bottom of the frame.
 */
    c_plchhq (c_cfux(.5),c_cfuy(.95),"Some mappings transform two different parts of the grid to the same place in user coordinate space.",.012,0.,0.);
    c_plchhq (c_cfux(.5),c_cfuy(.926),"The :F33:r/q:F: mapping used here maps the grid into a doughnut; left and right edges map to the same line.",.012,0.,0.);
    c_plchhq (c_cfux(.5),c_cfuy(.074),"Similarly, EZMAP frequently maps the left and right edges of the grid into the same great circle.",.012,0.,0.);
    c_plchhq (c_cfux(.5),c_cfuy(.050), "This can sometimes cause area identifiers for the outside of the grid to appear to apply to the inside.",.012,0.,0.);
    c_plchhq (c_cfux(.5),c_cfuy(.026),"See the CONPACK programmer document for a complete discussion of this problem.",.012,0.,0.);
/*
 * Redefine the mapping from the user system to the fractional system.
 */
    c_set    (.05,.95,.05,.95,-1.1,1.1,-1.1,1.1,1);
/*
 * Change the mapping function to be used.  For "MAP" = 3, we get a
 * standard rho/theta mapping.
 */
    c_cpseti ("MAP - MAPPING FLAG",3);
/*
 * Tell CONPACK that no out-of-range values will be returned.
 */
    c_cpsetr ("ORV - OUT-OF-RANGE VALUE",0.);
/*
 * Change the X and Y values assumed to correspond to the edges of the
 * data grid.
 */
    c_cpsetr ("XC1 - X COORDINATE FOR I = 1",.3);
    c_cpsetr ("XCM - X COORDINATE FOR I = M",1.);
    c_cpsetr ("YC1 - Y COORDINATE FOR J = 1",  0.);
    c_cpsetr ("YCN - Y COORDINATE FOR J = N",360.);
/*
 * Generate rho/theta data.
 */
    for( i = 1; i <= 10; i++ ) {
        rho=.3+.07*(float)i;
        for( j = 1; j <= 25; j++ ) {
            theta=.017453292519943*(float)(15*j-15);
            rhth[j-1][i-1]=2.*rho*pow(cos(theta),2.)+rho*pow(sin(theta),2.);
        }
    }
/*
 * Tell CONPACK the dimensions of its data array, the real workspace
 * array, and the integer workspace array, so that it can initialize
 * itself to work with those arrays.
 */
    c_cprect (&rhth[0][0],10,10,25,rwrk,LRWK,iwrk,LIWK);
/*
 * Initialize the area map.
 */
    c_arinam (iama,LAMA);
/*
 * Put into the area map the viewport perimeter, the  edge of the grid,
 * the edges of the special-value areas, and the contour lines.
 */
    c_cpclam (&rhth[0][0],rwrk,iwrk,iama);
/*
 * Call a modified version of the AREAS routine ARDBPA to draw the
 * contents of the area map.
 */
    ardbpx (iama,3);
/*
 * Label features of interest on the plot.
 */
    c_plchhq (c_cfux(.90),c_cfuy(.11),"EDGE OF PLOTTER FRAME  ",.008,0.,1.);

    c_plchhq (c_cfux(.86),c_cfuy(.15),"EDGE OF VIEWPORT  ",.008,0.,1.);

    c_plchhq (c_cfux(.24),c_cfuy(.83),"UPPER EDGE OF",.008,0.,0.);
    c_plchhq (c_cfux(.24),c_cfuy(.81),"DATA GRID MAPS",.008,0.,0.);
    c_plchhq (c_cfux(.24),c_cfuy(.79),"TO OUTER CIRCLE",.008,0.,0.);

    c_plchhq (c_cfux(.50),c_cfuy(.52),"LOWER EDGE OF",.008,0.,0.);
    c_plchhq (c_cfux(.50),c_cfuy(.50),"DATA GRID MAPS",.008,0.,0.);
    c_plchhq (c_cfux(.50),c_cfuy(.48),"TO INNER CIRCLE",.008,0.,0.);

    c_plchhq (c_cfux(.76),c_cfuy(.83),"LEFT AND RIGHT EDGES",.008,0.,0.);
    c_plchhq (c_cfux(.76),c_cfuy(.81),"OF DATA GRID MAP TO",.008,0.,0.);
    c_plchhq (c_cfux(.76),c_cfuy(.79),"HORIZONTAL LINE",.008,0.,0.);
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
/*
 * Done.
 */
}

void NGCALLF(cpmpxy,CPMPXY)(imap,xinp,yinp,xotp,yotp)
int *imap;
float *xinp, *yinp, *xotp, *yotp;
{
/*
 * When "MAP" = 1, this version of CPMPXY just does the identity mapping
 * and its inverse except in the upper right corner, in which it returns
 * the out-of-range value (for illustrative purposes).
 *
 * Using "MAP" = 2 is the same except that the routine says that it can't
 * do the inverse mapping (IMAP = 0 and *xinp = 1 gives *yinp = 1 instead
 * of 3).  This is used to show the adverse effects on the generation of
 * the edge of the area invisible under the mapping.
 *
 * Using "MAP" = 3 gives the polar coordinate transformation and its
 * inverse.  No out-of-range values can be returned.
 */
    if (*imap == 0) {
        if (*xinp == 1.) {
            *yinp=3.;
        }
        else if (*xinp == 2.) {
            *yinp=1.;
        }
        else if (*xinp == 3.) {
            *yinp=3.;
        }
        else {
            *yinp=3.;
        }
    }
    else if (abs(*imap) == 1 || abs(*imap) == 2) {
        if (pow((*xinp-1.),2.)+pow((*yinp-1.),2.) > .0625) {
            *xotp=*xinp;
            *yotp=*yinp;
        }
        else {
            *xotp=1.e+12;
            *yotp=1.e+12;
        }
    }
    else if (abs(*imap) == 3) {
        if (*imap > 0) {
            *xotp = *xinp*cos(.017453292519943*(*yinp));
            *yotp = *xinp*sin(.017453292519943*(*yinp));
        }
        else {
            *xotp = sqrt(pow(*xinp,2.)+pow(*yinp,2.));
            *yotp = 57.2957795130823*atan2(*yinp,*xinp);
        }
    }
    else {
        *xotp = *xinp;
        *yotp = *yinp;
    }
/*
 * Done.
 */
    return;
}

void ardbpx (iama,igip)
int *iama, igip;
{
    float xvpl,xvpr,yvpb,yvpt,xwdl,xwdr,ywdb,ywdt;
    int lnlg, igid, iail, iair;
    float rxcn, rycn, rxco, ryco;
    int indx, lama2;
/*
 * The routine ARDBPX produces a picture of that part of the contents of
 * the area map iama that belongs to the group IGIP; if IGIP is zero or
 * negative, all groups of edges are shown.  This is a modified version
 * of the AREAS routine ARDBPA.  No label is written at the top.  All
 * color-setting and error-recovery code has been removed.  The code
 * computing RXCN and RYCN has been changed to force the picture into
 * a smaller square than the whole plotter frame (so that labels near
 * the edge are readable and there is room for additional labels at top
 * and bottom).
 *
 * The common block ARCOM1 is used to communicate with the arrow-drawing
 * routine ARDBDA.
 */
struct common {
    float dt;
} NGCALLF(arcom1,ARCOM1);

/*
 * Bump the line width by a factor of two.
 */
      gset_linewidth (2.);
/*
 * Extract the length of the area map.
 */
    lama2 = iama[0];
/*
 * Save the current state of the SET call and switch to the fractional
 * coordinate system.
 */
    c_getset (&xvpl,&xvpr,&yvpb,&yvpt,&xwdl,&xwdr,&ywdb,&ywdt,&lnlg);
    c_set (  0.,  1.,  0.,  1.,  0.,  1.,  0.,  1.,   1);
/*
 * Trace the edges in the area map, drawing arrows as we go.
 */
    NGCALLF(arcom1,ARCOM1).dt=0.;
    indx=8;
    rxcn=.5;
    rycn=.5;
start:
    rxco=rxcn;
    ryco=rycn;

    rxcn=.1+.8*(float)(iama[indx])/1000000.;
    rycn=.1+.8*(float)(iama[indx+1])/1000000.;

    if (iama[indx+6] != 0) {
        igid=abs(iama[indx+6]);
        if (igid < iama[5]) {
            igid=iama[iama[0]-igid-1]/2;
        }
        else {
            igid=iama[igid-1]/2;
        }
        if (igip <= 0 || igid == igip) {
            iail=iama[indx+7];
            if (iail > 0) iail=iama[iail-1]/2;
            iair=iama[indx+8];
            if (iair > 0) iair=iama[iair-1]/2;
            NGCALLF(ardbda,ARDBDA) (&rxco,&ryco,&rxcn,&rycn,&iail,&iair,&igip,&igid);
        }
    }
    else {
        NGCALLF(arcom1,ARCOM1).dt=0.;
    }

    if (iama[indx+2] != 0) {

        indx=iama[indx+2];
        goto start;
    }
/*
 * restore the original set call.
 */
    c_set (xvpl,xvpr,yvpb,yvpt,xwdl,xwdr,ywdb,ywdt,lnlg);
/*
 * set the line width back to normal.
 */
    gset_linewidth (1.);
/*
 * Done.
 */
    return;
}


void circle (xcen,ycen,radc)
float xcen, ycen, radc;
{
    int i;
    float angr;
/*
 * This routine draws a circle with center (XCEN,YCEN) and radius RADC.
 * All input variables are stated in the fractional system.
 */
    c_dpdraw (xcen+radc,ycen,0);

    for( i=1; i <= 90; i++ ) {
        angr=.017453292519943*(float)(4*i);
        c_dpdraw (xcen+radc*cos(angr),ycen+radc*sin(angr),1);
    }

    c_dpdraw (0.,0.,2);
/*
 * Done.
 */
    return;
}

