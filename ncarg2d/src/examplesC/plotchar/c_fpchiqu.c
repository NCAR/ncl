/*
 *	$Id: c_fpchiqu.c,v 1.1 1994-08-02 20:08:03 haley Exp $
 */
#include <stdio.h>
#include <math.h>
#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define WSTYPE  SED_WSTYPE
#define WKID    1

main()
{
	extern void drawbx();
	Gcolr_rep rgb;
/*
 * OPEN GKS, OPEN WORKSTATION OF TYPE 1, ACTIVATE WORKSTATION
 */
    gopen_gks ("stdout",0);
    gopen_ws (WKID, NULL, WSTYPE);
    gactivate_ws (WKID);
/*
 * Set the "fill area interior style" to "solid".
 */
	gset_fill_int_style (1);
/*
 * Do a call to SET which allows us to use fractional coordinates.
 */
	c_set (0.,1.,0.,1.,0.,1.,0.,1.,1);
/*
 * Define some colors to use.
 */
	rgb.rgb.red = 1.; rgb.rgb.green = 1.; rgb.rgb.blue = 1.;
	gset_colr_rep (WKID,0,&rgb);
	rgb.rgb.red = 0.; rgb.rgb.green = .0; rgb.rgb.blue = .0;
	gset_colr_rep (WKID,1,&rgb);
	rgb.rgb.red = 0.; rgb.rgb.green = .5; rgb.rgb.blue = .5;
	gset_colr_rep (WKID,2,&rgb);
	rgb.rgb.red = .9; rgb.rgb.green = .9; rgb.rgb.blue = 0.;
	gset_colr_rep (WKID,3,&rgb);
	rgb.rgb.red = 1.; rgb.rgb.green = .3; rgb.rgb.blue = .3;
	gset_colr_rep (WKID,4,&rgb);
	rgb.rgb.red = 0.; rgb.rgb.green = 0.; rgb.rgb.blue = 1.;
	gset_colr_rep (WKID,5,&rgb);
	rgb.rgb.red = .2; rgb.rgb.green = .2; rgb.rgb.blue = .2;
	gset_colr_rep (WKID,6,&rgb);
	rgb.rgb.red = .8; rgb.rgb.green = .8; rgb.rgb.blue = .8;
	gset_colr_rep (WKID,7,&rgb);
/*
 * Do a single frame showing various capabilities of PLCHHQ.
 *
 * Put labels at the top of the plot.
 */
	c_plchhq (.5,.98,"PLCHHQ - VARIOUS CAPABILITIES",.02,0.,0.);
/*
 * First, write characters at various sizes.
 */
	c_plchhq (.225,.900,"SIZE is -1.0",-1.0,0.,0.);
	c_plchhq (.225,.873,"SIZE is -.75",-.75,0.,0.);
	c_plchhq (.225,.846,"SIZE is .015",.015,0.,0.);
	c_plchhq (.225,.811,"SIZE is .020",.020,0.,0.);
	c_plchhq (.225,.776,"SIZE is 15.0",15.0,0.,0.);
	c_plchhq (.225,.742,"SIZE is 20.0",20.0,0.,0.);
/*
 * Next, write characters at various angles.
 */
	c_plchhq (.225,.453,"   ANGD is   0.",.012,  0.,-1.);
	c_plchhq (.225,.453,"   ANGD is  45.",.012, 45.,-1.);
	c_plchhq (.225,.453,"   ANGD is  90.",.012, 90.,-1.);
	c_plchhq (.225,.453,"   ANGD is 135.",.012,135.,-1.);
	c_plchhq (.225,.453,"   ANGD is 180.",.012,180.,-1.);
	c_plchhq (.225,.453,"   ANGD is 225.",.012,225.,-1.);
	c_plchhq (.225,.453,"   ANGD is 270.",.012,270.,-1.);
	c_plchhq (.225,.453,"   ANGD is 315.",.012,315.,-1.);
/*
 * Next, use various values of the centering option.
 */
	c_plchhq (.225,.164,"CNTR is -1.5",.012,0.,-1.5);
	c_plchhq (.225,.140,"CNTR is -1.0",.012,0.,-1.0);
	c_plchhq (.225,.116,"CNTR is -0.5",.012,0.,-0.5);
	c_plchhq (.225,.092,"CNTR is  0.0",.012,0., 0.0);
	c_plchhq (.225,.068,"CNTR is +0.5",.012,0.,+0.5);
	c_plchhq (.225,.044,"CNTR is +1.0",.012,0.,+1.0);
	c_plchhq (.225,.020,"CNTR is +1.5",.012,0.,+1.5);
/*
 * Turn on the computation of text-extent-vector magnitudes and use
 * them to draw a box around a label.  (DRAWBX is not part of PLOTCHAR;
 * the code for it appears at the end of this example.)
 */
	c_pcseti ("TE - TEXT EXTENT FLAG",1);

	c_plchhq (.130,.140,"TEXT EXTENT BOX",.012,33.,0.);
	drawbx (.130,.140,33.,.01);

	c_pcseti ("TE - TEXT EXTENT FLAG",0);
/*
 * On the right side of the frame, create examples of the various kinds
 * of function codes.  First, do them using high-quality characters.
 */
	c_plchhq (.715,.900,"HIGH-QUALITY CHARACTERS USED BELOW",.012,0.,0.);

	c_pcsetc ("FC","$");
	c_plchhq (.625,.870,"INPUT STRING",.012,0.,0.);
	c_plchhq (.625,.840,"------------",.012,0.,0.);
	c_plchhq (.625,.810,":L:A",.012,0.,0.);
	c_plchhq (.625,.780,":IGL:A",.012,0.,0.);
	c_plchhq (.625,.750,"A:S:2:N:+B:S:2:N:",.012,0.,0.);
	c_plchhq (.625,.720,"A:S:B",.012,0.,0.);
	c_plchhq (.625,.690,"A:SPU:B",.012,0.,0.);
	c_plchhq (.625,.660,":GIU:+",.012,0.,0.);
	c_plchhq (.625,.630,":1045:",.012,0.,0.);
	c_plchhq (.625,.600,"10:S:10:S:100",.012,0.,0.);
	c_plchhq (.625,.570,"X:B1:2:S1:3",.012,0.,0.);
	c_plchhq (.625,.540,"X:B1:2:S:3:N:Y:S:2",.012,0.,0.);
	c_plchhq (.625,.510,"X:S:A:B:1:NN:ABC",.012,0.,0.);
	c_plchhq (.625,.480,"1.3648:L1:410:S:-13",.012,0.,0.);

	c_pcsetc ("FC",":");
	c_plchhq (.875,.870,"RESULT",.012,0.,0.);
	c_plchhq (.875,.840,"------",.012,0.,0.);
	c_plchhq (.875,.810,":L:A",.012,0.,0.);
	c_plchhq (.875,.780,":IGL:A",.012,0.,0.);
	c_plchhq (.875,.750,"A:S:2:N:+B:S:2:N:",.012,0.,0.);
	c_plchhq (.875,.720,"A:S:B",.012,0.,0.);
	c_plchhq (.875,.690,"A:SPU:B",.012,0.,0.);
	c_plchhq (.875,.660,":GIU:+",.012,0.,0.);
	c_plchhq (.875,.630,":1045:",.012,0.,0.);
	c_plchhq (.875,.600,"10:S:10:S:100",.012,0.,0.);
	c_plchhq (.875,.570,"X:B1:2:S1:3",.012,0.,0.);
	c_plchhq (.875,.540,"X:B1:2:S:3:N:Y:S:2",.012,0.,0.);
	c_plchhq (.875,.510,"X:S:A:B:1:NN:ABC",.012,0.,0.);
	c_plchhq (.875,.480,"1.3648:L1:410:S:-13",.012,0.,0.);
/*
 * Show various other features like lines with several fonts.
 */
	c_plchhq (.715,.440,"OTHER FEATURES",.012,0.,0.);
/*
 * Show the use of fontcap databases and some of
 * the new features added in June of 1990.
 */
	c_plotif (0.,0.,2);
/*
 * Temporarily use the slash as a function code character.
 */
	c_pcsetc ("FC - FUNCTION CODE CHARACTER","/");
/*
 * Combine characters from several different fonts to produce a single
 * line.
 */
	c_plchhq (.990,.410,"/F13/A line with characters from several fonts:  /F8/P/BF13/0/N/=/F5/g/SF13/2/N/+/F5/j/SF13/2/N/",.012,0.,1.);
/*
 * Reset the internal parameter "FN" to 4 and write a line illustrating
 * the effect of function codes "Fn", "F", and "F0".  Then reset "FN"
 * to 0.
 */
	c_pcseti ("FN - FONT NUMBER",4);
	c_plchhq (.990,.380,"Set 'FN' (Font Number) to 4 and then use 'F' function codes:",.012,0.,1.);
	c_plchhq (.990,.350,"Before F10 - /F10/after F10 - /F/after F - /F0/after F0.",.012,0.,1.);
	c_pcseti ("FN - FONT NUMBER",0);
/*
 * Write lines illustrating various kinds of zooming.
 */
	c_plchhq (.990,.320,"/F13/Unzoomed characters from font 13.",.012,0.,1.);
	c_plchhq (.990,.290,"/F13X130Q/Characters zoomed in width, using X130Q.",.012,0.,1.);
	c_plchhq (.990,.260,"/F13Y130Q/Characters zoomed in height, using Y130Q.",.012,0.,1.);
	c_plchhq (.99000,.230,"/F13Z130Q/Characters zoomed both ways, using Z130Q.",.012,0.,1.);
/*
 * Write a line illustrating non-aligned zooming in height.
 */
	c_plchhq (.990,.200,"/F13/Unaligned zoom of characters: /F16Y200/S/Y/cientific /Y200/V/Y/isualization /Y200/G/Y/roup", .012,0.,1.);
/*
 * Write lines illustrating the use of 'AS' and 'SS'.
 */
	c_pcsetr ("AS - ADD SPACE BETWEEN CHARACTERS     ",.125);
	c_pcsetr ("SS - SUBTRACT SPACE BETWEEN CHARACTERS",  0.);
	c_plchhq (.990,.170,"/F14/Line with 'AS' = .125 and 'SS' = 0.",.012,0.,1.);
	c_pcsetr ("AS - ADD SPACE BETWEEN CHARACTERS     ",  0.);
	c_pcsetr ("SS - SUBTRACT SPACE BETWEEN CHARACTERS",  0.);
	c_plchhq (.990,.140,"/F14/Line with 'AS' = 0. and 'SS' = 0.",.012,0.,1.);
	c_pcsetr ("AS - ADD SPACE BETWEEN CHARACTERS     ",  0.);
	c_pcsetr ("SS - SUBTRACT SPACE BETWEEN CHARACTERS",.125);
	c_plchhq (.990,.110, "/F14/Line with 'AS' = 0. and 'SS' = .125",.012,0.,1.);
	c_pcsetr ("AS - ADD SPACE BETWEEN CHARACTERS     ",  0.);
	c_pcsetr ("SS - SUBTRACT SPACE BETWEEN CHARACTERS",  0.);
/*
 * Return to a colon as the function code character.
 */
	c_pcsetc ("FC - FUNCTION CODE CHARACTER",":");
/*
 * Go back to normal line width.
 */
	c_plotif (0.,0.,2);
	gset_linewidth (1.);
/*
 * Draw a bounding box
 */
	c_line(0.,0.,1.,0.);
	c_line(1.,0.,1.,1.);
	c_line(1.,1.,0.,1.);
	c_line(0.,1.,0.,0.);
/*
 * Advance the frame.
 */
	c_frame();
/*
 * Do a single frame showing some of the new features added in December
 * of 1992.
 *
 * Put a label at the top of the plot and, below that, an explanatory
 * note.
 */
	c_plchhq (.5,.975,":F25:PLCHHQ - FEATURES ADDED 12/92",.025,0.,0.);

	c_pcsetr ("SS - SUBTRACT SPACE BETWEEN CHARACTERS",.275);
	c_plchhq (.5,.938,":F13:(Use idt's 'zoom' to view some of this in detail, especially stacking.",.017,0.,0.);
	c_pcsetr ("SS - SUBTRACT SPACE BETWEEN CHARACTERS",0.);
/*
 * Illustrate the use of filled fonts with shadows and outlines.
 *
 * Write a line.
 */
	c_plchhq (.5,.900,":F26:By default, the current foreground color is used.",.024,0.,0.);
/*
 * Define the principal color to be used for characters.
 */
	c_pcseti ("CC - CHARACTER COLOR",4);
/*
 * Write another line.
 */
	c_plchhq (.5,.850,":F26:A non-negative 'CC' requests a different color.", .026,0.,0.);
/*
 * Turn on character shadows and define various characteristics of the
 * shadow.
 */
	c_pcseti ("SF - SHADOW FLAG",1);
	c_pcsetr ("SX - SHADOW OFFSET IN X",-.15);
	c_pcsetr ("SY - SHADOW OFFSET IN Y",-.15);
	c_pcseti ("SC - SHADOW COLOR",1);
/*
 * Write another line.
 */
	c_plchhq (.5,.796,":F26:'SF', 'SC', 'SX', and 'SY' create shadows.",.028,0.,0.);
/*
 * Turn on character outlines and define the color of the outline.
 */
	c_pcseti ("SF - SHADOW FLAG",0);
	c_pcseti ("OF - OUTLINE FLAG",1);
	c_pcseti ("OC - OUTLINE COLOR",1);
	c_pcseti ("OL - OUTLINE LINE WIDTH",1);
/*
 * Write another line.
 */
	c_plchhq (.5,.738,":F26:'OF', 'OC', and 'OL' add outlines.",.030,0.,0.);
/*
 * Turn on the drawing of boxes and define characteristics of them.
 */
	c_pcseti ("BF - BOX FLAG",7);
	c_pcseti ("BL - BOX LINE WIDTH",2);
	c_pcsetr ("BM - BOX MARGIN",.15);
	c_pcsetr ("BX - BOX SHADOW X OFFSET",-.1);
	c_pcsetr ("BY - BOX SHADOW Y OFFSET",-.1);
	c_pcseti ("BC(1) - BOX COLOR - BOX OUTLINE    ",5);
	c_pcseti ("BC(2) - BOX COLOR - BOX FILL       ",7);
	c_pcseti ("BC(3) - BOX COLOR - BOX SHADOW FILL",1);
/*
 * Write another line.
 */
	c_plchhq (.5,.672,":F26:'BF', 'BC', 'BL', 'BM', 'BX', and 'BY' add a box.",.026,0.,0.);
/*
 * Get rid of the box shadow, which doesn't add much.
 */
	c_pcseti ("BF - BOX FLAG",3);
/*
 * Write another line.
 */
	c_pcsetc ("FC - FUNCTION-CODE CHARACTER","/");
	c_plchhq (.5,.592,"/F26/'MA' and 'OR' are used for mapping:",.030,0.,0.);
	c_pcsetc ("FC - FUNCTION CODE CHARACTER",":");
/*
 * Write a couple of headers for the plots that follow.
 */
	c_plchhq (.28,.528,":F25:(EZMAP)",.024,0.,0.);
	c_plchhq (.72,.528,":F33:(r:F25: and :F33:q)",.024,0.,0.);
/*
 * Initialize EZMAP and draw a background.
 */
	c_mapstc ("OU","CO");
	c_mapsti ("GR",5);
	c_mappos (.065,.495,.065,.495);
	c_mapstr ("SA",8.5);
	c_maproj ("SV",0.,-25.,0.);
	c_mapint();
	c_maplot();
	c_mapgrd();
/*
 * Tell PLOTCHAR to map characters through EZMAP.
 */
	c_pcseti ("MA - MAPPING FLAG",1);
	c_pcsetr ("OR - OUT-OF-RANGE FLAG",1.e12);
/*
 * Write a line across the surface of the globe.
 */
	c_plchhq (-25.,0.,":F25Y200:NCAR GRAPHICS",8.,30.,0.);
/*
 * Do an appropriate SET call for a rho-theta mapping.
 */
	c_set    (.505,.935,.065,.495,-27.5,27.5,-27.5,27.5,1);
/*
 * Tell PLOTCHAR to use a rho-theta mapping.
 */
	c_pcseti ("MA - MAPPING FLAG",2);
	c_pcsetr ("OR - OUT-OF-RANGE FLAG",0.);
/*
 * Write three lines in rho/theta space, orienting them so they come out
 * in a circle after mapping.
 */
	c_plchhq (20., 90.,":F25Y125:NCAR GRAPHICS",8.,-90.,0.);
	c_plchhq (20.,210.,":F25Y125:NCAR GRAPHICS",8.,-90.,0.);
	c_plchhq (20.,-30.,":F25Y125:NCAR GRAPHICS",8.,-90.,0.);
/*
 * Turn off mapping and recall SET to allow fractional coordinates again.
 */
	c_pcseti ("MA - MAPPING FLAG",0);

	c_set    (0.,1.,0.,1.,0.,1.,0.,1.,1);
/*
 * Change the drawing order to allow for "stacking" characters from
 * right to left.
 */
	c_pcseti ("DO - DRAWING ORDER",-2);
/*
 * Reduce the space between characters so the "stacking" is visible.
 */
	c_pcsetr ("SS - SUBTRACT-SPACE FLAG",.3);
/*
 * Turn off the box.  Make the shadows black and position them so they
 * help make the stacked characters readable.
 */
	c_pcseti ("BF - BOX FLAG",0);
	c_pcseti ("SC - SHADOW COLOR",0);
	c_pcsetr ("SX - SHADOW OFFSET IN X",.1);
	c_pcsetr ("SY - SHADOW OFFSET IN Y",0.);
/*
 * Write a final line demonstrating "stacking".
 */
    c_plchhq (.5,.030,":F26:Use    'DO'    and    'SS'    to   'stack'    characters    in    either    direction.",.026,0.,0.);
/*
 * Draw a bounding box
 */
    c_line(0.,0.,1.,0.);
    c_line(1.,0.,1.,1.);
    c_line(1.,1.,0.,1.);
    c_line(0.,1.,0.,0.);
/*
 * Advance the frame.
 */
    c_frame();
/*
 *  Deactivate and close workstation, close GKS.
 */
    gdeactivate_ws(WKID);
    gclose_ws(WKID);
    gclose_gks();
}

void drawbx(xcen,ycen,angd,xtra)
float xcen,ycen,angd,xtra;
{
    float dstl, dstr, dstb, dstt;
    float angr, sina, cosa, xfra, yfra, xalb, yalb, xarb, yarb;
    float xart, yart, xalt, yalt;
    c_pcgetr ("DL - DISTANCE LEFT  ",&dstl);
    c_pcgetr ("DR - DISTANCE RIGHT ",&dstr);
    c_pcgetr ("DB - DISTANCE BOTTOM",&dstb);
    c_pcgetr ("DT - DISTANCE TOP   ",&dstt);
    angr=.017453292519943*angd;
    sina=sin(angr);
    cosa=cos(angr);
    xfra=c_cufx(xcen);
    yfra=c_cufy(ycen);
    xalb=xfra-(dstl+xtra)*cosa+(dstb+xtra)*sina;
    yalb=yfra-(dstl+xtra)*sina-(dstb+xtra)*cosa;
    xarb=xfra+(dstr+xtra)*cosa+(dstb+xtra)*sina;
    yarb=yfra+(dstr+xtra)*sina-(dstb+xtra)*cosa;
    xart=xfra+(dstr+xtra)*cosa-(dstt+xtra)*sina;
    yart=yfra+(dstr+xtra)*sina+(dstt+xtra)*cosa;
    xalt=xfra-(dstl+xtra)*cosa-(dstt+xtra)*sina;
    yalt=yfra-(dstl+xtra)*sina+(dstt+xtra)*cosa;
    c_plotif (xalb,yalb,0);
    c_plotif (xarb,yarb,1);
    c_plotif (xart,yart,1);
    c_plotif (xalt,yalt,1);
    c_plotif (xalb,yalb,1);
    c_plotif (0.,0.,2);
	return;
}

