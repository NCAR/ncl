/*
 *  $Id: c_tdshpk.c,v 1.4 1995-06-14 13:59:17 haley Exp $
 */
#include <stdio.h>
#include <math.h>

#include <ncarg/ncargC.h>
#include <ncarg/gks.h>


#define IWTYPE 1
#define WKID   1

main()
{
#ifdef NeedFuncProto
    extern void tdshpk(int *);
#else
    extern void tdshpk();
#endif
    int ierr;
/*
 * Invoke demo driver.
 */
    gopen_gks ("stdout",0);
    gopen_ws (WKID, NULL, IWTYPE);
    gactivate_ws(WKID);

    tdshpk (&ierr);
/*
 * Deactivate and close workstation, close GKS.
 */
    gdeactivate_ws(WKID);
    gclose_ws(WKID);
    gclose_gks();
}

void tdshpk
#ifdef NeedFuncProto
(int *ierr)
#else
(ierr)
int *ierr;
#endif
{
    Gcolr_rep rgb;
    float ycen, angd, rrho, thta;
    int i;
#ifdef NeedFuncProto
    extern int ipkbts(int *,int);
#else
    extern int ipkbts();
#endif
/*
 * Declare arrays in which to put coordinates for calls to DPCURV.
 */ 
    float xcra[1001],ycra[1001];
/*
 * Declare an array in which to define the bits of an integer dash
 * pattern (which saves one from having to do the binary to decimal
 * conversion).
 * 
 * Define the fourteen bits of an integer dash pattern to be used.
 */ 
    int ibts[14] ={1,0,1,1,0,1,1,1,0,1,1,1,1,0};
/*
 * Define a multiplicative constant to get from degrees to radians.
 */ 
    float dtor = .017453292519943;
/*
 * Turn off clipping by GKS.
 */ 
    gset_clip_ind (GIND_NO_CLIP);
/*
 * Define some colors to use.
 */ 
    rgb.rgb.red = 1.; rgb.rgb.green =0.; rgb.rgb.blue = 1.;
    gset_colr_rep(WKID,2,&rgb);
    rgb.rgb.red = 1.; rgb.rgb.green =1.; rgb.rgb.blue = 0.;
    gset_colr_rep(WKID,3,&rgb);
/*
 * Define the mapping from the user system to the fractional system for
 * the first frame.
 */ 
    c_set (.03,.97,.01,.95,-10.,10.,-10.,10.,1);
/*
 * Put a label at the top of the first frame.
 */ 
    c_plchhq (c_cfux(.5),c_cfuy(.975),"DEMONSTRATING THE USE OF DASHPACK - FRAME 1",.015,0.,0.);
/*
 * Use the default character dash pattern to draw a box whose edges
 * are straight lines and put a label in the middle of the box.
 * 
 * Note: At this point, "DPS" = 0, "DPT" = 1111111111111111 (binary)
 * or "$$$$$$$$$$$$$$$$" (character), "LTL" = 0, "MFS" = 1, "PCF" = 0,
 * "SAF" = 360, "SCF" = 0, "SSL" = .01, "TCS" = -1.,
 * "WOC" = .01, "WOG" = .005, and "WOS" = .005.
 */
    ycen=8.5;

    c_dpline (-9.,ycen+.75, 9.,ycen+.75);
    c_dpline ( 9.,ycen+.75, 9.,ycen-.75);
    c_dpline ( 9.,ycen-.75,-9.,ycen-.75);
    c_dpline (-9.,ycen-.75,-9.,ycen+.75);

    c_plchhq (0.,ycen,"A box drawn using DPLINE and the default character dash pattern.",.01,0.,0.);
/*
 * Redefine the character dash pattern, draw a second box whose edges
 * are straight lines, and put a label in the middle of the box.
 * 
 * Note: At this point, "DPS" = 0, "DPT" = 1111111111111111 (binary)
 * or "$$$$$$$$$$$$$$$$" (character), "LTL" = 0, "MFS" = 1, "PCF" = 0,
 * "SAF" = 360, "SCF" = 0, "SSL" = .01, "TCS" = -1.,
 * "WOC" = .01, "WOG" = .005, and "WOS" = .005.
 */
    c_dpsetc ("DPT - DASH PATTERN (CHARACTER)","$$$_$$$A");
/*
 * Note: At this point, "DPS" = 0, "DPT" = 1111111111111111 (binary)
 * or "$$$_$$$A" (character), "LTL" = 0, "MFS" = 1, "PCF" = 0,
 * "SAF" = 360, "SCF" = 0, "SSL" = .01, "TCS" = -1., 
 * "WOC" = .01, "WOG" = .005, and "WOS" = .005.
 */ 
    ycen=6.0;
          
    c_dpline (-9.,ycen+.75, 9.,ycen+.75);
    c_dpline ( 9.,ycen+.75, 9.,ycen-.75);
    c_dpline ( 9.,ycen-.75,-9.,ycen-.75);
    c_dpline (-9.,ycen-.75,-9.,ycen+.75);

    c_plchhq (0.,ycen,"A box drawn using DPLINE and a simple character dash pattern.",.01,0.,0.);
/*
 * Use a 14-bit binary dash pattern to draw a third box whose edges are
 * straight lines and put a label in the middle of the box.  Note that
 * the routine IPKBTS, which packs the bits of the integer dash pattern
 * into an integer variable, is not a part of DASHPACK, but of this
 * example.
 * 
 * Note: At this point, "DPS" = 0, "DPT" = 1111111111111111 (binary)
 * or "$$$_$$$A" (character), "LTL" = 0, "MFS" = 1, "PCF" = 0,
 * "SAF" = 360, "SCF" = 0, "SSL" = .01, "TCS" = -1.,
 * "WOC" = .01, "WOG" = .005, and "WOS" = .005.
 */
    c_dpseti ("DPS - DASH PATTERN SELECTOR",-14);
    c_dpseti ("DPT - DASH PATTERN (BINARY)",ipkbts(ibts,14));
/*
 * Note: At this point, "DPS" = -14, "DPT" = 01011011101111 (binary)
 * or "$$$_$$$A" (character), "LTL" = 0, "MFS" = 1, "PCF" = 0,
 * "SAF" = 360, "SCF" = 0, "SSL" = .01, "TCS" = -1.,
 * "WOC" = .01, "WOG" = .005, and "WOS" = .005.
 */ 
    ycen=3.5;

    c_dpline (-9.,ycen+.75, 9.,ycen+.75);
    c_dpline ( 9.,ycen+.75, 9.,ycen-.75);
    c_dpline ( 9.,ycen-.75,-9.,ycen-.75);
    c_dpline (-9.,ycen-.75,-9.,ycen+.75);

    c_plchhq (0.,ycen,"A box drawn using DPLINE and a 14-bit binary dash pattern.",.01,0.,0.);
/*
 * Draw an oval using DPCURV and a character dash pattern in which there
 * are no breakpoints.  Smoothing is off by default.
 * 
 * Note: At this point, "DPS" = -14, "DPT" = 01011011101111 (binary)
 * or "$$$_$$$A" (character), "LTL" = 0, "MFS" = 1, "PCF" = 0,
 * "SAF" = 360, "SCF" = 0, "SSL" = .01, "TCS" = -1.,
 * "WOC" = .01, "WOG" = .005, and "WOS" = .005.
 */
    c_dpseti ("DPS - DASH PATTERN SELECTOR",0);

    c_dpsetc ("DPT - DASH PATTERN (CHARACTER)","$$$$$$$$$$$$_W/O BREAKPOINTS_");
/*
 * Note: At this point, "DPS" = 0, "DPT" = 01011011101111 (binary)
 * or "$$$$$$$$$$$$_W/O BREAKPOINTS_" (character), "LTL" = 0, "MFS" = 1,
 * "PCF" = 0, "SAF" = 360, "SCF" = 0, "SSL" = .01, "TCS" = -1.,
 * "WOC" = .01, "WOG" = .005, and "WOS" = .005.
 */ 
    ycen=0.5;

    for( i = 0; i < 19; i++ ) {
        angd=20.*(float)i;
        xcra[i]=9.*cos(dtor*angd);
        ycra[i]=.11 * (float)pow(((float)pow(9.,6.)-(float)pow(xcra[i],6.)),(1./6.));
        if (angd > 180.) ycra[i] = -ycra[i];
        ycra[i]=ycen+ycra[i];
    }

    c_dpcurv (xcra,ycra,19);

    c_plchhq (0.,ycen,"An oval drawn using DPCURV, with smoothing off and a:C:dash pattern in which the labels have no breakpoints.",.01,0.,0.);
/*
 * Draw a second oval using DPCURV and a character dash pattern in which
 * there are breakpoints.  Reduce the added space to be left in each
 * label gap.  Smoothing is still off.
 * 
 * Note: At this point, "DPS" = 0, "DPT" = 01011011101111 (binary)
 * or "$$$$$$$$$$$$_W/O BREAKPOINTS_" (character), "LTL" = 0, "MFS" = 1,
 * "PCF" = 0, "SAF" = 360, "SCF" = 0, "SSL" = .01, "TCS" = -1.,
 * "WOC" = .01, "WOG" = .005, and "WOS" = .005.
 */
    c_dpsetc ("DPT - DASH PATTERN (CHARACTER)", "$$$$$$$$$_W|I|T|H| |B|R|E|A|K|P|O|I|N|T|S_");
/*
 * Note: At this point, "DPS" = 0, "DPT" = 01011011101111 (binary)
 * or "$$$$$$$$$_W|I|T|H| |B|R|E|A|K|P|O|I|N|T|S_" (character),
 * "LTL" = 0, "MFS" = 1, "PCF" = 0, "SAF" = 360, "SCF" = 0, "SSL" = .01,
 * "TCS" = -1., "WOC" = .01, "WOG" = .005, and "WOS" = .005.
 */ 
    ycen=-3.5;

    for( i = 0; i < 19; i++ ) {
        angd=20.*(float)i;
        xcra[i]=9.*cos(dtor*angd);
        ycra[i]=.11 * (float)pow(((float)pow(9.,6.)-(float)pow(xcra[i],6.)),(1./6.));
        if (angd > 180.) ycra[i] = -ycra[i];
        ycra[i]=ycen+ycra[i];
    }

    c_dpcurv (xcra,ycra,19);

    c_plchhq (0.,ycen,"An oval drawn using DPCURV, with smoothing off and a dash:C:pattern in which the labels have breakpoints.", .01,0.,0.);
/*
 * Draw a third oval in the same way as the second one, but instead of
 * embedding break characters in the label, turn on the single-character
 * flag.  Turn on the smoother and use even less added space around each
 * piece of the broken label.
 * 
 * Note: At this point, "DPS" = 0, "DPT" = 01011011101111 (binary)
 * or "$$$$$$$$$_W|I|T|H| |B|R|E|A|K|P|O|I|N|T|S_" (character),
 * "LTL" = 0, "MFS" = 1, "PCF" = 0, "SAF" = 360, "SCF" = 0, "SSL" = .01,
 * "TCS" = -1., "WOC" = .01, "WOG" = .005, and "WOS" = .005.
 */
    c_dpsetc ("DPT - DASH PATTERN (CHARACTER)","$$$$$$$$$$$$$$_WITH SINGLE-CHARACTER FLAG SET_");

    c_dpseti ("SCF - SINGLE-CHARACTER FLAG",1);

    c_dpsetr ("TCS - TENSION ON CUBIC SPLINES",2.5);
/*
 * Note: At this point, "DPS" = 0, "DPT" = 01011011101111 (binary)
 * or "$$$$$$$$$$$$$$_WITH SINGLE-CHARACTER FLAG SET_" (character),
 * "LTL" = 0, "MFS" = 1, "PCF" = 0, "SAF" = 360, "SCF" = 1, "SSL" = .01,
 * "TCS" = 2.5, "WOC" = .01, "WOG" = .005, and "WOS" = .005.
 */ 
    ycen=-7.5;

    for( i = 0; i < 19; i++ ) {
        angd=20.*(float)i;
        xcra[i]=9.*cos(dtor*angd);
        ycra[i]=.11 * (float)pow(((float)pow(9.,6.)-(float)pow(xcra[i],6.)),(1./6.));
        if (angd > 180.) ycra[i] = -ycra[i];
        ycra[i]=ycen+ycra[i];
    }

    c_dpcurv (xcra,ycra,19);

    c_plchhq (0.,ycen,"An oval drawn using DPCURV, with smoothing on and the single-:C:character flag set to create many breakpoints.",.01,0.,0.);
/*
 * Advance the frame.
 */ 
    c_frame();
/*
 * Put a label at the top of the second frame.
 */ 
    c_plchhq (c_cfux(.5),c_cfuy(.975), "DEMONSTRATING THE USE OF DASHPACK - FRAME 2",.015,0.,0.);
/*
 * Define the mapping from the user system to the fractional system in
 * such a way as to use only the upper left quadrant of the frame.
 */ 
    c_set (.030,.485,.495,.950,-10.,10.,-10.,10.,1);
/*
 * Use DPFRST, DPVECT, and DPCURV to draw a spiral.  The label will
 * follow the curve, because the single-character flag is still on, and
 * the curve will be smoothed, because the smoother is still turned on.
 * The additional space around the label pieces is reduced even more.
 * 
 * Note: At this point, "DPS" = 0, "DPT" = 01011011101111 (binary)
 * or "$$$$$$$$$$$$$$_WITH SINGLE-CHARACTER FLAG SET_" (character),
 * "LTL" = 0, "MFS" = 1, "PCF" = 0, "SAF" = 360, "SCF" = 1, "SSL" = .01,
 * "TCS" = 2.5, "WOC" = .01, "WOG" = .005, and "WOS" = .005.
 */
    c_dpsetc ("DPT - DASH PATTERN (CHARACTER)", "$$$$$$$$$$$$$_A SPIRAL DRAWN USING DPFRST/VECT/LAST_");
/*
 * Note: At this point, "DPS" = 0, "DPT" = 01011011101111 (binary)
 * or "$$$$$$$$$$$$$_A SPIRAL DRAWN USING DPFRST/VECT/LAST_" (character),
 * "LTL" = 0, "MFS" = 1, "PCF" = 0, "SAF" = 360, "SCF" = 1, "SSL" = .01,
 * "TCS" = 2.5, "WOC" = .01, "WOG" = .005, and "WOS" = .005.
 */ 
    c_dpfrst (0.,0.);

    for( i=2; i <= 101; i++ ) {
        rrho=8.*(float)(i-1)/100.;
        thta=8.*(float)(i-1)/50.;
        c_dpvect(rrho*cos(thta),rrho*sin(thta));
    }

    c_dplast();

/*
 * Put a small label below the spiral.
 */ 
    c_plchhq (0.,-9.,"Using DPFRST, DPVECT, and DPLAST.",.01,0.,0.);
/*
 * Define the mapping from the user system to the fractional system in
 * such a way as to use only the upper right quadrant of the frame.
 */ 
    c_set (.515,.970,.495,.950,-10.,10.,-10.,10.,1);
/*
 * Use DPDRAW to draw another spiral.  Note that the single-character
 * flag is still on, so the label follows the curve.  Note also that,
 * even though the smoother is still on, it has no effect on DPDRAW.
 * 
 * Note: At this point, "DPS" = 0, "DPT" = 01011011101111 (binary)
 * or "$$$$$$$$$$$$$_A SPIRAL DRAWN USING DPFRST/VECT/LAST_" (character),
 * "LTL" = 0, "MFS" = 1, "PCF" = 0, "SAF" = 360, "SCF" = 1, "SSL" = .01,
 * "TCS" = 2.5, "WOC" = .01, "WOG" = .005, and "WOS" = .005.
 */
    c_dpsetc ("DPT - DASH PATTERN (CHARACTER)", "$_$_$_$_$_$_$_$_$_$_A SPIRAL DRAWN USING DPDRAW_");
/*
 * Note: At this point, "DPS" = 0, "DPT" = 01011011101111 (binary)
 * or "$_$_$_$_$_$_$_$_$_$_A SPIRAL DRAWN USING DPDRAW_" (character),
 * "LTL" = 0, "MFS" = 1, "PCF" = 0, "SAF" = 360, "SCF" = 1, "SSL" = .01,
 * "TCS" = 2.5, "WOC" = .01, "WOG" = .005, and "WOS" = .005.
 */ 
    c_dpdraw (c_cufx(0.),c_cufy(0.),0);

    for( i=2; i <= 101; i++ ) {
        rrho=8.*(float)(i-1)/100.;
        thta=6.28318530717958*(float)(i-1)/20.;
        c_dpdraw (c_cufx(rrho*cos(thta)),c_cufy(rrho*sin(thta)),1);
    }

    c_dpdraw (0.,0.,2);
/*
 * Put a small label below the spiral.
 */ 
    c_plchhq (0.,-9.,"Using DPDRAW.",.01,0.,0.);
/*
 * Define the mapping from the user system to the fractional system in
 * such a way as to use only the lower left quadrant of the frame.
 */ 
    c_set (.030,.485,.010,.455,-10.,10.,-10.,10.,1);
/*
 * Use DPSMTH to draw the same spiral.  The single-character flag is
 * still on and smoothing is still on.
 * 
 * Note: At this point, "DPS" = 0, "DPT" = 01011011101111 (binary)
 * or "$_$_$_$_$_$_$_$_$_$_A SPIRAL DRAWN USING DPDRAW_" (character),
 * "LTL" = 0, "MFS" = 1, "PCF" = 0, "SAF" = 360, "SCF" = 1, "SSL" = .01,
 * "TCS" = 2.5, "WOC" = .01, "WOG" = .005, and "WOS" = .005.
 */
    c_dpsetc ("DPT - DASH PATTERN (CHARACTER)", "$_$_$_$_$_$_$_$_$_$_A SPIRAL DRAWN USING DPSMTH_");
/*
 * Note: At this point, "DPS" = 0, "DPT" = 01011011101111 (binary)
 * or "$_$_$_$_$_$_$_$_$_$_A SPIRAL DRAWN USING DPSMTH_" (character),
 * "LTL" = 0, "MFS" = 1, "PCF" = 0, "SAF" = 360, "SCF" = 1, "SSL" = .01,
 * "TCS" = 2.5, "WOC" = .01, "WOG" = .005, and "WOS" = .005.
 */ 
    c_dpsmth (c_cufx(0.),c_cufy(0.),0);

    for( i=2; i <= 101; i++ ) {
        rrho=8.*(float)(i-1)/100.;
        thta=6.28318530717958*(float)(i-1)/20.;
        c_dpsmth (c_cufx(rrho*cos(thta)),c_cufy(rrho*sin(thta)),1);
    }

    c_dpsmth (0.,0.,2);
/*
 * Put a small label below the spiral.
 */ 
    c_plchhq (0.,-9.,"Using DPSMTH.",.01,0.,0.);
/*
 * Define the mapping from the user system to the fractional system in
 * such a way as to use only the lower right quadrant of the frame.
 */ 
    c_set (.515,.970,.010,.455,-10.,10.,-10.,10.,1);
/*
 * Use DPSMTH to draw another spiral.  This time, use PLCHHQ function
 * codes in the label string and use color to distinguish the label
 * from the line.
 * 
 * Note: At this point, "DPS" = 0, "DPT" = 01011011101111 (binary)
 * or "$_$_$_$_$_$_$_$_$_$_A SPIRAL DRAWN USING DPSMTH_" (character),
 * "LTL" = 0, "MFS" = 1, "PCF" = 0, "SAF" = 360, "SCF" = 1, "SSL" = .01,
 * "TCS" = 2.5, "WOC" = .01, "WOG" = .005, and "WOS" = .005.
 */
    c_dpsetc ("DPT - DASH PATTERN (CHARACTER)","$$$$$$$$$$$$$$$$$$$$$$C|o|n|t|o|u|r| |l|e|v|e|l| |=| |1|3|.|6|2|:L1:4|1|0:S:14:N:");

    c_dpseti ("LTL - LINE-THROUGH-LABEL FLAG",1);

    c_dpseti ("SCF - SINGLE-CHARACTER FLAG",0);

    c_dpsetr ("SSL - SMOOTHED SEGMENT LENGTH",.001);
/*
 * Note: At this point, "DPS" = 0, "DPT" = 01011011101111 (binary)
 * or "$$$$$$$$$$$$$$$$$$$$$$C|o|n|t|o|u|r| |l|e|v|e|l| |=| |1|3|.|6|2|:L
 * 1:4|1|0:S:14:N:" (character), "LTL" = 1, "MFS" = 1, "PCF" = 0,
 * "SAF" = 360, "SCF" = 0, "SSL" = .001, "TCS" = 2.5,
 * "WOC" = .01, "WOG" = .005, and "WOS" = .005.
 */ 
    gset_line_colr_ind (2);
    c_pcseti ("CC - CHARACTER COLOR",3);

    c_dpsmth (c_cufx(0.),c_cufy(0.),0);

    for( i=2; i <= 101; i++ ) {
        rrho=8.*(float)(i-1)/100.;
        thta=6.28318530717958*(float)(i-1)/15.;
        c_dpsmth (c_cufx(rrho*cos(thta)),c_cufy(rrho*sin(thta)),1);
    }

    c_dpsmth (0.,0.,2);
/*
 * Put a small label below the spiral.
 */ 
    gset_line_colr_ind (1);
    c_pcseti ("CC - CHARACTER COLOR",1);

    c_plchhq (0.,-9.,"Using PLCHHQ function codes and colors.", .01,0.,0.);
/*
 * Advance the frame.
 */ 
    c_frame();
/*
 * Put a label at the top of the third frame.
 */ 
    c_plchhq (c_cfux(.5),c_cfuy(.975), "DEMONSTRATING THE USE OF DASHPACK - FRAME 3",.015,0.,0.);
/*
 * Define the mapping from the user system to the fractional system in
 * such a way as to use only the upper left quadrant of the frame.
 */ 
    c_set (.030,.485,.495,.950,-10.,10.,-10.,10.,1);
/*
 * Use DPSMTH to draw a spiral.  Use color to distinguish the labels
 * from the line, and orient all the labels horizontally.
 * 
 * Note: At this point, "DPS" = 0, "DPT" = 01011011101111 (binary)
 * or "$$$$$$$$$$$$$$$$$$$$$$C|o|n|t|o|u|r| |l|e|v|e|l| |=| |1|3|.|6|2|:L
 * 1:4|1|0:S:14:N:" (character), "LTL" = 1, "MFS" = 1, "PCF" = 0,
 * "SAF" = 360, "SCF" = 0, "SSL" = .001, "TCS" = 2.5,
 * "WOC" = .01, "WOG" = .005, and "WOS" = .005.
 */
    c_dpsetc ("DPT - DASH PATTERN (CHARACTER)","$$$$$$$$SPIRAL");

    c_dpseti ("SAF - STRING-ANGLE FLAG",-360);
/*
 * Note: At this point, "DPS" = 0, "DPT" = 01011011101111 (binary)
 * or "$$$$$$$$SPIRAL" (character), "LTL" = 1, "MFS" = 1, "PCF" = 0,
 * "SAF" = -360, "SCF" = 0, "SSL" = .001, "TCS" = 2.5,
 * "WOC" = .01, "WOG" = .005, and "WOS" = .005.
 */ 
    gset_line_colr_ind (2);
    c_pcseti ("CC - CHARACTER COLOR",3);

    c_dpsmth (c_cufx(0.),c_cufy(0.),0);

    for( i=2; i <= 101; i++ ) {
        rrho=8.*(float)(i-1)/100.;
        thta=8.*(float)(i-1)/50.;
        c_dpsmth (c_cufx(rrho*cos(thta)),c_cufy(rrho*sin(thta)),1);
    }

    c_dpsmth (0.,0.,2);
/*
 * Put a small label below the spiral.
 */ 
    gset_line_colr_ind (1);

    c_pcseti ("CC - CHARACTER COLOR",1);

    c_plchhq (0.,-9.,"Using horizontal labels.",.01,0.,0.);
/*
 * Define the mapping from the user system to the fractional system in
 * such a way as to use only the upper right quadrant of the frame.
 */ 
    c_set (.515,.970,.495,.950,-10.,10.,-10.,10.,1);
/*
 * Use DPDRAW to draw a spiral.
 * 
 * Note: At this point, "DPS" = 0, "DPT" = 01011011101111 (binary)
 * or "$$$$$$$$SPIRAL" (character), "LTL" = 1, "MFS" = 1, "PCF" = 0,
 * "SAF" = -360, "SCF" = 0, "SSL" = .001, "TCS" = 2.5,
 * "WOC" = .01, "WOG" = .005, and "WOS" = .005.
 */
    c_dpsetc ("DPT - DASH PATTERN (CHARACTER)","$$$$$$$_SPIRAL_");

    c_dpseti ("LTL - LINE-THROUGH-LABEL FLAG",0);

    c_dpseti ("PCF - PLOTCHAR FLAG",1);

    c_dpseti ("SAF - STRING-ANGLE FLAG",360);

    c_dpseti ("SCF - SINGLE-CHARACTER FLAG",1);
/*
 * Note: At this point, "DPS" = 0, "DPT" = 01011011101111 (binary)
 * or "$$$$$$$_SPIRAL_" (character), "LTL" = 0, "MFS" = 1, "PCF" = 1,
 * "SAF" = 360, "SCF" = 1, "SSL" = .001, "TCS" = 2.5,
 * "WOC" = .01, "WOG" = .005, and "WOS" = .005.
 */ 
    c_dpdraw (c_cufx(0.),c_cufy(0.),0);

    for( i=2; i <= 101; i++ ) {
        rrho=8.*(float)(i-1)/100.;
        thta=8.*(float)(i-1)/50.;
        c_dpdraw (c_cufx(rrho*cos(thta)),c_cufy(rrho*sin(thta)),1);
    }

    c_dpdraw (0.,0.,2);
/*
 * Put a small label below the spiral.
 */ 
    c_plchhq (0.,-9.,"Using PLCHMQ instead of PLCHHQ.",.01,0.,0.);
/*
 * Define the mapping from the user system to the fractional system in
 * such a way as to use only the lower left quadrant of the frame.
 */ 
    c_set (.030,.485,.010,.455,-10.,10.,-10.,10.,1);
/*
 * Use DPDRAW to draw a spiral.
 * 
 * Note: At this point, "DPS" = 0, "DPT" = 01011011101111 (binary)
 * or "$$$$$$$_SPIRAL_" (character), "LTL" = 0, "MFS" = 1, "PCF" = 1,
 * "SAF" = 360, "SCF" = 1, "SSL" = .001, "TCS" = 2.5,
 * "WOC" = .01, "WOG" = .005, and "WOS" = .005.
 */
    c_dpsetc ("DPT - DASH PATTERN (CHARACTER)","$_$_$_$_SPIRAL_");

    c_dpseti ("PCF - PLOTCHAR FLAG",0);

    c_dpsetr ("WOC - WIDTH OF CHARACTERS",.02);

    c_dpsetr ("WOG - WIDTH OF GAP",.01);

    c_dpsetr ("WOS - WIDTH OF SOLID",.01);
/*
 * Note: At this point, "DPS" = 0, "DPT" = 01011011101111 (binary)
 * or "$_$_$_$_SPIRAL_" (character), "LTL" = 0, "MFS" = 1, "PCF" = 0,
 * "SAF" = 360, "SCF" = 1, "SSL" = .001, "TCS" = 2.5,
 * "WOC" = .02, "WOG" = .01, and "WOS" = .01.
 */ 
    c_dpdraw (c_cufx(0.),c_cufy(0.),0);

    for( i=2; i <= 101; i++ ) {
        rrho=8.*(float)(i-1)/100.;
        thta=8.*(float)(i-1)/50.;
        c_dpdraw (c_cufx(rrho*cos(thta)),c_cufy(rrho*sin(thta)),1);
    }

    c_dpdraw (0.,0.,2);
/*
 * Put a small label below the spiral.
 */ 
    c_plchhq (0.,-9.,"Changing character and solid/gap sizes.",.01,0.,0.);
/*
 * Define the mapping from the user system to the fractional system in
 * such a way as to use only the lower right quadrant of the frame.
 */ 
    c_set (.515,.970,.010,.455,-10.,10.,-10.,10.,1);
/*
 * Use DPDRAW to draw two spirals and then use "MFS" to offset one.
 * 
 * Note: At this point, "DPS" = 0, "DPT" = 01011011101111 (binary)
 * or "$_$_$_$_SPIRAL_" (character), "LTL" = 0, "MFS" = 1, "PCF" = 0,
 * "SAF" = 360, "SCF" = 1, "SSL" = .001, "TCS" = 2.5,
 * "WOC" = .02, "WOG" = .01, and "WOS" = .01.
 */
    c_dpsetr ("WOC - WIDTH OF CHARACTERS",.008);

    c_dpsetr ("WOG - WIDTH OF GAP",.008);

    c_dpsetr ("WOS - WIDTH OF SOLID",.008);
/*
 * Note: At this point, "DPS" = 0, "DPT" = 01011011101111 (binary)
 * or "$_$_$_$_SPIRAL_" (character), "LTL" = 0, "MFS" = 1, "PCF" = 0,
 * "SAF" = 360, "SCF" = 1, "SSL" = .001, "TCS" = 2.5, 
 * "WOC" = .008, "WOG" = .008, and "WOS" = .008.
 *
 * Draw the first spiral.
 */
    c_dpsetc ("DPT - DASH PATTERN (CHARACTER)","$$$$$_SPIRAL 1_");

    c_dpsetr ("MFS - MULTIPLIER FOR FIRST SOLID",1.5);
/*
 * Note: At this point, "DPS" = 0, "DPT" = 01011011101111 (binary)
 * or "$$$$$_SPIRAL 1_" (character), "LTL" = 0, "MFS" = 1.5, "PCF" = 0,
 * "SAF" = 360, "SCF" = 1, "SSL" = .001, "TCS" = 2.5,
 * "WOC" = .008, "WOG" = .008, and "WOS" = .008.
 */ 
    c_dpdraw (c_cufx(0.),c_cufy(0.),0);

    for( i=2; i <= 101; i++ ) {
        rrho=6.*(float)(i-1)/100.;
        thta=8.*(float)(i-1)/50.;
        c_dpdraw (c_cufx(rrho*cos(thta)),c_cufy(rrho*sin(thta)),1);
    }

    c_dpdraw (0.,0.,2);
/*
 * Draw the second spiral.
 */ 
    c_dpsetc ("DPT - DASH PATTERN (CHARACTER)","$$$$$_SPIRAL 2_");

    c_dpsetr ("MFS - MULTIPLIER FOR FIRST SOLID",3.);
/*
 * Note: At this point, "DPS" = 0, "DPT" = 01011011101111 (binary)
 * or "$$$$$_SPIRAL 2_" (character), "LTL" = 0, "MFS" = 3., "PCF" = 0,
 * "SAF" = 360, "SCF" = 1, "SSL" = .001, "TCS" = 2.5,
 * "WOC" = .008, "WOG" = .008, and "WOS" = .008.
 */ 
    c_dpdraw (c_cufx(0.),c_cufy(0.),0);

    for( i=2; i <= 101; i++ ) {
        rrho=8.*(float)(i-1)/100.;
        thta=8.*(float)(i-1)/50.;
        c_dpdraw (c_cufx(rrho*cos(thta)),c_cufy(rrho*sin(thta)),1);
    }

    c_dpdraw (0.,0.,2);
/*
 * Put a small label below the spirals.
 */ 
    c_plchhq (0.,-9.,"Using the first-solid multiplier.",.01,0.,0.);
/*
 * Advance the frame.
 */ 
    c_frame();
/*
 * Done.
 */ 
    printf( "DASHPACK TEST EXECUTED OKAY - SEE PLOTS TO CERTIFY\n");

    *ierr=0;
    return;
}


/*
 * This value of this function, when given an array of NBTS 0s and 1s,
 * is the integer resulting from packing those bits together, to be
 * used as an integer dash pattern.
 */ 
int ipkbts
#ifdef NeedFuncProto
(int *ibts,int nbts)
#else
(ibts,nbts)
int *ibts, nbts;
#endif
{
    int i, j, k;
    int ii, jj, kk;
    int ival;
    extern int NGCALLF(iand,IAND)(), NGCALLF(ior,IOR)(), NGCALLF(ishift,ISHIFT)();
    ival = 0;
/*
 * One at a time, shift the bits by the proper amount and "or" them into
 * the value of the function, making sure to use only the lowest-order
 * bit of each incoming array element.
 */ 
    j = 1;
    for( i=1; i <= nbts; i++ ) {
        ii = NGCALLF(iand,IAND)(&ibts[i-1],&j);
        k = nbts-i;
        kk = NGCALLF(ishift,ISHIFT)(&ii,&k);
        ival = NGCALLF(ior,IOR)(&ival,&kk);
    }
/*
 * Done.
 */ 
    return(ival);
}

