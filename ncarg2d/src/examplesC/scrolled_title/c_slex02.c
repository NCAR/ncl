/*
 * $Id: c_slex02.c,v 1.1 1996-10-18 14:43:14 haley Exp $
 */
#include <stdio.h>
#include <math.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <sys/types.h>

/*
 * Include function prototypes
 */
#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

/*
 * This program illustrates some features of stitle that have not been
 * covered elsewhere and, in particular, some new features that went
 * into the code in July, 1995.
 *
 * The C example doesn't create the file 'slex02.input' like the Fortran
 * example does, so you need to create 'slex02.input' yourself. It should
 * contain the following 7 lines (starting with the line containing the '3'):
 *
    3   2.  .75                                                
Stationary titles can be created by calling FTITLE.            
This is the first example of such a title.                     
Notice how it fades in and fades out.                          
    2   2.  .75                                                
This is another stationary title.                              
It was created during the same call to FTITLE as the first one.
 *
 */

#define WSTYPE SED_WSTYPE
#define WKID   1

int ibkd;

/*
 * Define an array in which to put "card images".
 *
 * Set up the CRDS array as expected by STITLE.  Each "card" contains
 * an X position on the scroll, a Y position on the scroll, a color
 * index, a horizontal centering option, a character size, and text.
 * In this example, all the X-coordinates are set to 512, and the
 * horizontal centering is always set to 1, so each line of text will
 * be centered horizontally.  When the color index is omitted, the
 * assumed value is that of the internal parameter 'FGC'.
 */
char *crds[11] = {"  512  825  2 1  1.2PLOTCHAR",
                  "  512  750    1  1.2can be",
                  "  512  675    1  1.2used to",
                  "  512  600    1  1.2write",
                  "  512  525    1  1.2an equation",
                  "  512  450    1  1.2like",
                  "  512  375  3 1  2.5C:S:2:N:=A:S:2:N:+B:S:2:N:",
                  "  512  300    1  1.2in a",
                  "  512  225    1  1.2movie title",
                  "  512  150    1  1.2created by",
                  "  512   75  2 1  1.2STITLE"};

main()
{
    int mtst, ncds, iyst, iynd;
    float tmst, tmmv, tmnd;
/*
 * Open GKS.
 */
    gopen_gks("stdout",0);
    gopen_ws(WKID, NULL, WSTYPE);
    gactivate_ws(WKID);
/*
 * Set the argument that will be used to tell FTITLE and STITLE whether
 * a real run (MTST = 0) or a test run (MTST = 1) is being done.  You
 * should change this and rerun to see the effect on the output.
 */
    mtst=0;
/*
 * Tell STITLE to reset itself.  (At this point, since no internal
 * parameters of STITLE have been changed, this call effectively does
 * nothing; it's here just to remind the reader that SLRSET exists).
 */
    c_slrset();
/*
 * Tell STITLE to output only two frames per second.  (Normally, one
 * uses either 24 or 30 frames per second; using 2 in this example is
 * just a good way to hold down the length of the metafile produced.)
 */
    c_slseti ("NFS - NUMBER OF FRAMES PER SECOND",2);
/*
 * Turn on fade-in and fade-out by telling STITLE how many seconds of
 * each to do.  (By default, fade-in and fade-out are turned off.)
 */
    c_slsetr ("FIN - FADE-IN TIME",2.);
    c_slsetr ("FOU - FADE-OUT TIME",2.);
/*
 * Set some parameters to their default values.  (These calls are here
 * just to remind the reader of the existence of the parameters.)
 *
 * Tell STITLE not to output alignment frames.  These were mostly for
 * use in the old days of real 35-millimeter film and are probably not
 * useful now.
 */
    c_slseti ("ALN - ALIGNMENT FLAG",0);
/*
 * Tell STITLE what vertical spacing to use between test frames that
 * represent the scrolled portion of output.  The effect of changing
 * this will be seen only when MTST = 1.
 */
    c_slseti ("INC - INCREMENT BETWEEN TEST FRAMES",300);
/*
 * Tell STITLE what logical unit to use for WISS (Workstation-Independent
 * Segment Storage).
 */
    c_slseti ("LOG - LOGICAL UNIT FOR WISS",4);
/*
 * Tell STITLE what to use for the X coordinates at the start and end of
 * the scrolling phase.  It is not clear that these are good for much;
 * one possible use might be to set both to a value other than 512 in
 * order to get the lines of text centered about a particular vertical
 * line.  Values used have to be between 1 and 1024.
 */
    c_slseti ("NXS - X COORDINATE AT START OF SCROLL",512);
    c_slseti ("NXE - X COORDINATE AT END OF SCROLL",512);
/*
 * Tell STITLE what "principal size" to use for characters.  The value
 * is given in terms of a 1024x1024 plotter window.  When the input to
 * FTITLE or STITLE says to use a "SIZE" of 1., what is meant is a
 * height of "PSZ"/1023. fractional units (NDCs).  Similarly, a "SIZE"
 * of 1.25 means a height of 1.25*"PSZ"/1023. fractional units (NDCs).
 */
    c_slseti ("PSZ - PRINCIPAL SIZE",21);
/*
 * Define the STITLE viewport.  Characters are clipped at the edges of
 * this viewport and, when "BGC" is set non-zero, STITLE fills it with
 * the background color defined by "BGC" (instead of depending on GKS
 * to supply the background fill).  These parameters should be used in
 * lieu of the old ones named "LX1", "LX2", "LY1", and "LY2".
 */
    c_slsetr ("VPL - VIEWPORT LEFT",0.);
    c_slsetr ("VPR - VIEWPORT RIGHT",1.);
    c_slsetr ("VPB - VIEWPORT BOTTOM",0.);
    c_slsetr ("VPT - VIEWPORT TOP",1.);
/*
 * Turn off the flag that tells the version of SLUBKG included below
 * to draw a background.  This flag will be turned on for an STITLE
 * example during which the background is appropriate for the title
 * being scrolled by.
 */
    ibkd=0;
/*
 * BEGIN SIMPLE FTITLE EXAMPLE -----------------------------------------
 *
 * Tell FTITLE what "gap size" to use for the vertical spacing between
 * lines.  The value is given in terms of a 1024x1024 plotter window.
 */
    c_slseti ("GSZ - GAP SIZE",40);
/*
 * Set the centering option for FTITLE.  You can set this to 0 to get
 * left-justified titles, to 1 to get centered titles, or to 2 to get
 * right-justified titles.
 */
    c_slseti ("ICO - INTEGER CENTERING OPTION",1);
/*
 * Tell FTITLE how many seconds worth of blank frames to put before
 * the first title frame ("TM1"), after each title frame except the last
 * ("TM2"), and after the last title frame ("TM2"+"TM3").  Note that
 * "TM3" can be negative to make the amount of time after the last
 * title frame less than that between title frames.
 */
    c_slsetr ("TM1 - TIME 1 (BEFORE FIRST TITLE)",1.);
    c_slsetr ("TM2 - TIME 2 (BETWEEN PAIRS OF TITLES)",.5);
    c_slsetr ("TM3 - TIME 3 (EXTRA TIME AFTER LAST TITLE)",.5);
/*
 * Call FTITLE to write out the titles defined by the card input.
 */
    c_ftitle (mtst);
/*
 * END OF SIMPLE FTITLE EXAMPLE, BEGIN SIMPLE STITLE EXAMPLE -----------
 */
    ncds=11;
/*
 * Define the remaining inputs for STITLE.  (Note that there is no
 * reason not to put constant values directly in an STITLE call;
 * defining each variable is done for purposes of clarity in the
 * example.)
 *
 * IYST specifies the Y-coordinate that will be at the vertical center
 * of the frame when scrolling begins.
 */
    iyst=825;
/*
 * IYND specifies the Y-coordinate that will be at the vertical center
 * of the frame when scrolling is terminated.
 */
    iynd=0;
/*
 * TMST specifies how many seconds the first frame will remain
 * stationary before scrolling begins.
 */
    tmst=1.;
/*
 * Specify the scroll time.  This is the time in seconds that the
 * text will be scrolled from position IYST to IYND.
 */
    tmmv=5.;
/*
 * Specify how many seconds the final frame will remain stationary
 * after scrolling stops.
 */
    tmnd=1.;
/*
 * Call STITLE, putting 1-second gaps before and after its output.
 */
    c_slogap (1.,mtst);
    c_stitle (crds,ncds,iyst,iynd,tmst,tmmv,tmnd,mtst);
    c_slogap (1.,mtst);
/*
 * End of SIMPLE STITLE EXAMPLE, BEGIN FANCIER STITLE EXAMPLE ----------
 *
 * Tell PLOTCHAR to use font 25 (one of the filled fonts) by default.
 */
    c_pcseti ("FN",25);
/*
 * Define the color with index "BGC" (the default background color).
 */
    c_slsetr ("BGR - BACKGROUND COLOR, RED COMPONENT"  ,1.);
    c_slsetr ("BGG - BACKGROUND COLOR, GREEN COMPONENT",1.);
    c_slsetr ("BGB - BACKGROUND COLOR, BLUE COMPONENT" ,1.);
/*
 * Define the color with index "FGC" (the default foreground color).
 */
    c_slsetr ("FGR - FOREGROUND COLOR, RED COMPONENT"  ,0.);
    c_slsetr ("FGG - FOREGROUND COLOR, GREEN COMPONENT",.7);
    c_slsetr ("FGB - FOREGROUND COLOR, BLUE COMPONENT" ,0.);
/*
 * Define the color with index 2 (for the words "PLOTCHAR" and "STITLE").
 */
    c_slsetr ("FGR(2) - FOREGROUND COLOR, RED COMPONENT"  ,1.);
    c_slsetr ("FGG(2) - FOREGROUND COLOR, GREEN COMPONENT",0.);
    c_slsetr ("FGB(2) - FOREGROUND COLOR, BLUE COMPONENT" ,0.);
/*
 * Define the color with index 3 (for the equation).
 */
    c_slsetr ("FGR(3) - FOREGROUND COLOR, RED COMPONENT"  ,0.);
    c_slsetr ("FGG(3) - FOREGROUND COLOR, GREEN COMPONENT",0.);
    c_slsetr ("FGB(3) - FOREGROUND COLOR, BLUE COMPONENT" ,1.);
/*
 * Define the color with index 4 (for the background drawing).
 */
    c_slsetr ("FGR(4) - FOREGROUND COLOR, RED COMPONENT"  ,.9);
    c_slsetr ("FGG(4) - FOREGROUND COLOR, GREEN COMPONENT",.9);
    c_slsetr ("FGB(4) - FOREGROUND COLOR, BLUE COMPONENT" ,.9);
/*
 * Tell STITLE what kind of fade-in and fade-out to use.  In both cases,
 * you can use -2 for a fade from/to black in the HSV system, -1 for no
 * fade at all, and "n" greater than or equal to 0 for a fade-in from
 * the color with color index "ni" = n/1000 and a fade-out to the color
 * with color index "no" = MOD[n,1000], in the RGB system (in which case,
 * "ni" = 999 requests no fade-in and "no" = 999 requests no fade-out.)
 */
    c_slseti ("BGF - BACKGROUND FADE-IN/OUT TYPE",0);
    c_slseti ("FGF - FOREGROUND FADE-IN/OUT TYPE",0);
/*
 * Turn on the flag that tells the version of SLUBKG included below
 * to draw a background.
 */
    ibkd=1;
/*
 * Call STITLE again, putting 1-second gaps before and after its output.
 */
    c_slogap (1.,mtst);
    c_stitle (crds,ncds,iyst,iynd,tmst,tmmv,tmnd,mtst);
    c_slogap (1.,mtst);
/*
 * End of FANCIER STITLE EXAMPLE ---------------------------------------
 *
 * Close GKS.
 */
    gdeactivate_ws(WKID);
    gclose_ws(WKID);
    gclose_gks();
}

/*
 * Define some transformations to put the drawing where it looks best.
 */
#define trnx(x) (.4+x/16.)
#define trny(y) (.4+y/16.)

void NGCALLF(slubkg,SLUBKG) (ipoc)
int *ipoc;
{
    int ierr, ispl, iscc;
    Gdouble wdth;
/*
 * This version of SLUBKG replaces the dummy one in STITLE.  When the
 * flag IBKD in the common block CMUSER is turned on, this routine draws
 * an appropriate background for the label that is being scrolled by.
 *
 * IPOC says what is going on in STITLE at the time that SLUBKG is
 * called, as follows:
 *
 * IPOC  Position of call to SLUBKG
 * ----  ---------------------------------------------------------------
 *  -1   Just before drawing titles on a "fade-in" frame.
 *  +1   Just after drawing titles on a "fade-in" frame.
 *  -2   Just before drawing titles on a "start" frame.
 *  +2   Just after drawing titles on a "start" frame.
 *  -3   Just before drawing titles on a "move" frame.
 *  +3   Just after drawing titles on a "move" frame.
 *  -4   Just before drawing titles on an "end" frame.
 *  +4   Just after drawing titles on an "end" frame.
 *  -5   Just before drawing titles on a "fade-out" frame.
 *  +5   Just after drawing titles on a "fade-out" frame.
 *
 * Draw a background if and only if IPOC is negative and IBKD is not
 * zero.  Note that we are careful to save and restore the state of
 * anything we change in GKS and in PLOTCHAR.
 */
    if (*ipoc < 0 && ibkd != 0) {
        c_plotif (0.,0.,2);
        ginq_line_colr_ind (&ierr,&ispl);
        gset_line_colr_ind (4);
        ginq_linewidth (&ierr,&wdth);
        gset_linewidth (4.);
        c_plotif (trnx( 0.),trny( 0.),0);
        c_plotif (trnx(-3.),trny( 0.),1);
        c_plotif (trnx(-3.),trny( 3.),1);
        c_plotif (trnx( 0.),trny( 3.),1);
        c_plotif (trnx( 0.),trny(-4.),1);
        c_plotif (trnx( 4.),trny(-4.),1);
        c_plotif (trnx( 4.),trny( 0.),1);
        c_plotif (trnx( 0.),trny( 3.),1);
        c_plotif (trnx( 3.),trny( 7.),1);
        c_plotif (trnx( 7.),trny( 4.),1);
        c_plotif (trnx( 4.),trny( 0.),1);
        c_plotif (trnx( 0.),trny( 0.),1);
        c_plotif (0.,0.,2);
        gset_line_colr_ind (ispl);
        gset_linewidth (wdth);
        c_pcgeti ("cc",&iscc);
        c_pcseti ("cc",4);
        c_plchhq (trnx(-1.5),trny( 1.5),"A:S:2",.05,0.,0.);
        c_plchhq (trnx( 2.0),trny(-2.0),"B:S:2",.05,0.,0.);
        c_plchhq (trnx( 3.5),trny( 3.5),"C:S:2",.05,0.,0.);
        c_pcseti ("cc",iscc);
    }
/*
 * done.
 */
    return;
}
