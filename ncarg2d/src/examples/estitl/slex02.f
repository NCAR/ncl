
      PROGRAM SLEX02
C
C This program illustrates some features of STITLE that have not been
C covered elsewhere and, in particular, some new features that went
C into the code in July, 1995.
C
C Define the error file, the Fortran unit number, the workstation type,
C and the workstation ID to be used in calls to GKS routines.
C
C       PARAMETER (IERRF=6, LUNIT=2, IWTYPE=1,  IWKID=1)   ! NCGM
C       PARAMETER (IERRF=6, LUNIT=2, IWTYPE=8,  IWKID=1)   ! X Windows
C       PARAMETER (IERRF=6, LUNIT=2, IWTYPE=11, IWKID=1)   ! PDF
C       PARAMETER (IERRF=6, LUNIT=2, IWTYPE=20, IWKID=1)   ! PostScript
C
        PARAMETER (IERRF=6, LUNIT=2, IWTYPE=1,  IWKID=1)
C
C Declare a common block for communication with the routine SLUBKG.
C
        COMMON /CMUSER/ IBKD
C
C Define an array in which to put "card images".
C
        CHARACTER*80 CRDS(11)
C
C Open GKS.
C
        CALL GOPKS (IERRF,ISZDM)
        CALL GOPWK (IWKID,LUNIT,IWTYPE)
        CALL GACWK (IWKID)
C
C Set the argument that will be used to tell FTITLE and STITLE whether
C a real run (MTST = 0) or a test run (MTST = 1) is being done.  You
C should change this and rerun to see the effect on the output.
C
        MTST=0
C
C Tell STITLE to reset itself.  (At this point, since no internal
C parameters of STITLE have been changed, this call effectively does
C nothing; it's here just to remind the reader that SLRSET exists).
C
        CALL SLRSET
C
C Tell STITLE to output only two frames per second.  (Normally, one
C uses either 24 or 30 frames per second; using 2 in this example is
C just a good way to hold down the length of the metafile produced.)
C
        CALL SLSETI ('NFS - NUMBER OF FRAMES PER SECOND',2)
C
C Turn on fade-in and fade-out by telling STITLE how many seconds of
C each to do.  (By default, fade-in and fade-out are turned off.)
C
        CALL SLSETR ('FIN - FADE-IN TIME',2.)
        CALL SLSETR ('FOU - FADE-OUT TIME',2.)
C
C Set some parameters to their default values.  (These calls are here
C just to remind the reader of the existence of the parameters.)
C
C Tell STITLE not to output alignment frames.  These were mostly for
C use in the old days of real 35-millimeter film and are probably not
C useful now.
C
        CALL SLSETI ('ALN - ALIGNMENT FLAG',0)
C
C Tell STITLE what vertical spacing to use between test frames that
C represent the scrolled portion of output.  The effect of changing
C this will be seen only when MTST = 1.
C
        CALL SLSETI ('INC - INCREMENT BETWEEN TEST FRAMES',300)
C
C Tell STITLE what logical unit to use for WISS (Workstation-Independent
C Segment Storage).
C
        CALL SLSETI ('LOG - LOGICAL UNIT FOR WISS',4)
C
C Tell STITLE what to use for the X coordinates at the start and end of
C the scrolling phase.  It is not clear that these are good for much;
C one possible use might be to set both to a value other than 512 in
C order to get the lines of text centered about a particular vertical
C line.  Values used have to be between 1 and 1024.
C
        CALL SLSETI ('NXS - X COORDINATE AT START OF SCROLL',512)
        CALL SLSETI ('NXE - X COORDINATE AT END OF SCROLL',512)
C
C Tell STITLE what "principal size" to use for characters.  The value
C is given in terms of a 1024x1024 plotter window.  When the input to
C FTITLE or STITLE says to use a "SIZE" of 1., what is meant is a
C height of 'PSZ'/1023. fractional units (NDCs).  Similarly, a "SIZE"
C of 1.25 means a height of 1.25*'PSZ'/1023. fractional units (NDCs).
C
        CALL SLSETI ('PSZ - PRINCIPAL SIZE',21)
C
C Define the STITLE viewport.  Characters are clipped at the edges of
C this viewport and, when 'BGC' is set non-zero, STITLE fills it with
C the background color defined by 'BGC' (instead of depending on GKS
C to supply the background fill).  These parameters should be used in
C lieu of the old ones named 'LX1', 'LX2', 'LY1', and 'LY2'.
C
        CALL SLSETR ('VPL - VIEWPORT LEFT',0.)
        CALL SLSETR ('VPR - VIEWPORT RIGHT',1.)
        CALL SLSETR ('VPB - VIEWPORT BOTTOM',0.)
        CALL SLSETR ('VPT - VIEWPORT TOP',1.)
C
C Turn off the flag that tells the version of SLUBKG included below
C to draw a background.  This flag will be turned on for an STITLE
C example during which the background is appropriate for the title
C being scrolled by.
C
        IBKD=0
C
C BEGIN SIMPLE FTITLE EXAMPLE -----------------------------------------
C
C Tell FTITLE what "gap size" to use for the vertical spacing between
C lines.  The value is given in terms of a 1024x1024 plotter window.
C
        CALL SLSETI ('GSZ - GAP SIZE',40)
C
C Set the centering option for FTITLE.  You can set this to 0 to get
C left-justified titles, to 1 to get centered titles, or to 2 to get
C right-justified titles.
C
        CALL SLSETI ('ICO - INTEGER CENTERING OPTION',1)
C
C Tell FTITLE how many seconds worth of blank frames to put before
C the first title frame ('TM1'), after each title frame except the last
C ('TM2'), and after the last title frame ('TM2'+'TM3').  Note that
C 'TM3' can be negative to make the amount of time after the last
C title frame less than that between title frames.
C
        CALL SLSETR ('TM1 - TIME 1 (BEFORE FIRST TITLE)',1.)
        CALL SLSETR ('TM2 - TIME 2 (BETWEEN PAIRS OF TITLES)',.5)
        CALL SLSETR ('TM3 - TIME 3 (EXTRA TIME AFTER LAST TITLE)',.5)
C
C Call the routine SLWFIF to write an input file for FTITLE.  (One
C would normally just create the input file with an editor.  Writing
C it out in this way is easier logistically for the purposes of an
C example.)
C
        CALL SLWFIF
C
C Tell FTITLE what unit it should read the card input from.
C
        CALL SLSETI ('ICU - INPUT CARD UNIT',12)
C
C Open the file that SLWFIF created and rewind it.
C
        OPEN (UNIT=12,FILE='slex02.input',STATUS='OLD',
     +                                FORM='FORMATTED')
C
        REWIND (12)
C
C Call FTITLE to write out the titles defined by the card input.
C
        CALL FTITLE (MTST)
C
C Close the FTITLE input file.
C
        CLOSE (12)
C
C END OF SIMPLE FTITLE EXAMPLE, BEGIN SIMPLE STITLE EXAMPLE -----------
C
C Set up the CRDS array as expected by STITLE.  Each "card" contains
C an X position on the scroll, a Y position on the scroll, a color
C index, a horizontal centering option, a character size, and text.
C In this example, all the X-coordinates are set to 512, and the
C horizontal centering is always set to 1, so each line of text will
C be centered horizontally.  When the color index is omitted, the
C assumed value is that of the internal parameter 'FGC'.
C
        CRDS( 1)='  512  825  2 1  1.2PLOTCHAR'
        CRDS( 2)='  512  750    1  1.2can be'
        CRDS( 3)='  512  675    1  1.2used to'
        CRDS( 4)='  512  600    1  1.2write'
        CRDS( 5)='  512  525    1  1.2an equation'
        CRDS( 6)='  512  450    1  1.2like'
        CRDS( 7)='  512  375  3 1  2.5C:S:2:N:=A:S:2:N:+B:S:2:N:'
        CRDS( 8)='  512  300    1  1.2in a'
        CRDS( 9)='  512  225    1  1.2movie title'
        CRDS(10)='  512  150    1  1.2created by'
        CRDS(11)='  512   75  2 1  1.2STITLE'
C
        NCDS=11
C
C Define the remaining inputs for STITLE.  (Note that there is no
C reason not to put constant values directly in an STITLE call;
C defining each variable is done for purposes of clarity in the
C example.)
C
C IYST specifies the Y-coordinate that will be at the vertical center
C of the frame when scrolling begins.
C
        IYST=825
C
C IYND specifies the Y-coordinate that will be at the vertical center
C of the frame when scrolling is terminated.
C
        IYND=0
C
C TMST specifies how many seconds the first frame will remain
C stationary before scrolling begins.
C
        TMST=1.
C
C Specify the scroll time.  This is the time in seconds that the
C text will be scrolled from position IYST to IYND.
C
        TMMV=5.
C
C Specify how many seconds the final frame will remain stationary
C after scrolling stops.
C
        TMND=1.
C
C Call STITLE, putting 1-second gaps before and after its output.
C
        CALL SLOGAP (1.,MTST)
        CALL STITLE (CRDS,NCDS,IYST,IYND,TMST,TMMV,TMND,MTST)
        CALL SLOGAP (1.,MTST)
C
C END OF SIMPLE STITLE EXAMPLE, BEGIN FANCIER STITLE EXAMPLE ----------
C
C Tell PLOTCHAR to use font 25 (one of the filled fonts) by default.
C
        CALL PCSETI ('FN',25)
C
C Define the color with index 'BGC' (the default background color).
C
        CALL SLSETR ('BGR - BACKGROUND COLOR, RED COMPONENT'  ,1.)
        CALL SLSETR ('BGG - BACKGROUND COLOR, GREEN COMPONENT',1.)
        CALL SLSETR ('BGB - BACKGROUND COLOR, BLUE COMPONENT' ,1.)
C
C Define the color with index 'FGC' (the default foreground color).
C
        CALL SLSETR ('FGR - FOREGROUND COLOR, RED COMPONENT'  ,0.)
        CALL SLSETR ('FGG - FOREGROUND COLOR, GREEN COMPONENT',.7)
        CALL SLSETR ('FGB - FOREGROUND COLOR, BLUE COMPONENT' ,0.)
C
C Define the color with index 2 (for the words "PLOTCHAR" and "STITLE").
C
        CALL SLSETR ('FGR(2) - FOREGROUND COLOR, RED COMPONENT'  ,1.)
        CALL SLSETR ('FGG(2) - FOREGROUND COLOR, GREEN COMPONENT',0.)
        CALL SLSETR ('FGB(2) - FOREGROUND COLOR, BLUE COMPONENT' ,0.)
C
C Define the color with index 3 (for the equation).
C
        CALL SLSETR ('FGR(3) - FOREGROUND COLOR, RED COMPONENT'  ,0.)
        CALL SLSETR ('FGG(3) - FOREGROUND COLOR, GREEN COMPONENT',0.)
        CALL SLSETR ('FGB(3) - FOREGROUND COLOR, BLUE COMPONENT' ,1.)
C
C Define the color with index 4 (for the background drawing).
C
        CALL SLSETR ('FGR(4) - FOREGROUND COLOR, RED COMPONENT'  ,.9)
        CALL SLSETR ('FGG(4) - FOREGROUND COLOR, GREEN COMPONENT',.9)
        CALL SLSETR ('FGB(4) - FOREGROUND COLOR, BLUE COMPONENT' ,.9)
C
C Tell STITLE what kind of fade-in and fade-out to use.  In both cases,
C you can use -2 for a fade from/to black in the HSV system, -1 for no
C fade at all, and "n" greater than or equal to 0 for a fade-in from
C the color with color index "ni" = n/1000 and a fade-out to the color
C with color index "no" = MOD[n,1000], in the RGB system (in which case,
C "ni" = 999 requests no fade-in and "no" = 999 requests no fade-out.)
C
        CALL SLSETI ('BGF - BACKGROUND FADE-IN/OUT TYPE',0)
        CALL SLSETI ('FGF - FOREGROUND FADE-IN/OUT TYPE',0)
C
C Turn on the flag that tells the version of SLUBKG included below
C to draw a background.
C
        IBKD=1
C
C Call STITLE again, putting 1-second gaps before and after its output.
C
        CALL SLOGAP (1.,MTST)
        CALL STITLE (CRDS,NCDS,IYST,IYND,TMST,TMMV,TMND,MTST)
        CALL SLOGAP (1.,MTST)
C
C END OF FANCIER STITLE EXAMPLE ---------------------------------------
C
C Close GKS.
C
        CALL GDAWK (IWKID)
        CALL GCLWK (IWKID)
        CALL GCLKS
C
      END
      SUBROUTINE SLWFIF
C
C This routine just creates an input file for FTITLE to read.
C
C Declare an array to hold card images and then fill the array.
C
        CHARACTER*63 CRDS(7)
C
        DATA CRDS /
     +'    3   2.  .75                                                ',
     +'Stationary titles can be created by calling FTITLE.            ',
     +'This is the first example of such a title.                     ',
     +'Notice how it fades in and fades out.                          ',
     +'    2   2.  .75                                                ',
     +'This is another stationary title.                              ',
     +'It was created during the same call to FTITLE as the first one.'/
C
C Open the output file in the current directory.
C
        OPEN (UNIT=11,FILE='slex02.input',STATUS='UNKNOWN',
     +                                    FORM='FORMATTED')
C
C Write out the contents of the array CRDS.
C
        WRITE (11,'(A63)') CRDS
C
C Close the output file.
C
        CLOSE (11)
C
C Done.
C
        RETURN
C
      END
      SUBROUTINE SLUBKG (IPOC)
C
C This version of SLUBKG replaces the dummy one in STITLE.  When the
C flag IBKD in the common block CMUSER is turned on, this routine draws
C an appropriate background for the label that is being scrolled by.
C
C IPOC says what is going on in STITLE at the time that SLUBKG is
C called, as follows:
C
C IPOC  Position of call to SLUBKG
C ----  ---------------------------------------------------------------
C  -1   Just before drawing titles on a "fade-in" frame.
C  +1   Just after drawing titles on a "fade-in" frame.
C  -2   Just before drawing titles on a "start" frame.
C  +2   Just after drawing titles on a "start" frame.
C  -3   Just before drawing titles on a "move" frame.
C  +3   Just after drawing titles on a "move" frame.
C  -4   Just before drawing titles on an "end" frame.
C  +4   Just after drawing titles on an "end" frame.
C  -5   Just before drawing titles on a "fade-out" frame.
C  +5   Just after drawing titles on a "fade-out" frame.
C
C Declare a common block for communication with the main program.
C
        COMMON /CMUSER/ IBKD
C
C Define some transformations to put the drawing where it looks best.
C
        TRNX(X)=.4+X/16.
        TRNY(Y)=.4+Y/16.
C
C Draw a background if and only if IPOC is negative and IBKD is not
C zero.  Note that we are careful to save and restore the state of
C anything we change in GKS and in PLOTCHAR.
C
        IF (IPOC.LT.0.AND.IBKD.NE.0) THEN
          CALL PLOTIF (0.,0.,2)
          CALL GQPLCI (IERR,ISPL)
          CALL GSPLCI (4)
          CALL GQLWSC (IERR,WDTH)
          CALL GSLWSC (4.)
          CALL PLOTIF (TRNX( 0.),TRNY( 0.),0)
          CALL PLOTIF (TRNX(-3.),TRNY( 0.),1)
          CALL PLOTIF (TRNX(-3.),TRNY( 3.),1)
          CALL PLOTIF (TRNX( 0.),TRNY( 3.),1)
          CALL PLOTIF (TRNX( 0.),TRNY(-4.),1)
          CALL PLOTIF (TRNX( 4.),TRNY(-4.),1)
          CALL PLOTIF (TRNX( 4.),TRNY( 0.),1)
          CALL PLOTIF (TRNX( 0.),TRNY( 3.),1)
          CALL PLOTIF (TRNX( 3.),TRNY( 7.),1)
          CALL PLOTIF (TRNX( 7.),TRNY( 4.),1)
          CALL PLOTIF (TRNX( 4.),TRNY( 0.),1)
          CALL PLOTIF (TRNX( 0.),TRNY( 0.),1)
          CALL PLOTIF (0.,0.,2)
          CALL GSPLCI (ISPL)
          CALL GSLWSC (WDTH)
          CALL PCGETI ('CC',ISCC)
          CALL PCSETI ('CC',4)
          CALL PLCHHQ (TRNX(-1.5),TRNY( 1.5),'A:S:2',.05,0.,0.)
          CALL PLCHHQ (TRNX( 2.0),TRNY(-2.0),'B:S:2',.05,0.,0.)
          CALL PLCHHQ (TRNX( 3.5),TRNY( 3.5),'C:S:2',.05,0.,0.)
          CALL PCSETI ('CC',ISCC)
        END IF
C
C Done.
C
        RETURN
C
      END
