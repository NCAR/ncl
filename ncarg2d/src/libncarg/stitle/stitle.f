C
C	$Id: stitle.f,v 1.1.1.1 1992-04-17 22:33:00 ncargd Exp $
C
C
C PACKAGE STITLE         Documentation for each user entry follows below
C                        this package description.
C
C LATEST REVISION        June 1989
C
C PURPOSE                To produce movie titles with a minimum of
C                        effort.  Scrolling is possible.  Options
C                        exist for fading in and fading out.
C
C USAGE                  This package contains six user entries --
C                        FTITLE, STITLE, SLSETR, SLGETR, SLSETI,
C                        SLGETI.
C
C                        FTITLE stands for "fixed titles" and should
C                        be used when no scrolling is desired.  STITLE
C                        stands for "scrolled titles" and should be
C                        used when scrolling is desired.  The other
C                        four user entries are for setting and
C                        retrieving useful parameters.
C
C                        If the following assumptions are met, use
C                             CALL FTITLE (MOVIE)
C
C                          ASSUMPTIONS:
C
C                              .Each group of lines of text is contained
C                               on one frame (i.e., no scrolling).
C                              .Vertical spacing of titles is done
C                               automatically.
C                              .Titles are centered horizontally.
C                              .There is a maximum of 60 characters per
C                               line of text including PLOTCHAR modifiers.
C                              .In production mode, blank frames are
C                               generated before and after each group of
C                               title frames.
C                              .There are no more than 120 lines of text
C                               to be displayed on any given frame.
C
C                        If these assumptions are not met, use
C                             CALL STITLE (CARDS,NCARDS,NYST,
C                                          NYFIN,TST,TMV,TFIN,MV)
C
C SPECIAL CONDITIONS     See the user entry points SLSETR and SLSETI
C                        for setting common modifications.
C
C                        Since STITLE uses the GFLASH package to reproduce
C                        pictures when possible, and since a setting
C                        for color index 0 is saved in GFLASH buffers,
C                        you may get a warning message from some CGM
C                        interpreters that color index 0 has been set
C                        after plotting has begun in a given picture.
C                        This warning message can safely be ignored.
C
C PRECISION              SINGLE
C
C REQUIRED LIBRARY       PLOTCHAR, COLCONV, GFLASH
C PACKAGES
C
C LANGUAGE               FORTRAN
C
C HISTORY                Replaces previous routines AUTOTITLE and
C                        SCROLL at NCAR.
C
C                        Rewritten in April, 1980.
C                        Changed to use the new PWRITX instead of the
C                        old PWRX and made portable.
C
C                        Converted to FORTRAN 77, GKS, and usage of
C                        PLOTCHAR and GFLASH in June 1989.  User entries
C                        SLSETR and SLSETI added.  Fade in/out added.
C
C PORTABILITY            FORTRAN 77
C
C **********************************************************************
C
C SUBROUTINE STITLE (CARDS,NCARDS,NYST,NYFIN,TST,TMV,TFIN,MV)
C
C DIMENSION OF           CARDS(NCARDS) where CARDS is of type
C ARGUMENTS              CHARACTER*80
C
C PURPOSE                To produce scrolled movie titles.
C
C USAGE                  CALL STITLE (CARDS,NCARDS,NYST,
C                                     NYFIN,TST,TMV,TFIN,MV)
C
C ARGUMENTS
C
C ON INPUT               CARDS
C                          A CHARACTER*80 array dimensioned for NCARDS.
C                          This array must be filled prior to calling
C                          STITLE, either by internal manipulations
C                          or by reading card images.  Each element
C                          of the CARDS array contains the following:
C
C                            COLUMNS 1-5
C                              The MX coordinate of this line of text
C                              on the scroll, or an indicator that this
C                              line of text is a continuation of the
C                              previous line.  MX is the coordinate of
C                              the middle of the line if ICNTR is 1, and
C                              the coordinate of the left edge of the
C                              first character if ICNTR is 0.  see
C                              columns 11-15 for ICNTR.
C
C                              The scroll of text is 1024 units
C                              wide and any number of units high.
C                              MX = -9999 is the continuation card
C                              indicator.  Any number of continuation
C                              cards may be used.  Trailing blanks are
C                              omitted from each line, including those
C                              followed by a continuation card.
C
C                            COLUMNS 6-10
C                              The MY coordinate of this line of text on
C                              the scroll (MY may be outside the 1-1024
C                              unit range). In the case of a
C                              continue line, columns 6-20 are ignored.
C
C                            COLUMNS 11-15
C                              ICNTR - the centering option:
C                              = 0  start the text at MX.
C                              = 1  center the text about MX.
C                              = 2  end the text at MX.
C
C                            COLUMNS 16-20
C                              SIZE -  The relative size of characters.
C                              This multiplies the PLOTCHAR character
C                              height.  Recommended range 1. - 2.5
C                              (PLOTCHAR modifiers can also be used to
C                               change sizes).
C
C                            COLUMNS 21-80
C                              Text for this line, or for continuation
C                              of a line when MX = -9999.  Note:  Every
C                              line of text, except continuation lines,
C                              must start with a PLOTCHAR modifier.
C
C                        NCARDS
C                          Second dimension of CARDS, i.e., the number
C                          of lines in CARDS.
C
C                        NYST
C                          The STITLE coordinate that will be at the
C                          center of the screen when the text is first
C                          displayed (see the diagram in the Graphics
C                          Utilities section of the manual for
C                          clarification).
C
C                        NYFIN
C                          As NYST but for final position.
C
C                        TST
C                          Time in seconds that the scroll will be
C                          stationary at NYST.  One second is
C                          recommended.
C
C                        TMV
C                          Time to move the scroll from NYST to NYFIN.
C                          This should be the time required to read the
C                          text aloud at slow to normal speed.
C
C                        TFIN
C                          Time that the scroll will be stationary at
C                          NYFIN.  One second is recommended.
C
C                        MV
C                          Switch to indicate whether this is a practice
C                          run or the movie is being made.
C
C                                = 0  - Movie being made.
C                                = 1  - Practice run.
C
C                          Practice runs output outlined representative
C                          frames from the scroll with a legend
C                          indicating the number of seconds the frame
C                          will be shown at the start or finish, or the
C                          number of seconds into the total moving time
C                          that a particular frame represents.  When the
C                          movie is being made this practice output is
C                          suppressed.
C
C ON OUTPUT              All arguments are unchanged.
C
C I/O                    STITLE takes all input through its argument
C                        list, and generates graphic output.
C
C ALGORITHM              STITLE in effect moves the body of text up
C                        through the screen window, outputting frames
C                        required to generate a movie sequence of
C                        duration specified by the user.
C
C                        At each frame STITLE skips plotting lines of
C                        text that are completely outside of the screen
C                        window.  Lines of text that are partially in
C                        the window are clipped using SCPLTW, which is
C                        invoked by PLOTCHAR when PLOTCHAR parameter
C                        SC is set to 1.
C
C **********************************************************************
C
C SUBROUTINE FTITLE (MOVIE)
C
C DIMENSION OF           none
C ARGUMENTS
C
C PURPOSE                To produce unscrolled movie titles.
C
C USAGE                  CALL FTITLE (MOVIE)
C
C ARGUMENTS
C
C ON INPUT               MOVIE
C                          A switch to indicate whether this is a
C                          practice run or the movie is being made.
C
C                              = 0  -  Movie being made.
C                              = 1  -  Practice run.
C
C                          Practice runs output an outlined frame of
C                          titles with a legend indicating how many
C                          seconds the frame will be shown.  The number
C                          of blank frames that will be output before
C                          and after the title sequence are also
C                          indicated.  When the movie is being made this
C                          practice output is suppressed.
C
C ON OUTPUT              Argument is unchanged.
C FOR FTITLE
C
C I/O                    FTITLE reads data from the standard input
C                        and generates graphic output.
C
C                        INPUT DATA
C                        ----------
C                        The input data are read in groups, each
C                        group resulting in a frame of titles which
C                        is repeated to give enough time for reading.
C                        There can be any number of groups.
C                        FTITLE keeps processing groups until
C                        an NCARD = 0 is read.  A group consists
C                        of the following:
C
C                        . A header line from which NCARD, TIME, SIZE
C                          are read under FORMAT (I5,2F5.1).
C
C                          NCARD
C                            Number of text lines that follow.  If
C                            NCARD = 0, FTITLE will return to the
C                            calling routine.
C
C                          TIME
C                            Time in seconds this frame should be
C                            displayed.
C
C                          SIZE
C                            Relative size of characters.  This
C                            multiplies the PLOTCHAR character height.
C
C                        . Text lines, each containing one line of
C                          the movie title.  PLOTCHAR modifiers may be
C                          used (see PLOTCHAR documentation).
C                          Characters should not appear beyond column 60.
C
C **********************************************************************
C
C SUBROUTINE SLSETR (PA,RVAL)
C
C DIMENSION OF           None
C ARGUMENTS
C
C PURPOSE                To set useful real-valued parameters in the
C                        STITLE package.
C
C USAGE                  CALL SLSETR (PA,RVAL)
C
C ARGUMENTS
C
C ON INPUT               PA
C                          A character string specifying what parameter
C                          is to be set.  Only the first three
C                          characters of the string are used in
C                          determining the legal values.
C
C                        RVAL
C                          The real value to be set for PA.
C
C ON OUTPUT              Arguments are unchanged.
C FOR SLSETR
C
C I/O                    None
C
C NOTES
C
C   Table of current settable parameters:
C
C   PA   Default  Function
C   ---  -------  ---------------------------------------------------
C   BGR  Device   Specify a real value in the range 0. to 1. to be
C        default  used for the red component of the background color.
C
C   BGG  Device   Specify a real value in the range 0. to 1. to be
C        default  used for the green component of the background color.
C
C   BGB  Device   Specify a real value in the range 0. to 1. to be
C        default  used for the blue component of the background color.
C
C   FGR  Device   Specify a real value in the range 0. to 1. to be
C        default  used for the red component of the foreground color.
C                 The characters are drawn in the foreground color.
C                 The foreground color can alternatively be set
C                 using a call to the GKS subroutine GSCR.
C
C   FGG  Device   Specify a real value in the range 0. to 1. to be
C        default  used for the green component of the foreground color.
C                 The characters are drawn in the foreground color.
C                 The foreground color can alternatively be set
C                 using a call to the GKS subroutine GSCR.
C
C   FGB  Device   Specify a real value in the range 0. to 1. to be
C        default  used for the blue component of the foreground color.
C                 The characters are drawn in the foreground color.
C                 The foreground color can alternatively be set
C                 using a call to the GKS subroutine GSCR.
C
C   FIN     0.    Number of seconds to fade in the first title.  The
C                 background color and foreground colors are faded in
C                 independently.  Each color is faded in from black
C                 to its current color by varying the value parameter
C                 in an HSV representation of the color in a linear
C                 manner over the time period specified.
C
C   FOU     0.    Number of seconds to fade out the last title.  The
C                 background color and foreground colors are faded out
C                 independently.  Each color is faded out from its
C                 current color to black by varying the value parameter
C                 in an HSV representation of the color in a linear
C                 manner over the time period specified.
C
C   GSZ    40.    Value for interline spacing.  Used only by FTITLE.
C
C   PSZ    21.    Specify default character height.  Used only by
C                 FTITLE.
C
C   TM1     1.    Number of seconds worth of blank frames
C                 generated before any title frames
C                 (at 24 frames/second.)  Used only by FTITLE.
C
C   TM2     .5    Number of seconds worth of blank frames
C                 between sets of title frames, and after
C                 the last set of title frames.  Used only by FTITLE.
C
C **********************************************************************
C
C SUBROUTINE SLSETI (PA,IVAL)
C
C DIMENSION OF           None
C ARGUMENTS
C
C PURPOSE                To set integer-valued parameters in the
C                        STITLE package.
C
C USAGE                  CALL SLSETI (PA,IVAL)
C
C ARGUMENTS
C
C ON INPUT               PA
C                          A character string specifying what parameter
C                          is to be set.  Only the first three
C                          characters of PA are used in the match.
C
C                        IVAL
C                          An integer value supplying the
C                          value to be set for PA.
C
C ON OUTPUT              Arguments are unchanged.
C FOR SLSETI
C
C I/O                    None
C
C NOTES
C
C   Table of current settable parameters:
C
C
C   PA   Default  Function
C   ---  -------  ------------------------------------------
C   ICU     5     Unit number for reading input.  Used only
C                 by FTITLE.
C
C   ICO     1     Centering option.  Set to 0 to get left
C                 edges lined up at X-coordinate 64, and to 2
C                 to get right edges lined up at
C                 X-coordinate 960.  Set to 1 for centered
C                 text.  Used only by FTITLE.
C
C   NXS    512    Analogous to argument NYST to allow for
C                 limited scrolling in the X direction.
C                 NXS must be within the current STITLE
C                 window, and text must leave the window
C                 through the top and not the sides.
C                 Used only by STITLE.
C
C   NXE    512    Analogous to argument NYFIN to allow for
C                 limited scrolling in the X direction.
C                 NXE must be within the current STITLE
C                 window, and text must leave the window
C                 through the top and not the sides.
C                 Used only by STITLE.
C
C   INC    300    Vertical STITLE coordinate spacing
C                 between practice frames.  Used only by
C                 STITLE.
C
C   LX1     0     Integer in the range 0 to 32767 specifying the
C                 lower left X value of the viewport.
C
C   LX2   32767   Integer in the range 0 to 32767 specifying the
C                 upper right X value of the viewport.
C
C   LY1     0     Integer in the range 0 to 32767 specifying the
C                 lower left Y value of the viewport.
C
C   LY2   32767   Integer in the range 0 to 32767 specifying the
C                 upper right Y value of the viewport.
C
C   ALN     0     Flag to control whether the alignment frames with
C                 dots in the corners are put out in non-test mode.
C                 ALN=1 means put the frames out; ALN=0 means
C                 suppress the frames.
C
C   SBK     0     Suppress fade in/out of the background color
C                 when a fade in/out time has been specified.  The
C                 background color will appear at full intensity
C                 during a fade in/out.  If SBK=0, then the fade
C                 in/out will be honored; otherwise not.
C
C   SFG     0     Suppress fade in/out of the foreground color
C                 when a fade in/out time has been specified.  The
C                 foreground color will appear at full intensity
C                 during a fade in/out.  If SFG=0, then the fade
C                 in/out will be honored; otherwise not.
C
C   WID     9     Workstation identifier for WISS which is used
C                 internally in the calls to GFLASx.
C
C   LOG     4     FORTRAN logical unit number for opening WISS.
C
C **********************************************************************
C
C SUBROUTINE SLGETI (PA,IVAL)
C
C DIMENSION OF           None
C ARGUMENTS
C
C PURPOSE                To retrieve the settings of integer-valued
C                        parameters in the STITLE package.
C
C USAGE                  CALL SLGETI (PA,IVAL)
C
C ARGUMENTS
C
C ON INPUT               PA
C                          A character string specifying the parameter
C                          whose value is to be retrieved.  Only the
C                          first three characters of PA are used in
C                          the match.
C
C                        IVAL
C                          An integer value giving the current setting
C                          of the parameter specified in PA.
C
C ON OUTPUT              Arguments are unchanged.
C FOR SLGETI
C
C I/O                    None
C
C NOTES
C
C   Consult the table of parameters in the documetation of SLSETI above
C   for the legal values for PA.
C
C **********************************************************************
C
C SUBROUTINE SLGETR (PA,RVAL)
C
C DIMENSION OF           None
C ARGUMENTS
C
C PURPOSE                To retrieve the settings of real-valued
C                        parameters in the STITLE package.
C
C USAGE                  CALL SLGETR (PA,RVAL)
C
C ARGUMENTS
C
C ON INPUT               PA
C                          A character string specifying the parameter
C                          whose value is to be retrieved.  Only the
C                          first three characters of PA are used in
C                          the match.
C
C                        RVAL
C                          A real value giving the current setting
C                          of the parameter specified in PA.
C
C ON OUTPUT              Arguments are unchanged.
C FOR SLGETI
C
C I/O                    None
C
C NOTES
C
C   Consult the table of parameters in the documetation of SLSETR above
C   for the legal values for PA.
C
C **********************************************************************
C
C
      SUBROUTINE STITLE (CARDS,NCARDS,NYST,NYFIN,TST,TMV,TFIN,MV)
C
      CHARACTER*(*) CARDS(NCARDS)
      CHARACTER*80  CTMP
      INTEGER CLNTH
C
      DIMENSION CRECT(4),XE(4),YE(4)
C
C
C The labeled common block SCRLDT holds all of the internal parameters
C for the STITLE package.
C
      COMMON /SCRLDT/ ICU,ICO,PCHSZ,GAPSZ,T1,T2,NXST,NXFIN,ICRTJP,
     +                LIM(4),MXOLD,MYOLD,LOLD,IBKG,LND,BGCLR(3),
     +                FGCLR(3),IFST,IWK,FIN,FOU,ISPB,ISPF,IDEN,IWU
      SAVE   /SCRLDT/
C
C Declare the block data routine SCRLBD external to force it to load,
C so that the internal parameters will be initialized.
C
      EXTERNAL SCRLBD
C
C Preset local variables.
C
      DATA IORNT  / 0 /
      DATA XE(1),XE(2),XE(3),XE(4)/0.0,1.0,1.0,0.0/
      DATA YE(1),YE(2),YE(3),YE(4)/0.0,0.0,1.0,1.0/
C
C Initialize variables if this is the first user call.
C
      IF (IFST .EQ. 0) THEN
        CALL SLINIT
        IFST = 1
      ENDIF
C
C If the card buffer is empty, there is nothing to do.
C
      IF (NCARDS .LE. 0) RETURN
C
C Open WISS for GFLASH.
C
      CALL GOPWK(IDEN,IWU,3)
C
C Set flag to inform PLOTCHAR that SCPLTW is to do the clipping.
C
      CALL PCSETI('SC',1)
C
C Save current clipping indicator, and set clipping off.  All
C necessary clipping is currently done by calls to SCPLTW.
C
      CALL GQCLIP(IER,ICLP,CRECT)
      CALL GSCLIP(0)
C
C Save current grid size, and set to 1024.
C
      CALL GETSI (IXSC,IYSC)
      CALL SETI  (  10,  10)
C
C Save current normalization number and set to normalization
C transformation 0.
C
      CALL GQCNTN(IER,NTOLD)
      CALL GSELNT(0)
C
C If in production mode, dot the corners of the first and last frames
C (for splicing purposes).
C
      IF (MV.EQ.0 .AND. LND.EQ.1) THEN
        CALL GQCR(IWK,1,0,IER,RR,GG,BB)
        CALL GSCR(IWK,1,1.,1.,1.)
        CALL GSPMCI(1)
        CALL GPM(4,XE,YE)
        CALL FRAME
        IF (IER .EQ. 0) CALL GSCR(IWK,1,RR,GG,BB)
      ENDIF
C
C ISCRD1 is the X-coordinate of the center of the window.
C
      ISCRD1 = (LIM(1)+LIM(2))/64
C
C ISCRD2 is the Y-coordinate of the center of the window.
C
      ISCRD2 = (LIM(3)+LIM(4))/64
C
C ISCRD3 is half the height of the window.
C
      ISCRD3 = (LIM(4)-LIM(3))/64
C
C Initial picture.
C
      IF (FIN .EQ. 0.) GO TO 101
      NREP = FIN*24.+.5
      IF (MV .NE. 0) NREP = 1
      DO 104 J=1,NREP
C
C Plot background.
C
      IF (ISPB .EQ. 0) THEN
        CALL RGBHSV (BGCLR(1),BGCLR(2),BGCLR(3),HH,SS,VV)
        VV = REAL(J)*VV/REAL(NREP)
        CALL HSVRGB (HH,SS,VV,RR,GG,BB)
        CALL GSCR(IWK,IBKG,RR,GG,BB)
      ELSE
        RR = 1.
        CALL GSCR(IWK,IBKG,BGCLR(1),BGCLR(2),BGCLR(3))
      ENDIF
      IF (RR.GT.0.01 .OR. GG.GT.0.01 .OR. BB.GT.0.01) CALL BKGND
C
C Set foreground color.
C
      IF (ISPF .EQ. 0) THEN
        CALL RGBHSV (FGCLR(1),FGCLR(2),FGCLR(3),HH,SS,VV)
        VV = REAL(J)*VV/REAL(NREP)
        CALL HSVRGB (HH,SS,VV,RR,GG,BB)
        CALL GSCR(IWK,1,RR,GG,BB)
      ELSE
        CALL GSCR(IWK,1,FGCLR(1),FGCLR(2),FGCLR(3))
      ENDIF
C
C Plot characters.
C
      DO 102 I=1,NCARDS
      READ(CARDS(I)(1:5),910) IXPOS
      IF (IXPOS.EQ.(-9999)) GO TO 102
      XPOS = CPUX(IXPOS-NXST+ISCRD1)
      READ(CARDS(I)(6:10),910) IYPOS
      YPOS = CPUY(IYPOS-NYST+ISCRD2)
      NUMCHR = CLNTH(CARDS(I)(21:80))+20
      READ(CARDS(I)(11:15),910) ICNTR
      CNTR = REAL(ICNTR-1)
      READ(CARDS(I)(16:20),912) SIZE
      PSZ = SIZE*PCHSZ
      CALL PLCHHQ(XPOS,YPOS,
     -       CARDS(I)(21:NUMCHR),PSZ,REAL(IORNT),CNTR)
  102 CONTINUE
      IF (MV .EQ. 0) GO TO 103
      CALL WNDOUT
      CTMP = ' '
      WRITE(CTMP(1:8),911) FIN
      CTMP(9:24) = ' SECS OF FADE IN'
      CALL PWRITV (CPUX(1012),CPUY(848),CTMP,24,2,0,0)
  103 CALL FRAME
  104 CONTINUE
C
  101 CONTINUE
      IF (TST .EQ. 0.) GO TO 109
      CALL GFLAS1(IDEN)
         CALL GSCR(IWK,1,FGCLR(1),FGCLR(2),FGCLR(3))
         CALL GSCR(IWK,IBKG,BGCLR(1),BGCLR(2),BGCLR(3))
         DO 106 I=1,NCARDS
         READ(CARDS(I)(1:5),910) IXPOS
         IF (IXPOS.EQ.(-9999)) GO TO 106
         XPOS = CPUX(IXPOS-NXST+ISCRD1)
         READ(CARDS(I)(6:10),910) IYPOS
         YPOS = CPUY(IYPOS-NYST+ISCRD2)
         NUMCHR = CLNTH(CARDS(I)(21:80))+20
         READ(CARDS(I)(11:15),910) ICNTR
         CNTR = REAL(ICNTR-1)
         READ(CARDS(I)(16:20),912) SIZE
         PSZ = SIZE*PCHSZ
         IF (IBKG.NE.0 .AND. I.EQ.1) CALL BKGND
         CALL PLCHHQ(XPOS,YPOS,
     -          CARDS(I)(21:NUMCHR),PSZ,REAL(IORNT),CNTR)
  106    CONTINUE
      CALL GFLAS2
      NREP = TST*24.+.5
      IF (MV .NE. 0) NREP = 1
         DO 108 I=1,NREP
         CALL GFLAS3 (IDEN)
         IF (MV .EQ. 0) GO TO 107
         CALL WNDOUT
         CTMP = ' '
         WRITE(CTMP(1:8),911) TST
         CTMP(9:22) = ' SECS AT START'
         CALL PWRITV (CPUX(1012),CPUY(848),CTMP,22,2,0,0)
  107    CALL FRAME
  108    CONTINUE
C
C Moving portion.
C
  109 IF (TMV .EQ. 0.) GO TO 115
      TFRAC=0.
      NDIFFR = TMV*24.+.5
      IF (MV .NE. 0) NDIFFR = MAX0(IABS(NYFIN-NYST)/ICRTJP,1)
      TPER = TMV/FLOAT(NDIFFR)
      DIFFX = FLOAT(NXFIN-NXST)/FLOAT(NDIFFR)
      DIFFY = FLOAT(NYFIN-NYST)/FLOAT(NDIFFR)
      NDIFFR = NDIFFR+1
         DO 114 K=1,NDIFFR
            FKM1 = K-1
            CALL GSCR(IWK,IBKG,BGCLR(1),BGCLR(2),BGCLR(3))
            CALL GSCR(IWK,1,FGCLR(1),FGCLR(2),FGCLR(3))
            LX = ISCRD1-NXST-IFIX(FKM1*DIFFX)
            LY = ISCRD2-NYST-IFIX(FKM1*DIFFY)
            DO 111 I=1,NCARDS
            READ(CARDS(I)(1:5),910) IXPOS
            IF (IXPOS.EQ.(-9999)) GO TO 111
            READ(CARDS(I)(6:10),910) IYPOS
            MX = IXPOS+LX
            MY = IYPOS+LY
            XPOS = CPUX(MX)
            YPOS = CPUY(MY)
            NUMCHR = CLNTH(CARDS(I)(21:80))+20
            READ(CARDS(I)(11:15),910) ICNTR
            CNTR = REAL(ICNTR-1)
            READ(CARDS(I)(16:20),912) SIZE
            PSZ = SIZE*PCHSZ
            MYCRIT = 25.*SIZE
            MYDIS = IABS(MY-ISCRD2)
            IF (IBKG.NE.0 .AND. I.EQ.1) CALL BKGND
C
C  Test if clipping is needed.
C
            IF (MYDIS .LE. ISCRD3-MYCRIT) GO TO 110
C
C  Test if line is entirely out of the viewport.
C
            IF (MYDIS .GE. ISCRD3+MYCRIT) GO TO 111
C
C  Call PLCHHQ with clipping turned on.
C
            CALL PCSETI('SC',1)
            CALL PLCHHQ(XPOS,YPOS,
     -             CARDS(I)(21:NUMCHR),PSZ,REAL(IORNT),CNTR)
            GO TO 111
  110       CONTINUE
C
C  Call PLCHHQ with clipping turned off.
C
            CALL PCSETI('SC',0)
            CALL PLCHHQ(XPOS,YPOS,
     -             CARDS(I)(21:NUMCHR),PSZ,REAL(IORNT),CNTR)
            CALL PCSETI('SC',1)
  111       CONTINUE
         IF (MV .EQ. 0) GO TO 113
         CALL WNDOUT
         CTMP = ' '
         WRITE(CTMP(1:8),911) TFRAC
         CTMP(9:28) = ' SECS INTO MOVE TIME'
         CALL PWRITV (CPUX(1012),CPUY(848),CTMP,28,2,0,0)
         TFRAC = TFRAC+TPER
  113    CALL FRAME
  114    CONTINUE
C
C Last picture.
C
  115 IF (TFIN .EQ. 0.) GO TO 119
      CALL GFLAS1 (IDEN)
         CALL GSCR(IWK,IBKG,BGCLR(1),BGCLR(2),BGCLR(3))
         CALL GSCR(IWK,1,FGCLR(1),FGCLR(2),FGCLR(3))
         DO 116 I=1,NCARDS
         READ(CARDS(I)(1:5),910) IXPOS
         IF (IXPOS.EQ.(-9999)) GO TO 116
         XPOS = CPUX(IXPOS-NXFIN+ISCRD1)
         READ(CARDS(I)(6:10),910) IYPOS
         YPOS = CPUY(IYPOS-NYFIN+ISCRD2)
         NUMCHR = CLNTH(CARDS(I)(21:80))+20
         READ(CARDS(I)(11:15),910) ICNTR
         CNTR = REAL(ICNTR-1)
         READ(CARDS(I)(16:20),912) SIZE
         PSZ = SIZE*PCHSZ
         IF (IBKG.NE.0 .AND. I.EQ.1) CALL BKGND
         CALL PLCHHQ(XPOS,YPOS,
     -          CARDS(I)(21:NUMCHR),PSZ,REAL(IORNT),CNTR)
  116    CONTINUE
      CALL GFLAS2
      NREP = TFIN*24.+.5
      IF (MV .NE. 0) NREP = 1
         DO 118 I=1,NREP
         CALL GFLAS3 (IDEN)
         IF (MV .EQ. 0) GO TO 117
         CALL WNDOUT
         CTMP = ' '
         WRITE(CTMP(1:8),911) TFIN
         CTMP(9:20) = ' SECS AT END'
         CALL PWRITV (CPUX(1012),CPUY(848),CTMP,20,2,0,0)
  117    CALL FRAME
  118    CONTINUE
  119 CONTINUE
C
C Fade out.
C
      IF (FOU .EQ. 0.) GO TO 201
      NREP = FOU*24.+.5
      IF (MV .NE. 0) NREP = 1
      DO 204 J=1,NREP
C
C Plot background.
C
      IF (ISPB .EQ. 0) THEN
        CALL RGBHSV (BGCLR(1),BGCLR(2),BGCLR(3),HH,SS,VV)
        VV = REAL(NREP-J+1)*VV/REAL(NREP)
        CALL HSVRGB (HH,SS,VV,RR,GG,BB)
        CALL GSCR(IWK,IBKG,RR,GG,BB)
      ELSE
        RR = 1.
        CALL GSCR(IWK,IBKG,BGCLR(1),BGCLR(2),BGCLR(3))
      ENDIF
      IF (RR.GT.0.01 .OR. GG.GT.0.01 .OR. BB.GT.0.01) CALL BKGND
C
C Set foreground color.
C
      IF (ISPF .EQ. 0) THEN
        CALL RGBHSV (FGCLR(1),FGCLR(2),FGCLR(3),HH,SS,VV)
        VV = REAL(NREP-J+1)*VV/REAL(NREP)
        CALL HSVRGB (HH,SS,VV,RR,GG,BB)
        CALL GSCR(IWK,1,RR,GG,BB)
      ELSE
        CALL GSCR(IWK,1,FGCLR(1),FGCLR(2),FGCLR(3))
      ENDIF
C
C Plot characters.
C
      DO 202 I=1,NCARDS
      READ(CARDS(I)(1:5),910) IXPOS
      IF (IXPOS.EQ.(-9999)) GO TO 202
      XPOS = CPUX(IXPOS-NXFIN+ISCRD1)
      READ(CARDS(I)(6:10),910) IYPOS
      YPOS = CPUY(IYPOS-NYFIN+ISCRD2)
      NUMCHR = CLNTH(CARDS(I)(21:80))+20
      READ(CARDS(I)(11:15),910) ICNTR
      CNTR = REAL(ICNTR-1)
      READ(CARDS(I)(16:20),912) SIZE
      PSZ = SIZE*PCHSZ
      CALL PLCHHQ(XPOS,YPOS,
     -       CARDS(I)(21:NUMCHR),PSZ,REAL(IORNT),CNTR)
  202 CONTINUE
      IF (MV .EQ. 0) GO TO 203
      CALL WNDOUT
      CTMP = ' '
      WRITE(CTMP(1:8),911) FOU
      CTMP(9:25) = ' SECS OF FADE OUT'
      CALL PWRITV (CPUX(1012),CPUY(848),CTMP,25,2,0,0)
  203 CALL FRAME
  204 CONTINUE
C
  201 CONTINUE
C
C Dot corners of frame.
C
      IF (MV.EQ.0 .AND. LND.EQ.1) THEN
        CALL GQCR(IWK,1,0,IER,RR,GG,BB)
        CALL GSCR(IWK,1,1.,1.,1.)
        CALL GSPMCI(1)
        CALL GPM(4,XE,YE)
        CALL FRAME
        IF (IER .EQ. 0) CALL GSCR(IWK,1,RR,GG,BB)
      ENDIF
C
C Close WISS.
C
      CALL GCLWK(IDEN)
C
C Turn off SCPLTW calls for clipping in PLOTCHAR.
C
      CALL PCSETI('SC',0)
C
C Restore original settings.
C
      CALL GSCLIP(ICLP)
      CALL SETI (IXSC,IYSC)
      CALL GSELNT(NTOLD)
C
      RETURN
C
  910 FORMAT(I5)
  911 FORMAT(F8.2)
  912 FORMAT(F5.1)
      END
