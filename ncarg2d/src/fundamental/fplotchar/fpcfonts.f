
      PROGRAM FPCFONTS
C
C Define the error file, the Fortran unit number, the workstation type,
C and the workstation ID to be used in calls to GKS routines.
C
C     PARAMETER (IERRF=6, LUNIT=2, IWTYPE=1,  IWKID=1)   ! NCGM
C     PARAMETER (IERRF=6, LUNIT=2, IWTYPE=8,  IWKID=1)   ! X Windows
C     PARAMETER (IERRF=6, LUNIT=2, IWTYPE=11, IWKID=1)   ! PDF
C     PARAMETER (IERRF=6, LUNIT=2, IWTYPE=20, IWKID=1)   ! PostScript
C
      PARAMETER (IERRF=6, LUNIT=2, IWTYPE=1,  IWKID=1)
C
C --- D E C L A R A T I O N S -----------------------------------------
C
C Define arrays for column labels and row labels for the plots showing
C the complex and duplex character sets.
C
      CHARACTER*9 CLBL(12)
      CHARACTER*5 RLBL(48)
C
C Define a couple of temporary variables of type CHARACTER.
C
      CHARACTER*6 CTMP
      CHARACTER*8 CHRS
C
C Define an array in which to put the numbers of the filled fonts.
C
      DIMENSION IFFN(22)
C
C Define the column and row labels.  The character string ':c:r', where
C "c" is the first three characters of a column label and "r" is the
C first character of a row label, is used to select the character to
C be written in that column of that row.
C
      DATA CLBL /'PRU(0000)','PRL(0100)','IRU(0200)','IRL(0300)',
     +           'KRU(0400)','KRL(0500)','PGU(0600)','PGL(0700)',
     +           'IGU(1000)','IGL(1100)','KGU(1200)','KGL(1300)'/
C
      DATA RLBL /'A(01)','B(02)','C(03)','D(04)','E(05)','F(06)',
     +           'G(07)','H(10)','I(11)','J(12)','K(13)','L(14)',
     +           'M(15)','N(16)','O(17)','P(20)','Q(21)','R(22)',
     +           'S(23)','T(24)','U(25)','V(26)','W(27)','X(30)',
     +           'Y(31)','Z(32)','0(33)','1(34)','2(35)','3(36)',
     +           '4(37)','5(40)','6(41)','7(42)','8(43)','9(44)',
     +           '+(45)','-(46)','*(47)','/(50)','((51)',')(52)',
     +           '$(53)','=(54)',' (55)',',(56)','.(57)','     '/
C
C
C Define a flag which says, if 0, that the first eight plots are to
C occupy eight separate frames and, if 1, that those plots are to be
C compressed onto two frames.
C
      DATA ICMP / 1 /
C
C Define the font numbers for the filled fonts.
C
      DATA IFFN /  21, 22, 25, 26, 29, 30, 33, 34, 35, 36, 37 ,
     +              121,122,125,126,129,130,133,134,135,136,137 /
C
C --- E X E C U T A B L E   C O D E -----------------------------------
C
C  Open GKS, open and activate a workstation.
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
C
C Set up the background and foreground colors
C
      CALL GSCR (IWKID,0,1.,1.,1.)
      CALL GSCR (IWKID,1,0.,0.,0.)
C
C Set the "fill area interior style" to "solid".
C
      CALL GSFAIS (1)
C
C Do a call to SET which allows us to use fractional coordinates.
C
      CALL SET (0.,1.,0.,1.,0.,1.,0.,1.,1)
C
C Produce examples of the complex and duplex character sets.
C
C Compute a character-size multiplier, depending on whether the first
C eight plots are being put on eight frames or two.
C
      CSMU=REAL(2-ICMP)
C
C Each pass through the loop on I produces four plots - the first four
C for the complex set, and the second four for the duplex set.
C
      DO 105 I=1,2
C
C Change to the appropriate character set.
C
        CALL PCSETI ('CD',I-1)
C
C Each pass through the following loop produces a single plot.
C
        DO 104 J=1,4
C
C If the first eight plots are to be compressed, re-do the SET call to
C put the plot in a particular quadrant of the frame.
C
          IF (ICMP.NE.0)
     +      CALL SET (.5*REAL(MOD(J-1,2)),.5*REAL(MOD(J-1,2))+.5,
     +                .5-.5*REAL((J-1)/2),   1.-.5*REAL((J-1)/2),
     +                                             0.,1.,0.,1.,1)
C
C Put labels at the top of the frame and along the left edge.
C
          IF (I.EQ.1) THEN
            CALL PLCHHQ (.5,.98,'PLCHHQ - COMPLEX CHARACTER SET',
     +                                                   CSMU*.01,0.,0.)
          ELSE
            CALL PLCHHQ (.5,.98,'PLCHHQ - DUPLEX CHARACTER SET',
     +                                                   CSMU*.01,0.,0.)
          END IF
C
          CALL PLCHHQ (.58,.9267,
     +                 'FUNCTION CODES SPECIFYING SIZE, FONT, AND CASE',
     +                                                CSMU*.00735,0.,0.)
C
          CALL PLCHHQ (.035,.445,':D:STANDARD FORTRAN CHARACTERS',
     +                                                CSMU*.00735,0.,0.)
C
C Force constant spacing of the characters used for the column and row
C labels, so that they will line up better with each other.
C
          CALL PCSETR ('CS',1.25)
C
C Label the columns.
C
          DO 101 K=1,12
            XPOS=.125+.07*REAL(K)
            CALL PLCHHQ (XPOS,.90,CLBL(K)(1:3),CSMU*.006,0.,0.)
            CALL PLCHHQ (XPOS,.88,CLBL(K)(4:9),CSMU*.004,0.,0.)
  101     CONTINUE
C
C Each pass through the following loop produces a single row.
C
          DO 103 K=1,12
C
C Compute the Y coordinate of the row.
C
            YPOS=.9-.07*REAL(K)
C
C Label the row.
C
            CALL PLCHHQ (.085,YPOS,RLBL(12*(J-1)+K)(1:1),CSMU*.006,0.,
     +                                                              -1.)
            CALL PLCHHQ (.105,YPOS,RLBL(12*(J-1)+K)(2:5),CSMU*.004,0.,
     +                                                              -1.)
C
C Each pass through the following loop produces a single character.
C
            DO 102 L=1,12
              XPOS=.125+.07*REAL(L)
              CTMP=':'//CLBL(L)(1:3)//':'//RLBL(12*(J-1)+K)(1:1)
              CALL PLCHHQ (XPOS,YPOS,CTMP,CSMU*.01,0.,0.)
  102       CONTINUE
C
  103     CONTINUE
C
C Return to variable spacing.
C
          CALL PCSETR ('CS',0.)
C
C If eight frames are being produced, advance the frame here.
C
        IF (ICMP.EQ.0) CALL FRAME
C
  104   CONTINUE
C
C If two frames are being produced, advance the frame here.
C
        IF (ICMP.NE.0) CALL FRAME
C
  105 CONTINUE
C
C Return to the complex character set.
C
      CALL PCSETI ('CD',0)
C
C If two frames were produced, re-do the call to SET which allows us to
C use fractional coordinates.
C
      IF (ICMP.NE.0) CALL SET (0.,1.,0.,1.,0.,1.,0.,1.,1)
C
C Do a single frame showing all the characters in the fontcap databases,
C access to which was added in June of 1990.
C
C Double the line width.
C
      CALL PLOTIF (0.,0.,2)
      CALL GSLWSC (2.)
C
C Put a label at the top of the plot.
C
      CALL PLCHHQ (.5,.98,'PLCHHQ - FONTCAP DATABASES ADDED 6/90',
     +                                                        .02,0.,0.)
C
C Temporarily use the slash as a function code character.
C
      CALL PCSETC ('FC - FUNCTION CODE CHARACTER','/')
C
C Put an explanatory note on the plot.
C
      CALL PLCHHQ (.5,.945,':F1:c selects the ASCII character "c", as sh
     +own in the first two lines.',.01,0.,0.)
C
      CALL PLCHHQ (.5,.925,':Fn:c (2/F18/K/F0/n/F18/K/F0/20) selects the
     + corresponding character from font n.',.01,0.,0.)
C
C Return to a colon as the function code character.
C
      CALL PCSETC ('FC - FUNCTION CODE CHARACTER',':')
C
C Loop through all the available fonts.
C
      DO 108 IFNT=1,20
C
        YCEN=.945-.045*REAL(IFNT)
C
        WRITE (CHRS,'(I8)') IFNT
        CHRS(1:4)='FONT'
        CALL PCSETI ('FN - FONTCAP NUMBER',7)
        CALL PLCHHQ (.005,YCEN,CHRS,.012,0.,-1.)
C
        CALL PCSETI ('FN - FONTCAP NUMBER',IFNT)
C
C Draw all the meaningful characters from the font.
C
        DO 107 ICHR=33,126
          IF (ICHR.LE.79) THEN
            XCEN=.125+.0183*REAL(ICHR-32)
          ELSE
            XCEN=.125+.0183*REAL(ICHR-79)
          END IF
          IF (ICHR.EQ.80) YCEN=YCEN-.0225
          IF (CHAR(ICHR).EQ.':') CALL PCSETC ('FC','!')
          CALL PLCHHQ (XCEN,YCEN,CHAR(ICHR),.01,0.,0.)
          IF (CHAR(ICHR).EQ.':') CALL PCSETC ('FC',':')
  107   CONTINUE
C
C End of loop through fonts.
C
  108 CONTINUE
C
C Restore the fontcap number to 0 to select the PWRITX database.
C
      CALL PCSETI ('FN - FONTCAP NUMBER',0)
C
C Go back to normal line width.
C
      CALL PLOTIF (0.,0.,2)
      CALL GSLWSC (1.)
C
C Advance the frame.
C
      CALL FRAME
C
C Do a single frame showing all the characters in the fontcap databases,
C access to which was added in October of 1992.
C
C Put a label at the top of the plot.
C
      CALL PLCHHQ (.5,.98,'PLCHHQ - FONTCAP DATABASES ADDED 10/92',
     +                                                        .02,0.,0.)
C
C Temporarily use the slash as a function code character.
C
      CALL PCSETC ('FC - FUNCTION CODE CHARACTER','/')
C
C Put an explanatory note on the plot.
C
      CALL PLCHHQ (.5,.945,':F21:c selects the ASCII character "c", as s
     +hown in the first two lines.',.01,0.,0.)
C
      CALL PLCHHQ (.5,.925,':Fn:c selects the corresponding character fr
     +om font n.',.01,0.,0.)
C
C Return to a colon as the function code character.
C
      CALL PCSETC ('FC - FUNCTION CODE CHARACTER',':')
C
C Loop through all the new filled fonts.
C
      DO 113 IFNS=1,22
C
        YCEN=.945-.0391304*REAL(IFNS)
C
        WRITE (CHRS,'(I8)') IFFN(IFNS)
        CHRS(1:4)='FONT'
        CALL PCSETI ('FN - FONTCAP NUMBER',7)
        CALL PLCHHQ (.005,YCEN,CHRS,.012,0.,-1.)
C
        CALL PCSETI ('FN - FONTCAP NUMBER',IFFN(IFNS))
C
C Draw all the meaningful characters from the font.
C
        DO 112 ICHR=33,126
          IF (ICHR.LE.79) THEN
            XCEN=.125+.0183*REAL(ICHR-32)
          ELSE
            XCEN=.125+.0183*REAL(ICHR-79)
          END IF
          IF (ICHR.EQ.80) YCEN=YCEN-.0195652
          IF (CHAR(ICHR).EQ.':') CALL PCSETC ('FC','!')
          CALL PLCHHQ (XCEN,YCEN,CHAR(ICHR),.01,0.,0.)
          IF (CHAR(ICHR).EQ.':') CALL PCSETC ('FC',':')
  112   CONTINUE
C
C End of loop through fonts.
C
  113 CONTINUE
C
C Restore the fontcap number to 0 to select the PWRITX database.
C
      CALL PCSETI ('FN - FONTCAP NUMBER',0)
C
C Advance the frame.
C
      CALL FRAME
C
C Deactivate and close workstation, close GKS.
C
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS
C
C Done.
C
      STOP
C
      END

