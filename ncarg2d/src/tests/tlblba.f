
      PROGRAM TLBLBA
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
C OPEN GKS, OPEN WORKSTATION OF TYPE 1, ACTIVATE WORKSTATION
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
C
C INVOKE DEMO DRIVER
C
      CALL LBLBA(IERR,IWKID)
C
C DEACTIVATE AND CLOSE WORKSTATION, CLOSE GKS.
C
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS
C
      STOP
      END
C
      SUBROUTINE LBLBA (IERR,IWKID)
C
C LATEST REVISION       July, 1989
C
C PURPOSE               To provide a simple demonstration of the use of
C                       LABELBAR to draw various kinds of labelled bars.
C
C USAGE                 CALL LBLBA (IERR,IWKID)
C
C ARGUMENTS (OUTPUT)    IERR
C
C                         An integer variable
C                         = 0, if the test was successful,
C                         = 1, otherwise
C
C I/O                   If the test is successful, the message "LABELBAR
C                       TEST EXECUTED--SEE PLOTS TO CERTIFY" is printed
C                       on unit 6.  In addition, a single frame is drawn
C                       on the graphics device.  In order to determine
C                       if the test was successful, it is necessary to
C                       examine this frame.
C
C PRECISION             Single.
C
C LANGUAGE              FORTRAN 77.
C
C REQUIRED ROUTINES     LABELBAR, SOFTFILL, and PLOTCHAR packages.
C
C REQUIRED GKS LEVEL    0A.
C
C ALGORITHM             Three simple label bars are drawn.
C
C Declare an array to hold the GKS "aspect source flags".
C
        DIMENSION IASF(13)
C
C Declare the arrays to hold the list of indices and the lists of labels
C required by the label-bar routine.
C
        DIMENSION LND1(20),LND2(16),LND3(16)
C
        CHARACTER*12 LLB1(20)
        CHARACTER*10 LLB2(17)
        CHARACTER*1  LLB3(16)
C
C Initialize the values in the aspect-source-flag array.
C
        DATA IASF / 13*1 /
C
C Define the list of indices required by the label-bar routine.
C
        DATA LND1 / 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20 /
        DATA LND2 / 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15 /
        DATA LND3 / 12,13,14,15,8,9,10,11,4,5,6,7,0,1,2,3 /
C
C Define labels for various bars.
C
        DATA LLB1 /   '0 to 5:H2Q', '5 to 10:H1Q','10 to 15:H1Q',
     +              '15 to 20:H1Q','20 to 25:H1Q','25 to 30:H1Q',
     +              '30 to 35:H1Q','35 to 40:H1Q','40 to 45:H1Q',
     +              '45 to 50:H1Q','50 to 55:H1Q','55 to 60:H1Q',
     +              '60 to 65:H1Q','65 to 70:H1Q','70 to 75:H1Q',
     +              '75 to 80:H1Q','80 to 85:H1Q','85 to 90:H1Q',
     +              '90 to 95:H1Q','95 to 100'   /
C
        DATA LLB2 / '-2000 feet',' Sea level',' 2000 feet',
     +              ' 4000 feet',' 6000 feet',' 8000 feet',
     +              '10000 feet','12000 feet','14000 feet',
     +              '16000 feet','18000 feet','20000 feet',
     +              '22000 feet','24000 feet','26000 feet',
     +              '28000 feet','30000 feet'/
C
        DATA LLB3 / 'M','N','O','P','I','J','K','L','E','F','G','H',
     +              'A','B','C','D'/
C
C Turn off the clipping indicator.
C
        CALL GSCLIP (0)
C
C Set all the GKS aspect source flags to "individual".
C
        CALL GSASF (IASF)
C
C Force solid fill.
C
        CALL GSFAIS (1)
C
C Define color indices.
C
        CALL LBCLRS(IWKID)
C
C Force PLOTCHAR to use constant spacing of characters.
C
        CALL PCSETR ('CS - CONSTANT SPACING',1.25)
C
C Set some parameter values.
C
        CALL LBSETR ('WBL - WIDTH OF BOX LINES',4.)
        CALL LBSETR ('WFL - WIDTH OF FILL LINES',2.)
        CALL LBSETR ('WLB - WIDTH OF LABEL LINES',2.)
C
C Put the first label bar vertically along the left edge of the plotter
C frame.  Use patterns.
C
        CALL SFSETI ('ANGLE OF FILL LINES',15)
        CALL SFSETI ('TYPE OF FILL',-4)
        CALL LBLBAR (1,.05,.30,.05,.95,20,.3333,1.,LND1,0,LLB1,20,2)
C
C Put the second label bar vertically along the right edge.  Use solid
C color fill.
C
        CALL SFSETI ('TYPE OF FILL',0)
        CALL LBLBAR (1,.70,.95,.05,.95,16,.3333,1.,LND2,0,LLB2,17,1)
C
C The remaining label bars are arranged horizontally in such a way as
C to form a rectangular key for color indices 1 through 12.  The
C default version of LBFILL is used.
C
        CALL LBLBAR (0,.35,.65,.05,.20,4,.5,.5,LND3( 1),1,LLB3( 1),4,1)
        CALL LBLBAR (0,.35,.65,.20,.35,4,.5,.5,LND3( 5),1,LLB3( 5),4,1)
        CALL LBLBAR (0,.35,.65,.35,.50,4,.5,.5,LND3( 9),1,LLB3( 9),4,1)
        CALL LBLBAR (0,.35,.65,.50,.65,4,.5,.5,LND3(13),1,LLB3(13),4,1)
C
C Put a title on the plot.  We must first call SET to define the ranges
C of the X and Y coordinates to be used.  The constant spacing feature
C is turned off so that the title will look normal.
C
        CALL SET (0.,1.,0.,1.,0.,1.,0.,1.,1)
        CALL PLCHLQ  (.5,.90,'THREE',24.,0.,0.)
        CALL PLCHLQ  (.5,.85,'LABELBAR',24.,0.,0.)
        CALL PLCHLQ  (.5,.80,'EXAMPLES',24.,0.,0.)
C
C Advance the frame.
C
        CALL FRAME
C
C Log a successful-completion message and return to the caller.
C
        WRITE (6,1001)
C
        RETURN
C
 1001 FORMAT (' LABELBAR TEST EXECUTED--SEE PLOTS TO CERTIFY')
C
      END
      SUBROUTINE LBCLRS(IWKID)
C
C Define a set of RGB color triples for colors 1 through 15.
C
        DIMENSION RGBV(3,15)
C
C Define the RGB color triples needed below.
C
        DATA RGBV / 1.00 , 1.00 , 1.00 ,
     +              0.70 , 0.70 , 0.70 ,
     +              0.75 , 0.50 , 1.00 ,
     +              0.50 , 0.00 , 1.00 ,
     +              0.00 , 0.00 , 1.00 ,
     +              0.00 , 0.50 , 1.00 ,
     +              0.00 , 1.00 , 1.00 ,
     +              0.00 , 1.00 , 0.60 ,
     +              0.00 , 1.00 , 0.00 ,
     +              0.70 , 1.00 , 0.00 ,
     +              1.00 , 1.00 , 0.00 ,
     +              1.00 , 0.75 , 0.00 ,
     +              1.00 , 0.38 , 0.38 ,
     +              1.00 , 0.00 , 0.38 ,
     +              1.00 , 0.00 , 0.00 /
C
C Define 16 different color indices, for indices 0 through 15.  The
C color corresponding to index 0 is black and the color corresponding
C to index 1 is white.
C
        CALL GSCR (IWKID,0,0.,0.,0.)
C
        DO 101 I=1,15
          CALL GSCR (IWKID,I,RGBV(1,I),RGBV(2,I),RGBV(3,I))
  101   CONTINUE
C
C Done.
C
        RETURN
C
      END
