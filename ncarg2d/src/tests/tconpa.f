
      PROGRAM TCONPA
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
      CALL CONPA(IERR,IWKID)
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
      SUBROUTINE CONPA (IERR,IWKID)
C
C PURPOSE               To provide a simple demonstration of the use of
C                       CONPACK to contour regularly-spaced rectangular
C                       data.
C
C USAGE                 CALL CONPA (IERR)
C
C ARGUMENTS (OUTPUT)    IERR
C
C                         An integer variable
C                         = 0, if the test was successful,
C                         = 1, otherwise
C
C I/O                   If the test is successful, the message "CONPACK
C                       TEST EXECUTED--SEE PLOTS TO CERTIFY" is printed
C                       on unit 6.  In addition, three frames are drawn
C                       on the graphics device.  In order to determine
C                       if the test was successful, it is necessary to
C                       examine these frames.
C
C PRECISION             Single.
C
C LANGUAGE              FORTRAN 77.
C
C REQUIRED ROUTINES     The AREAS, CONPACK, LABELBAR, SOFTFILL, and
C                       SPPS packages.
C
C REQUIRED GKS LEVEL    0A.
C
C ALGORITHM             The function
C
C                         Z(X,Y) = X + Y + 1./((X-.1)**2+Y**2+.09)
C                                         -1./((X+.1)**2+Y**2+.09)
C
C                       for X = -1. to 1. in increments of .1, and Y =
C                       -1.2 to 1.2 in increments of .1, is computed.
C                       Then, entries CPEZCT and CPCNRC are called to
C                       generate contour plots of Z.
C
C ZDAT contains the values to be plotted.
C
        DIMENSION  ZDAT(21,25)
C
C Declare an array to hold the GKS "aspect source flags".
C
        DIMENSION IASF(13)
C
C Define real and integer work spaces and an area-map array.
C
        DIMENSION RWRK(1000),IWRK(1000),IAMA(20000)
C
C Define arrays for use by ARSCAM.
C
        DIMENSION XCRA(1000),YCRA(1000),IAIA(10),IGIA(10)
C
C Declare arrays to hold the list of indices and the list of labels
C required by the label-bar routine.
C
        DIMENSION LIND(10)
        CHARACTER*2 LLBS(9)
C
C Declare the routine which will color the areas.
C
        EXTERNAL CPCOLR
C
C Initialize the values in the aspect-source-flag array.
C
        DATA IASF / 13*1 /
C
C Define the list of indices and the list of labels required by the
C label-bar routine.
C
        DATA LIND / 6,7,8,9,10,11,12,13,14,15 /
C
        DATA LLBS / '-4','-3','-2','-1',' 0',' 1',' 2',' 3',' 4' /
C
C Initialize the error parameter.
C
        IERR = 0
C
C Fill the 2D array to be plotted.
C
        DO  102 I=1,21
          X=.1*REAL(I-11)
          DO 101 J=1,25
            Y = .1*REAL(J-13)
            ZDAT(I,J)=X+Y+1./((X-.10)**2+Y**2+.09)
     +                   -1./((X+.10)**2+Y**2+.09)
  101     CONTINUE
  102   CONTINUE
C
C Frame 1 -- CPEZCT.
C
C The routine CPEZCT requires only the array name and dimensions.
C
      CALL SET (0.,1.,0.,1.,0.,1.,0.,1.,1)
      CALL WTSTR (.5,.9765,
     +            'DEMONSTRATION PLOT FOR CONPACK ROUTINE CPEZCT',2,0,0)
C
      CALL CPEZCT (ZDAT,21,25)
C
C Frame 2 -- CPCNRC.
C
C The routine CPCNRC is called just like the old routine CONREC.
C
C In this example, the lowest contour level (-4.5), the highest contour
C level (4.5), and the increment between contour levels (0.3) are set.
C Line labels are positioned using the penalty scheme and the smoother
C is turned on.
C
      CALL SET (0.,1.,0.,1.,0.,1.,0.,1.,1)
      CALL WTSTR (.5,.9765,
     +            'DEMONSTRATION PLOT FOR CONPACK ROUTINE CPCNRC',2,0,0)
C
      CALL CPSETI ('LLP - LINE LABEL POSITIONING',3)
      CALL CPSETR ('T2D - TENSION ON 2D SPLINES',3.6)
      CALL CPCNRC (ZDAT,21,21,25,-4.5,4.5,.3,0,0,0)
      CALL FRAME
C
C Frame 3 - A solid-filled contour plot.
C
C Set all the GKS aspect source flags to "individual".
C
        CALL GSASF (IASF)
C
C Force solid fill.
C
        CALL GSFAIS (1)
C
C Turn off the clipping indicator.
C
        CALL GSCLIP (0)
C
C Define a bunch of color indices.
C
        CALL CPCLRS(IWKID)
C
C Put a label at the top of the frame.
C
      CALL SET (0.,1.,0.,1.,0.,1.,0.,1.,1)
      CALL WTSTR (.5,.9765,
     +            'DEMONSTRATION PLOT FOR BASIC CONPACK ROUTINES',2,0,0)
C
C Force the plot into the left side of the frame.
C
        CALL CPSETR ('VPR - VIEWPORT RIGHT',.75)
C
C Force the use of exactly 9 contour levels, specify those levels, and
C define exactly what is to be done at each level.
C
        CALL CPSETI ('CLS - CONTOUR LEVEL SELECTOR',0)
        CALL CPSETI ('NCL - NUMBER OF CONTOUR LEVELS',9)
C
        DO 103 I=1,9
          CALL CPSETI ('PAI - PARAMETER ARRAY INDEX',I)
          CALL CPSETR ('CLV - CONTOUR LEVEL',REAL(I-5))
          CALL CPSETI ('CLU - CONTOUR LEVEL USE',1)
          CALL CPSETI ('AIB - AREA IDENTIFIER BELOW LEVEL',I)
          CALL CPSETI ('AIA - AREA IDENTIFIER ABOVE LEVEL',I+1)
  103   CONTINUE
C
C Initialize the drawing of the contour plot.
C
        CALL CPRECT (ZDAT,21,21,25,RWRK,1000,IWRK,1000)
C
C Initialize the area map and put the contour lines into it.
C
        CALL ARINAM (IAMA,20000)
        CALL CPCLAM (ZDAT,RWRK,IWRK,IAMA)
C
C Color the map.
C
        CALL ARSCAM (IAMA,XCRA,YCRA,1000,IAIA,IGIA,10,CPCOLR)
C
C Put black contour lines over the colored map.
C
        CALL GSPLCI (0)
        CALL CPCLDR (ZDAT,RWRK,IWRK)
        CALL GSPLCI (1)
C
C Draw a color bar for the plot.
C
        CALL LBSETI ('CBL - COLOR OF BOX LINES',2)
        CALL PCSETR ('CS - CONSTANT SPACING FLAG',1.25)
        CALL LBLBAR (1,.80,.95,.05,.95,10,.5,1.,LIND,0,LLBS,9,1)
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
 1001 FORMAT (' CONPACK TEST EXECUTED--SEE PLOTS TO CERTIFY')
C
      END
      SUBROUTINE CPCLRS(IWKID)
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
      SUBROUTINE CPCOLR (XCRA,YCRA,NCRA,IAIA,IGIA,NAIA)
C
        DIMENSION XCRA(*),YCRA(*),IAIA(*),IGIA(*)
C
C The arrays XCRA and YCRA, for indices 1 to NCRA, contain the X and Y
C coordinates of points defining a polygon.  The area identifiers in
C the array IAIA, each with an associated group identifier in the array
C IGIA, tell us whether the polygon is to be color-filled or not.
C
C
C Assume the polygon will be filled until we find otherwise.
C
        IFLL=1
C
C If any of the area identifiers is negative, don't fill the polygon.
C
        DO 101 I=1,NAIA
          IF (IAIA(I).LT.0) IFLL=0
  101   CONTINUE
C
C Otherwise, fill the polygon in the color implied by its area
C identifier relative to edge group 3 (the contour-line group).
C
        IF (IFLL.NE.0) THEN
          IFLL=0
          DO 102 I=1,NAIA
            IF (IGIA(I).EQ.3) IFLL=IAIA(I)
  102     CONTINUE
          IF (IFLL.GE.1.AND.IFLL.LE.10) THEN
            CALL GSFACI (IFLL+5)
            CALL GFA (NCRA-1,XCRA,YCRA)
          END IF
        END IF
C
C Done.
C
        RETURN
C
      END
