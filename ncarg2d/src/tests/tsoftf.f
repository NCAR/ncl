
      PROGRAM TSOFTF
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
      CALL SOFTF(IERR,IWKID)
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
      SUBROUTINE SOFTF (IERR,IWKID)
C
C LATEST REVISION       July, 1989
C
C PURPOSE               To provide a simple demonstration of software
C                       fill of polygons using the SOFTFILL package.
C
C USAGE                 CALL SOFTF (IERR,IWKID)
C
C ARGUMENTS (OUTPUT)    IERR
C
C                         An integer variable
C                         = 0, if the test was successful
C                         = 1, otherwise
C
C I/O                   If the test is successful, the message "SOFTFILL
C                       TEST EXECUTED--SEE PLOT TO CERTIFY" is printed
C                       on unit 6.  In addition, a frame is produced on
C                       the graphics device.  In order to determine if
C                       the test was successful, it is necessary to
C                       examine this frame.
C
C PRECISION             Single.
C
C LANGUAGE              FORTRAN 77.
C
C REQUIRED ROUTINES     SOFTFILL package.
C
C REQUIRED GKS LEVEL    0A.
C
C ALGORITHM             Nine circles are filled in various ways, using
C                       routines in the package.
C
C
C Declare required dimensioned arrays.
C
        DIMENSION XRA(106),YRA(106),DST(126),IND(146),XSV(101),YSV(101)
        DIMENSION ID1(8,8),ID2(8,8),ID3(8,8),ID4(8,8)
C
C Define four different dot patterns.
C
        DATA ID1 / 1,1,0,0,0,0,1,1,
     +             1,1,0,1,1,0,1,1,
     +             0,0,0,1,1,0,0,0,
     +             0,1,1,1,1,1,1,0,
     +             0,1,1,1,1,1,1,0,
     +             0,0,0,1,1,0,0,0,
     +             1,1,0,1,1,0,1,1,
     +             1,1,0,0,0,0,1,1/
        DATA ID2 / 0,0,0,0,0,0,0,0,
     +             0,1,1,1,1,1,1,0,
     +             0,1,1,1,1,1,1,0,
     +             0,1,1,0,0,1,1,0,
     +             0,1,1,0,0,1,1,0,
     +             0,1,1,1,1,1,1,0,
     +             0,1,1,1,1,1,1,0,
     +             0,0,0,0,0,0,0,0/
        DATA ID3 / 0,0,0,0,0,0,0,0,
     +             0,0,0,0,1,0,0,0,
     +             0,0,0,1,1,1,0,0,
     +             0,1,0,0,1,0,0,1,
     +             0,0,1,1,1,1,1,0,
     +             0,0,0,0,1,0,0,0,
     +             0,0,0,1,0,1,0,0,
     +             0,1,1,0,0,0,1,1/
        DATA ID4 / 0,0,0,0,0,0,0,0,
     +             0,1,1,0,0,1,1,1,
     +             0,1,1,0,0,1,1,0,
     +             0,1,1,0,1,1,0,0,
     +             0,1,1,1,1,0,0,0,
     +             0,1,1,0,1,1,0,0,
     +             0,1,1,0,0,1,1,0,
     +             0,1,1,0,0,1,1,1/
C
C Initialize the error parameter.
C
        IERR=0
C
C Double the size of the GKS dot.
C
        CALL GSMKSC (2.)
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
        CALL SFCLRS(IWKID)
C
C Do a set call allowing us to use X and Y coordinates in the range
C from 0 to 4.
C
        CALL SET (0.,1.,0.,1.,0.,4.,0.,4.,1)
C
C Put a label at the top of the frame.
C
        CALL PLCHLQ(2.,3.75,'DEMONSTRATION PLOT FOR SOFTFILL',16.,0.,0.)
C
C Return the basic internal parameters to their default values.
C
        CALL SFSETI ('AN',0)
        CALL SFSETI ('CH',0)
        CALL SFSETI ('DO',0)
        CALL SFSETR ('SP',.00125)
        CALL SFSETI ('TY',0)
C
C The following code creates a single frame showing nine circles filled
C in various ways.  The DO-loop variable I says which row of circles
C we're working on (1 => top, 2 => middle, 3 => bottom).  The DO-loop
C variable J says which column of circles we're working C on (1 =>
C left, 2 => center, 3 => right).  The variable K gives the number of
C the circle currently being drawn and is used in a computed GO TO to
C determine which block of code is executed.
C
        DO 112 I=1,3
C
          YCN=REAL(4-I)
C
          DO 111 J=1,3
C
            XCN=REAL(J)
C
            K=3*(I-1)+J
C
C Generate the coordinates defining the circle.  Two sets of arrays are
C used, one set for use in calling the fill routines, which transform
C the contents of the arrays, and one set for use in drawing the circle.
C
            DO 100 L=1,101
              XRA(L)=XCN+.48*SIN(.062831853071796*REAL(L))
              XSV(L)=XRA(L)
              YRA(L)=YCN+.48*COS(.062831853071796*REAL(L))
              YSV(L)=YRA(L)
  100       CONTINUE
C
C Jump to the proper piece of code to fill the circle.
C
            GO TO (101,102,103,104,105,106,107,108,109) , K
C
C Fill the first circle in color number 5.
C
  101       CALL SFSETI ('TYPE OF FILL',0)
            CALL SFSGFA (XRA,YRA,100,DST,126,IND,146,5)
            GO TO 110
C
C Add a diamond-shaped hole to the circle and fill it in color number
C 9, using lines in two directions to effect the fill.
C
  102       XRA(101)=2.00
            YRA(101)=3.24
            XRA(102)=1.76
            YRA(102)=3.00
            XRA(103)=2.00
            YRA(103)=2.76
            XRA(104)=2.24
            YRA(104)=3.00
            XRA(105)=XRA(101)
            YRA(105)=YRA(101)
            XRA(106)=XRA(100)
            YRA(106)=YRA(100)
            CALL SFSETI ('TYPE OF FILL',2)
            CALL SFSGFA (XRA,YRA,106,DST,126,IND,146,9)
            GO TO 110
C
C Create a more complicated hole in the third circle and fill it with
C pattern number 11 of the 20 or so that can be created using 'TY'=-4.
C
  103       XRA(101)=XRA( 40)
            YRA(101)=YRA( 40)
            XRA(102)=XRA( 80)
            YRA(102)=YRA( 80)
            XRA(103)=XRA( 20)
            YRA(103)=YRA( 20)
            XRA(104)=XRA( 60)
            YRA(104)=YRA( 60)
            XRA(105)=XRA(100)
            YRA(105)=YRA(100)
            CALL SFSETI ('TYPE OF FILL',-4)
            CALL SFSGFA (XRA,YRA,105,DST,126,IND,146,11)
            CALL SFSETI ('ANGLE OF FILL LINES',15)
            GO TO 110
C
C Fill the fourth circle with the default dot pattern, increasing the
C inter-dot spacing considerably.
C
  104       CALL SFSETR ('SPACING OF FILL LINES',.005)
            CALL SFSETI ('ANGLE OF FILL LINES',0)
            CALL SFSETI ('DOT-FILL FLAG',1)
            CALL SFWRLD (XRA,YRA,100,DST,126,IND,146)
            GO TO 110
C
C Fill the fifth circle with a combination of lines and dots.
C
  105       CALL SFSETR ('SPACING OF FILL LINES',.012)
            CALL SFSETI ('DOT-FILL FLAG',0)
            CALL SFWRLD (XRA,YRA,100,DST,126,IND,146)
            CALL SFSETR ('SPACING OF FILL LINES',.006)
            CALL SFSETI ('DOT-FILL FLAG',1)
            CALL SFNORM (XRA,YRA,100,DST,126,IND,146)
            GO TO 110
C
C Fill the sixth circle with a specified dot pattern.
C
  106       CALL SFSETR ('SPACING OF FILL LINES',.004)
            CALL SFSETP (ID1)
            CALL SFWRLD (XRA,YRA,100,DST,126,IND,146)
            GO TO 110
C
C Fill the seventh circle with a different dot pattern, tilted at an
C angle.
C
  107       CALL SFSETI ('ANGLE OF FILL LINES',45)
            CALL SFSETP (ID2)
            CALL SFWRLD (XRA,YRA,100,DST,126,IND,146)
            GO TO 110
C
C Fill the eighth circle with a different dot pattern, using characters.
C
  108       CALL GSCHH  (.004)
            CALL SFSETR ('SPACING OF FILL LINES',.006)
            CALL SFSETI ('ANGLE OF FILL LINES',0)
            CALL SFSETC ('CHARACTER SPECIFIER','O')
            CALL SFSETP (ID3)
            CALL SFWRLD (XRA,YRA,100,DST,126,IND,146)
            GO TO 110
C
C Fill the last circle with K's, both large and small.
C
  109       CALL GSCHH  (.008)
            CALL SFSETR ('SPACING OF FILL LINES',.012)
            CALL SFSETC ('CHARACTER SPECIFIER','K')
            CALL SFSETP (ID4)
            CALL SFWRLD (XRA,YRA,100,DST,126,IND,146)
            GO TO 110
C
C Draw the circle.
C
  110       CALL CURVE (XSV,YSV,101)
C
  111     CONTINUE
C
  112   CONTINUE
C
C Advance the frame.
C
        CALL FRAME
C
C Log execution message and return to caller.
C
        WRITE (6,1001)
        RETURN
C
 1001   FORMAT (' SOFTFILL TEST EXECUTED--SEE PLOT TO CERTIFY')
C
      END


      SUBROUTINE SFCLRS(IWKID)
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
